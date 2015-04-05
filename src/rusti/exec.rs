// Copyright 2014 Murarth
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Rust code parsing and compilation.

use std::ffi::{AsOsStr, CStr, CString};
use std::io;
use std::mem::transmute;
use std::path::PathBuf;
use std::process::Command;
use std::str::from_utf8;
use std::sync::mpsc::channel;
use std::thread::Builder;

use rustc;
use rustc_lint;

use rustc::llvm;
use rustc::metadata::cstore::RequireDynamic;
use rustc::middle::ty;
use rustc::session::config::{self, basic_options, build_configuration, Options};
use rustc::session::config::{Input, UnstableFeatures};
use rustc::session::build_session;
use rustc_driver::driver;
use rustc_resolve::MakeGlobMap;

use syntax::ast_map;
use syntax::diagnostics::registry::Registry;

// This seems like a such a simple solution that I'm surprised it works.
#[link(name = "morestack")]
extern "C" {
    fn __morestack();
}

fn morestack_addr() -> *const () {
    unsafe { transmute(__morestack) }
}

/// Compiles input code into an execution environment.
pub struct ExecutionEngine {
    ee: llvm::ExecutionEngineRef,
    modules: Vec<llvm::ModuleRef>,
    /// Additional search paths for libraries
    lib_paths: Vec<String>,
    sysroot: PathBuf,
}

/// A value that can be translated into `ExecutionEngine` input
pub trait IntoInput {
    fn into_input(self) -> Input;
}

impl<'a> IntoInput for &'a str {
    fn into_input(self) -> Input {
        Input::Str(self.to_string())
    }
}

impl IntoInput for String {
    fn into_input(self) -> Input {
        Input::Str(self)
    }
}

impl IntoInput for PathBuf {
    fn into_input(self) -> Input {
        Input::File(self)
    }
}

type Deps = Vec<PathBuf>;

impl ExecutionEngine {
    /// Constructs a new `ExecutionEngine` with the given library search paths.
    pub fn new(libs: Vec<String>, sysroot: Option<PathBuf>) -> ExecutionEngine {
        ExecutionEngine::new_with_input(String::new(), libs, sysroot)
    }

    /// Constructs a new `ExecutionEngine` with the given starting input
    /// and library search paths.
    pub fn new_with_input<T>(input: T, libs: Vec<String>, sysroot: Option<PathBuf>) -> ExecutionEngine
            where T: IntoInput {
        let sysroot = sysroot.unwrap_or_else(get_sysroot);

        let (llmod, deps) = compile_input(input.into_input(),
            sysroot.clone(), libs.clone())
            .expect("ExecutionEngine init input failed to compile");

        let morestack = morestack_addr();

        assert!(!morestack.is_null());

        let mm = unsafe { llvm::LLVMRustCreateJITMemoryManager(morestack) };

        assert!(!mm.is_null());

        let ee = unsafe { llvm::LLVMBuildExecutionEngine(llmod, mm) };

        if ee.is_null() {
            panic!("Failed to create ExecutionEngine: {}", llvm_error());
        }

        let ee = ExecutionEngine{
            ee: ee,
            modules: vec![llmod],
            lib_paths: libs,
            sysroot: sysroot,
        };

        ee.load_deps(&deps);

        ee
    }

    /// Compile a module and add it to the execution engine.
    /// If the module fails to compile, errors will be printed to `stderr`
    /// and `None` will be returned. Otherwise, the module is returned.
    pub fn add_module<T>(&mut self, input: T) -> Option<llvm::ModuleRef>
            where T: IntoInput {
        debug!("compiling module");

        let (llmod, deps) = match compile_input(input.into_input(),
                self.sysroot.clone(), self.lib_paths.clone()) {
            Some(r) => r,
            None => return None,
        };

        self.load_deps(&deps);

        self.modules.push(llmod);

        unsafe { llvm::LLVMExecutionEngineAddModule(self.ee, llmod); }

        Some(llmod)
    }

    /// Remove the given module from the execution engine.
    /// The module is destroyed after it is removed.
    ///
    /// # Panics
    ///
    /// If the Module does not exist within this `ExecutionEngine`.
    pub fn remove_module(&mut self, llmod: llvm::ModuleRef) {
        match self.modules.iter().position(|p| *p == llmod) {
            Some(i) => {
                self.modules.remove(i);
                let res = unsafe {
                    llvm::LLVMExecutionEngineRemoveModule(self.ee, llmod)
                };

                assert_eq!(res, 1);

                unsafe { llvm::LLVMDisposeModule(llmod) };
            },
            None => panic!("Module not contained in ExecutionEngine"),
        }
    }

    /// Compiles the given input only up to the analysis phase, calling the
    /// given closure with a borrowed reference to the analysis result.
    pub fn with_analysis<F, R, T>(&self, input: T, f: F) -> Option<R>
            where F: Send + 'static, R: Send + 'static, T: IntoInput,
            F: for<'tcx> FnOnce(&ty::CrateAnalysis<'tcx>) -> R {
        with_analysis(f, input.into_input(),
            self.sysroot.clone(), self.lib_paths.clone())
    }

    /// Searches for the named function in the set of loaded modules,
    /// beginning with the most recently added module.
    /// If the function is found, a raw pointer is returned.
    /// If the function is not found, `None` is returned.
    pub fn get_function(&mut self, name: &str) -> Option<*const ()> {
        let s = CString::new(name.as_bytes()).unwrap();

        for m in self.modules.iter().rev() {
            let fv = unsafe { llvm::LLVMGetNamedFunction(*m, s.as_ptr()) };

            if !fv.is_null() {
                let fp = unsafe { llvm::LLVMGetPointerToGlobal(self.ee, fv) };

                assert!(!fp.is_null());

                return Some(fp);
            }
        }

        None
    }

    /// Searches for the named global in the set of loaded modules,
    /// beginning with the most recently added module.
    /// If the global is found, a raw pointer is returned.
    /// If the global is not found, `None` is returned.
    pub fn get_global(&mut self, name: &str) -> Option<*const ()> {
        let s = CString::new(name.as_bytes()).unwrap();

        for m in self.modules.iter().rev() {
            let gv = unsafe { llvm::LLVMGetNamedGlobal(*m, s.as_ptr()) };

            if !gv.is_null() {
                let gp = unsafe { llvm::LLVMGetPointerToGlobal(self.ee, gv) };

                assert!(!gp.is_null());

                return Some(gp);
            }
        }

        None
    }

    /// Loads all dependencies of compiled code.
    /// Expects a series of paths to dynamic library files.
    fn load_deps(&self, deps: &Deps) {
        for path in deps.iter() {
            debug!("loading crate {}", path.display());

            let s = match path.as_os_str().to_str() {
                Some(s) => s,
                None => panic!(
                    "Could not convert crate path to UTF-8 string: {:?}", path)
            };
            let cs = CString::new(s).unwrap();

            let res = unsafe { llvm::LLVMRustLoadDynamicLibrary(cs.as_ptr()) };

            if res == 0 {
                panic!("Failed to load crate {:?}: {}",
                    path.display(), llvm_error());
            }
        }
    }
}

impl Drop for ExecutionEngine {
    fn drop(&mut self) {
        unsafe { llvm::LLVMDisposeExecutionEngine(self.ee) };
    }
}

/// Returns last error from LLVM wrapper code.
fn llvm_error() -> String {
    String::from_utf8_lossy(
        unsafe { CStr::from_ptr(llvm::LLVMRustGetLastError()).to_bytes() })
        .into_owned()
}

/// Runs `rustc` to ask for its sysroot path.
fn get_sysroot() -> PathBuf {
    let rustc = if cfg!(windows) { "rustc.exe" } else { "rustc" };

    let output = match Command::new(rustc).args(&["--print", "sysroot"]).output() {
        Ok(output) => output.stdout,
        Err(e) => panic!("failed to run rustc: {}", e),
    };

    let path = from_utf8(&output)
        .ok().expect("sysroot is not valid UTF-8").trim_right_matches(
            |c| c == '\r' || c == '\n');

    debug!("using sysroot: {:?}", path);

    PathBuf::from(path)
}

fn build_exec_options(sysroot: PathBuf, libs: Vec<String>) -> Options {
    let mut opts = basic_options();

    // librustc derives sysroot from the executable name.
    // Since we are not rustc, we must specify it.
    opts.maybe_sysroot = Some(sysroot);

    for p in libs.iter() {
        opts.search_paths.add_path(&p);
    }

    // Prefer faster build times
    opts.optimize = config::No;

    // Don't require a `main` function
    opts.crate_types = vec![config::CrateTypeDylib];

    // Allow use of unstable features
    opts.unstable_features = UnstableFeatures::Default;

    opts
}

/// Compiles input up to phase 4, translation to LLVM.
///
/// Returns the LLVM `ModuleRef` and a series of paths to dynamic libraries
/// for crates used in the given input.
fn compile_input(input: Input, sysroot: PathBuf, libs: Vec<String>)
        -> Option<(llvm::ModuleRef, Deps)> {
    let task = Builder::new().name("compile_input".to_string());

    let (tx, rx) = channel();

    let handle = task.spawn(move || {
        if !log_enabled!(::log::LogLevel::Error) {
            io::set_panic(Box::new(io::sink()));
        }
        let opts = build_exec_options(sysroot, libs);
        let sess = build_session(opts, None, Registry::new(&rustc::diagnostics::DIAGNOSTICS));
        rustc_lint::register_builtins(&mut sess.lint_store.borrow_mut(), Some(&sess));

        let cfg = build_configuration(&sess);

        let id = "repl".to_string();

        let krate = driver::phase_1_parse_input(&sess, cfg, &input);

        let krate = driver::phase_2_configure_and_expand(&sess, krate,
            &id, None).expect("phase_2 returned `None`");

        let mut forest = ast_map::Forest::new(krate);
        let arenas = ty::CtxtArenas::new();
        let ast_map = driver::assign_node_ids_and_map(&sess, &mut forest);

        let analysis = driver::phase_3_run_analysis_passes(
            sess, ast_map, &arenas, id, MakeGlobMap::No);

        let (tcx, trans) = driver::phase_4_translate_to_llvm(analysis);

        let crates = tcx.sess.cstore.get_used_crates(RequireDynamic);

        // Collect crates used in the session.
        // Reverse order finds dependencies first.
        let deps = crates.into_iter().rev()
            .filter_map(|(_, p)| p).collect();

        assert_eq!(trans.modules.len(), 1);
        let llmod = trans.modules[0].llmod;

        // Workaround because raw pointers do not impl Send
        let modp = llmod as usize;

        tx.send((modp, deps)).unwrap();
    }).unwrap();

    if let Err(_) = handle.join() {
        return None;
    }

    let (llmod, deps) = rx.recv().unwrap();
    Some((llmod as llvm::ModuleRef, deps))
}

/// Compiles input up to phase 3, type/region check analysis, and calls
/// the given closure with the resulting `CrateAnalysis`.
fn with_analysis<F, R>(f: F, input: Input, sysroot: PathBuf, libs: Vec<String>) -> Option<R>
        where F: Send + 'static, R: Send + 'static,
        F: for<'tcx> FnOnce(&ty::CrateAnalysis<'tcx>) -> R {
    let task = Builder::new().name("with_analysis".to_string());
    let (tx, rx) = channel();

    let handle = task.spawn(move || {
        if !log_enabled!(::log::LogLevel::Error) {
            io::set_panic(Box::new(io::sink()));
        }
        let opts = build_exec_options(sysroot, libs);
        let sess = build_session(opts, None, Registry::new(&rustc::diagnostics::DIAGNOSTICS));
        rustc_lint::register_builtins(&mut sess.lint_store.borrow_mut(), Some(&sess));

        let cfg = build_configuration(&sess);

        let id = "repl".to_string();

        let krate = driver::phase_1_parse_input(&sess, cfg, &input);

        let krate = driver::phase_2_configure_and_expand(&sess, krate,
            &id, None).expect("phase_2 returned `None`");

        let mut forest = ast_map::Forest::new(krate);
        let arenas = ty::CtxtArenas::new();
        let ast_map = driver::assign_node_ids_and_map(&sess, &mut forest);

        let analysis = driver::phase_3_run_analysis_passes(
            sess, ast_map, &arenas, id, MakeGlobMap::No);

        tx.send(f(&analysis)).unwrap();
    }).unwrap();

    let _ = handle.join();
    rx.recv().ok()
}
