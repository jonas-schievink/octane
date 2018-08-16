//! Xbox kernel implementation and HLE code.
//!
//! # Kernel Functions
//!
//! The XBE imports kernel symbols via the "kernel thunk table". These imports
//! are resolved by the loader.
//!
//! In order to perform HLE calls, the CPU implementation has to check when a
//! call to a kernel function occurs. This is done by "resolving" every function
//! import to the raw thunk table entry (`0x80000XXX`) and detecting `call`
//! instructions to those addresses.
//!
//! ## Adding a new kernel function
//!
//! When the program calls an unimplemented kernel function, the emulator
//! crashes. This is by design, as predicable emulator behaviour cannot be
//! guaranteed when such a call would be ignored, for example. The CPU trace
//! output should indicate the name of the called function. In order to
//! implement the missing function, the following steps are necessary:
//!
//! * The function needs to be added to the function table defined in
//!   `kernel/table.rs` (this might already be done).
//! * The function itself needs to be implemented. All kernel functions are
//!   methods of the `Syscall` type and are placed in the module corresponding
//!   the the subsystem they're part of. This step might also require new types
//!   to be defined in `kernel/types.rs`.
//! * The `do_hle_call` function in `kernel/mod.rs` needs to be expanded to
//!   dispatch the function's ID to the implementation you just created.

mod ps;
mod rtl;
mod table;
pub mod types;

use self::table::{KernelExportKind, KernelAbi};
use self::types::FromRawArgs;
use memory::{MemoryError, VirtualMemory};
use cpu::interpret::{Hooks, HookAction, HookError, Interpreter};

/// Host-side kernel data.
///
/// On the Xbox, the equivalent of this state is scattered somewhere in the top
/// 2G of address space. Here, we can also store it in host memory. This ensures
/// that a rogue guest program cannot overwrite important parts of it and
/// consequently cause crashes or exploit the emulator. Note that all kernel
/// variables that can be imported by the guest program still need to be stored
/// in the >2G kernel area of the guest address space to be accessible.
#[derive(Debug)]
pub struct Kernel {

}

impl Kernel {
    /// Creates a new kernel context.
    pub fn new() -> Self {
        Self {}
    }

    /// Perform an HLE call. Called by the interpreter.
    ///
    /// In almost all cases, this returns `Ok`. It only returns `Err` if the
    /// arguments could not be read (due to a memory fault) or if `addr` refers
    /// to a variable instead of a callable function.
    ///
    /// In particular, this will return `Ok` even if the called function
    /// encounters an error. Such errors will be forwarded to the guest program
    /// via the intended means (mostly return codes).
    ///
    /// # Parameters
    ///
    /// * `state`: CPU state. Will be changed by the call (eg. to store the
    ///   return value and pop the arguments).
    /// * `mem`: Xbox virtual memory. May or may not be modified by the call.
    /// * `addr`: Marker address of the kernel function. Must be above
    ///   `0x80000000` (kernel space).
    // TODO document expected `state` (eip and esp point where?)
    pub fn do_hle_call<M: VirtualMemory>(
        &mut self,
        cpu: &mut ::cpu::State,
        mem: &mut M,
        addr: u32,
    ) -> Result<(), ()> {
        // Practically, this "unresolves" the symbol address `addr` to obtain
        // the ordinal that was stored in the XBE thunk table.
        let ordinal = addr & 0x1FF;  // max = 511

        let info = table::get_export_info(ordinal as u16);
        match &info.kind {
            KernelExportKind::Function { abi, arity } => {
                // extract arguments
                let mut args = Vec::new();
                let mut args_left = *arity;
                if let KernelAbi::Fastcall = abi {
                    // obtain first 2 arguments from their registers
                    if args_left > 0 {
                        args.push(cpu.ecx());
                        args_left -= 1;
                    }
                    if args_left > 0 {
                        args.push(cpu.edx());
                        args_left -= 1;
                    }
                }

                // repeatedly popping an argument off the stack will
                // restore left-to-right argument order in `args`.
                for _ in 0..args_left {
                    args.push(pop(cpu, mem).map_err(|e| {
                        error!("memory error while fetching argument: {}", e);
                        ()
                    })?);
                }

                assert_eq!(args.len(), *arity as usize, "internal bug: arity mismatch");
                let args: &[u32] = &args;

                // find and call the shim that will translate the `&[u32]`
                // to the proper fn args
                let result: u32 = {
                    let mut syscall = Syscall {
                        kernel: self,
                        mem,
                        cpu,
                    };

                    macro_rules! dispatch {
                        ( $( $ord:tt => $func:ident, )* ) => {
                            match ordinal {
                                $( $ord => {
                                    let args = FromRawArgs::from_args(args);
                                    trace!("{}{:?}", stringify!($func), args);
                                    syscall.$func(args).into()
                                } )*
                                _ => unimplemented!("HLE function #{}", ordinal),
                            }
                        };
                    }

                    dispatch! {
                        255 => PsCreateSystemThreadEx,
                    }
                };

                cpu.set_eax(result);
                Ok(())
            }
            KernelExportKind::Variable { .. } => {
                error!("attempted HLE call to variable (address {:#010X})", addr);
                Err(())
            }
            KernelExportKind::Unused => {
                error!("attempted HLE call to unused export ID {}", ordinal);
                Err(())
            }
            KernelExportKind::Unimplemented => {
                unimplemented!("HLE function #{}", ordinal);
            }
        }
    }
}

/// Resolves `call`s to kernel functions and performs them via HLE.
impl Hooks for Kernel {
    fn call<M: VirtualMemory>(
        &mut self,
        interp: &mut Interpreter<Self, M>,
        _eip: u32,
        target: u32,
    ) -> Result<HookAction, HookError> {
        if target > 0x80000000 {
            // Everything in kernel space must be a kernel function.
            self.do_hle_call(&mut interp.state, &mut interp.mem, target).map_err(|()| {
                HookError::new("HLE error".to_string()) // FIXME real msg
            })?;
            Ok(HookAction::Nop)
        } else {
            Ok(HookAction::Continue)
        }
    }
}

/// Pops a 32-bit value off the stack.
fn pop<M: VirtualMemory>(state: &mut ::cpu::State, mem: &M) -> Result<u32, MemoryError> {
    let esp = state.esp();
    let value = mem.load_i32(esp)? as u32;
    state.set_esp(esp + 4);
    Ok(value)
}

/// Syscall context.
///
/// All syscalls are implemented as methods on this type, and an instance is
/// created whenever an HLE system function is being called. This also makes
/// this type useful as a system function index in the documentation.
///
/// Currently, rustdoc's formatting of the function parameters is pretty bad
/// (it would be much better when kernel functions would take all of their
/// arguments individually instead of a tuple of them). It would be great if
/// this was fixed somehow (use rustfmt to format the argument list?).
#[derive(Debug)]
pub struct Syscall<'a, M: VirtualMemory + 'a> {
    kernel: &'a mut Kernel,
    mem: &'a mut M,
    cpu: &'a mut ::cpu::State,
}

/*

Kernel functions need:
* `&mut` access to complete memory space
* `&mut Kernel` (bookkeeping data)
* `&mut cpu::State` to read and adjust stack ptr and other regs

Kernel function descriptor:
* ABI (std/fastcall)
* No. of arguments (to adjust stack and extract the args)
* Shim that takes a correctly-sized &[u32]
* Later the JIT probably wants an `extern "C"` shim it can easily call (that
  does `catch_panic`, too)

Also need a way to quickly resolve kernel fns.

*/
