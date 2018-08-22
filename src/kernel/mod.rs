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
//! aborts. This is by design, as predictable emulator behaviour cannot be
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
// FIXME: Remove all references that claim that the kernel is based on WinNT/2000

mod mm;
mod ps;
mod rtl;
mod table;
pub mod types;

use self::table::{KernelExportKind, KernelAbi};
use self::types::{Handle, FromRawArgs, ApiReturnValue};
use memory::{MemoryError, MapError, VirtualMemory};
use cpu::interpret::{Hooks, HookAction, HookError, Interpreter};
use cpu;

use xbe::Xbe;
use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::fmt;
use std::error::Error;

/// Stores a collections of handles, each associated to host data of type `T`.
///
/// See `types::Handle<T>`.
#[derive(Debug)]
pub struct HandleSet<T> {
    map: BTreeMap<u32, T>,
}

impl<T> HandleSet<T> {
    /// Creates a new, empty handle set.
    pub fn new() -> Self {
        Self {
            map: BTreeMap::new(),
        }
    }

    /// Insert a new value into the set.
    pub fn insert(&mut self, handle: Handle<T>, value: T) -> &mut T {
        match self.map.entry(handle.raw_addr()) {
            Entry::Vacant(e) => e.insert(value),
            Entry::Occupied(_) => {
                panic!("attempted to overwrite value associated to handle {:?}", handle);
            }
        }
    }

    pub fn get(&self, handle: Handle<T>) -> Option<&T> {
        self.map.get(&handle.raw_addr())
    }

    pub fn get_mut(&mut self, handle: Handle<T>) -> Option<&mut T> {
        self.map.get_mut(&handle.raw_addr())
    }
}

/// Kernel implementation and host-side data.
///
/// On the Xbox, the equivalent of this state is scattered somewhere in the top
/// 2G of address space. Here, we can also store it in host memory. This ensures
/// that a rogue guest program cannot overwrite important parts of it and
/// consequently cause crashes or exploit the emulator. Note that all kernel
/// variables that can be imported by the guest program still need to be stored
/// in the >2G kernel area of the guest address space to be accessible. The same
/// goes for any non-opaque structures.
#[derive(Debug)]
pub struct Kernel {
    /// Next "magic" address to use for the next handle that is allocated.
    next_handle_addr: u32,

    ps: ps::Subsystem,
    mm: mm::Subsystem,
}

impl Kernel {
    /// Creates a new kernel context, loads an XBE into memory and prepares it
    /// for execution.
    pub fn load<M: VirtualMemory>(xbe: &Xbe, mem: &mut M) -> Result<Self, LoadError> { // TODO proper error
        const HANDLE_START: u32 = 0xC0000000;

        // Set up the kernel first
        let mut this = Self {
            // Start handle is chosen arbitrarily but must not collide with any
            // allocated or allocatable memory.
            next_handle_addr: HANDLE_START,
            ps: ps::Subsystem::init(),
            mm: mm::Subsystem::init(),
        };

        info!("allocating memory for XBE");

        // Reserve the zero page
        this.mm.allocate_exact_range(0x00000000..=0x00000000)?;

        // Reserve handle space
        this.mm.allocate_exact_range(HANDLE_START..=0xffffffff)?;

        // Map the XBE headers at the specified base address
        let xbe_range = xbe.base_address()..=xbe.base_address()+xbe.header_size();
        this.mm.allocate_exact_range(xbe_range.clone())?;
        mem.add_mapping(xbe_range, &xbe.raw_data()[..xbe.header_size() as usize], "<xbe headers>")?;

        // XBE's usually specify the right values so that the mapped headers end
        // exactly where the first section starts (after rounding up to whole
        // pages).

        // Determine the virtual address range where sections are placed and
        // reserve it.
        // FIXME xbe crate should check there's at least 1 section
        let min = xbe.sections().map(|s| *s.virt_range().start()).min().expect("no sections");
        let max = xbe.sections().map(|s| *s.virt_range().end()).max().expect("no sections");
        info!("reserving section range {:#010X}..={:#010X} ({} Bytes)", min, max, max - min);
        this.mm.allocate_exact_range(min..=max)?;

        // FIXME we probably want the xbe crate to check that no sections overlap
        // FIXME this probably needs to handle the preload flag, but I haven't found much info on it
        for section in xbe.sections() {
            let range = section.virt_range();
            trace!("section {} -> {:#010X}..={:#010X}", section.name(), range.start(), range.end());
            mem.add_mapping(range, section.data(), section.name())?;
        }

        // TODO: Resolve kernel imports
        // TODO: Reserve space for shared kernel variables and initialize them

        // Allocate stack for main thread and initialize it
        info!("setting up main thread");
        this.setup_main_thread(xbe, mem);

        Ok(this)
    }

    fn setup_main_thread<M: VirtualMemory>(&mut self, xbe: &Xbe, mem: &mut M) {
        let entry = xbe.entry_point();

        let stack_size = 1024 * 64; // FIXME is this configurable?
        let stack = self.mm.allocate(stack_size)
            .expect("couldn't allocate stack for main thread");
        mem.add_mapping(stack.clone(), &[], "<stack>")
            .expect("couldn't map stack for main thread");

        let thread = ps::Thread::new(entry, *stack.start(), stack_size);
        let handle = self.alloc_handle();
        self.ps.register_thread(handle, thread);
    }

    /// Returns the CPU state stored for the current thread.
    ///
    /// This is not the current state of the CPU. The state will not update
    /// automatically when the interpreter runs. Updating the saved state
    /// requires a context switch to another thread.
    pub fn current_thread_state(&self) -> cpu::State {
        self.ps.current_thread().saved_state()
    }

    /// Allocates a new handle that can refer to a value of type `T`.
    ///
    /// The handle starts out unassociated and doesn't refer to any value.
    pub fn alloc_handle<T>(&mut self) -> Handle<T> {
        let addr = self.next_handle_addr;
        self.next_handle_addr = self.next_handle_addr.checked_add(1)
            .expect("out of handles");
        Handle::from(addr)
    }

    /// Perform an HLE call. Called by the interpreter.
    ///
    /// In almost all cases, this returns `Ok`. It only returns `Err` if the
    /// arguments could not be read (due to a memory fault) or if `addr` does
    /// not refer to a callable kernel function.
    ///
    /// In particular, this will return `Ok` even if the called function
    /// encounters an error. Such errors will be forwarded to the guest program
    /// via the intended means (mostly return codes).
    ///
    /// # Parameters
    ///
    /// * `cpu`: CPU state. Will be changed by the call (eg. to store the
    ///   return value and pop the arguments).
    /// * `mem`: Xbox virtual memory. May or may not be modified by the call.
    /// * `addr`: Marker address of the called kernel function. Must be above
    ///   `0x80000000` (kernel space).
    // TODO document expected `state` (eip and esp point where?)
    pub fn do_hle_call<M: VirtualMemory>(
        &mut self,
        cpu: &mut ::cpu::State,
        mem: &mut M,
        addr: u32,
    ) -> Result<(), HleError> {
        if addr < 0x80000000 || addr > 0x800001FF {
            return Err(format!("address {:#010X} not a valid HLE function", addr).into());
        }

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
                        HleError::from(format!("memory error while fetching argument: {}", e))
                    })?);
                }

                assert_eq!(args.len(), *arity as usize, "internal bug: arity mismatch");
                let args: &[u32] = &args;

                let result: u32 = {
                    let mut syscall = Syscall {
                        kernel: self,
                        mem,
                        cpu,
                    };

                    /// Converts arguments and calls the implementation of the
                    /// syscalls.
                    macro_rules! dispatch {
                        ( $( $ord:tt => $func:ident, )* ) => {
                            match ordinal {
                                $( $ord => {
                                    let args = FromRawArgs::from_args(args);
                                    trace!("{}{:?}", stringify!($func), args);
                                    syscall.$func(args).into_u32()
                                } )*
                                _ => unimplemented!("HLE function #{}", ordinal),
                            }
                        };
                    }

                    dispatch! {
                        165 => MmAllocateContiguousMemory,
                        255 => PsCreateSystemThreadEx,
                    }
                };

                cpu.set_eax(result);
                Ok(())
            }
            KernelExportKind::Variable { .. } => {
                Err(format!("attempted HLE call to variable (address {:#010X})", addr).into())
            }
            KernelExportKind::Unused => {
                Err(format!("attempted HLE call to unused export ID {}", ordinal).into())
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
            self.do_hle_call(&mut interp.state, &mut interp.mem, target).map_err(|e| {
                HookError::new(e.to_string())
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

/// Error returned while loading an XBE.
#[derive(Debug)]
pub enum LoadError {
    /// Could not map section data to virtual memory.
    Map(MapError),
    /// Could not allocate virtual address space.
    Alloc(mm::AllocError),
}
// FIXME LoadError might be unnecessary? (is loading infallible?)

impl Error for LoadError {}

impl fmt::Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoadError::Map(e) => e.fmt(f),
            LoadError::Alloc(e) => e.fmt(f),
        }
    }
}

impl From<MapError> for LoadError {
    fn from(e: MapError) -> Self {
        LoadError::Map(e)
    }
}

impl From<mm::AllocError> for LoadError {
    fn from(e: mm::AllocError) -> Self {
        LoadError::Alloc(e)
    }
}

/// Error when attempting an HLE call.
///
/// This can be returned by `do_hle_call` and indicates that dispatching the
/// call failed. If the call could be performed, but the called function
/// encounters an error, the error is returned to the calling program and is not
/// interpreted as an `HleError`.
#[derive(Debug)]
pub struct HleError(String);

impl Error for HleError {}

impl fmt::Display for HleError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<S> From<S> for HleError where S: Into<String> {
    fn from(s: S) -> Self {
        HleError(s.into())
    }
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
