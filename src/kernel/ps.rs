//! `Ps*` functions for process/thread management (**P**rocess **S**tructure).

#![allow(non_snake_case)]

use memory::VirtualMemory;
use kernel::types::*;
use cpu::State;

/// Kernel thread data.
///
/// In order to do context switches correctly, this needs to save the machine
/// state in the same manner as the real kernel would.
#[derive(Debug)]
pub struct Thread {
    /// Saved CPU state needed for context switches.
    saved_state: State,
}

impl<'a, M: VirtualMemory> super::Syscall<'a, M> {
    /// Spawn a new kernel/system level thread.
    ///
    /// # Parameters
    ///
    /// * `thread_handle`: Pointer to a `HANDLE` that will be set to the handle
    ///   of the newly created thread.
    /// * `extra_size`: ???
    /// * `stack_size`: Stack size to allocate (in bytes).
    /// * `tls_size`: ???
    /// * `thread_id`: Pointer to a `DWORD`/`u32` that will be set to the thread
    ///   ID. May be null.
    /// * `ctx1`/`ctx2`: Pointers passed to the thread. (how?)
    /// * `create_suspended`: If `true`, the thread will be created in suspended
    ///   state. If `false`, the thread will start running automatically.
    /// * `debug_stack`: ???
    /// * `start_routine`: Entry point of the created thread.
    pub fn PsCreateSystemThreadEx(&mut self, (
        thread_handle,  // out
        extra_size,
        stack_size,
        tls_size,
        thread_id,      // out
        ctx1,
        ctx2,
        create_suspended,
        debug_stack,
        start_routine,
    ): (
        XPtr<Handle<Thread>>,
        Ulong,
        Ulong,
        Ulong,
        XPtr<Ulong>,
        XPtr<()>,
        XPtr<()>,
        Boolean,
        Boolean,
        XPtr<()>,
    )) -> NtStatus {
        let _ = (thread_handle, extra_size, stack_size, tls_size, thread_id, ctx1, ctx2, create_suspended, debug_stack, start_routine);
        unimplemented!()
    }
}
