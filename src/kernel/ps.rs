//! `Ps*` functions for process/thread management (**P**rocess **S**tructure).

use memory::VirtualMemory;
use kernel::types::*;
use super::HandleSet;
use cpu::State;

use std::u32;

/// Process and thread management subsystem.
#[derive(Debug)]
pub struct Subsystem {
    next_thread_id: u32,
    /// Threads running on the system.
    ///
    /// This starts out with just the program's main thread. The program can
    /// launch other threads via `PsCreateSystemThread(Ex)`.
    threads: HandleSet<Thread>,
    /// Handle of the currently active thread.
    current_thread: Handle<Thread>,
}

impl Subsystem {
    /// Initialize the process subsystem.
    ///
    /// The subsystem starts out without any threads. You must call
    /// `register_thread` before using the subsystem to ensure there's at least
    /// one thread on the system.
    pub fn init() -> Self {
        Self {
            next_thread_id: 0,
            threads: HandleSet::new(),
            current_thread: Handle::from(0),    // starts out invalid
        }
    }

    /// Registers a new thread with the process subsystem.
    ///
    /// If no thread is currently registered, this thread will also be made
    /// active.
    pub fn register_thread(&mut self, handle: Handle<Thread>, mut thread: Thread) -> &mut Thread {
        if self.current_thread.raw_addr() == 0 {
            // first thread = main thread = initially active thread
            self.current_thread = handle.clone();
        }
        thread.id = self.next_thread_id;
        self.next_thread_id += 1;
        self.threads.insert(handle, thread)
    }

    /// Try to get the thread referred to by `handle`.
    pub fn thread(&self, handle: Handle<Thread>) -> Option<&Thread> {
        self.threads.get(handle)
    }

    /// Get a reference to the currently active thread.
    ///
    /// This cannot normally fail since at least one thread usually exists.
    pub fn current_thread(&self) -> &Thread {
        self.thread(self.current_thread.clone()).expect("current thread doesn't exist")
    }
}

#[derive(Debug)]
pub enum ThreadState {
    /// Thread is suspended (paused by another thread until further notice).
    Suspended,
    /// Thread is runnable.
    Running,
}

/// Kernel thread data.
///
/// In order to do context switches correctly, this needs to save the machine
/// state in the same manner as the real kernel would.
#[derive(Debug)]
pub struct Thread {
    /// Thread ID.
    id: u32,
    /// Saved CPU state needed for context switches.
    saved_state: State,
    /// Thread state (is it runnable?).
    state: ThreadState,
    /// Start address of the allocated stack.
    stack_start: u32,
}

impl Thread {
    /// Create a new `Thread` object.
    ///
    /// To register the created thread, call `ps::Subsystem::register_thread`.
    ///
    /// # Parameters
    ///
    /// * `entry`: The entry point where execution should start.
    /// * `stack_start`: Lowest address part of the allocated stack.
    /// * `stack_len`: Stack size in bytes.
    pub fn new(entry: u32, stack_start: u32, stack_len: u32) -> Self {
        info!("new thread @{:#010X}", entry);
        let stack_end = stack_start + stack_len;
        Self {
            id: u32::MAX,
            saved_state: State::new(entry, stack_end),
            state: ThreadState::Running,
            stack_start,
        }
    }

    /// Returns a copy of the saved CPU state for this thread.
    pub fn saved_state(&self) -> State {
        self.saved_state.clone()
    }

    pub fn saved_state_mut(&mut self) -> &mut State {
        &mut self.saved_state
    }

    pub fn set_state(&mut self, state: ThreadState) {
        self.state = state;
    }

    pub fn id(&self) -> u32 {
        self.id
    }
}

#[allow(non_snake_case)]
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
    /// * `ctx1`/`ctx2`: Pointers passed to `start_routine` as on-stack
    ///   arguments.
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
    )) -> Result<(), NtStatus> {
        if *debug_stack {
            warn!("PsCreateSystemThreadEx: `debug_stack` is not implemented");
        }

        if *extra_size > 0 {
            warn!("PsCreateSystemThreadEx: `extra_size` is not implemented");
        }

        if *tls_size > 0 {
            warn!("PsCreateSystemThreadEx: `tls_size` is not implemented");
        }

        let stack_start = self.allocate_pages(*stack_size);
        if stack_start == 0 {
            return Err(NtStatus::STATUS_NO_MEMORY);
        }

        // The ABI of the `start_routine` is a bit weird: First, `ctx2` and
        // `ctx1` are pushed onto the stack, then `ebp` is set to the value of
        // `esp`, then the start routine is called (and the return address is
        // pushed).
        // TODO: Verify that this is what the real kernel does
        let mut esp = stack_start + *stack_size;
        esp -= 4;
        self.mem.store_u32(esp, ctx2.raw_addr())?;
        esp -= 4;
        self.mem.store_u32(esp, ctx1.raw_addr())?;
        let ebp = esp;
        // Return address (FIXME we don't really have one)
        esp -= 4;
        self.mem.store_u32(esp, !0)?;

        let handle = self.kernel.alloc_handle();
        let mut thread = Thread::new(start_routine.raw_addr(), stack_start, *stack_size);
        {
            // Adjust esp and set ebp
            let state = thread.saved_state_mut();
            state.set_esp(esp);
            state.set_ebp(ebp);
        }
        if *create_suspended {
            thread.set_state(ThreadState::Suspended);
        }

        // Register thread, allocate thread ID
        let thread = self.kernel.ps.register_thread(handle.clone(), thread);

        // Write back ID and handle
        self.mem.store_u32(thread_handle.raw_addr(), handle.raw_addr())?;
        if thread_id.raw_addr() != 0 {
            self.mem.store_u32(thread_id.raw_addr(), thread.id())?;
        }

        Ok(())
    }
}