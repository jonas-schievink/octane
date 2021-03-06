//! `Ps*` functions for process/thread management (**P**rocess **S**tructure).

use memory::VirtualMemory;
use kernel::types::*;
use cpu::State;

use std::u32;

/// Process and thread management subsystem.
///
/// `kernel.ps`.
#[derive(Debug)]
pub struct Subsystem {
    next_thread_id: u32,
    /// Threads running on the system.
    ///
    /// This starts out with just the program's main thread. The program can
    /// launch other threads via `PsCreateSystemThread(Ex)`.
    threads: Vec<Handle<Thread>>,
    /// Index into `threads` (not a thread ID!).
    current_thread: usize,
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
            threads: Vec::new(),
            current_thread: 0,    // starts out invalid, later points to first thread
        }
    }
}

impl super::Kernel {
    /// Registers a new thread with the process subsystem.
    ///
    /// If no thread is currently registered, this thread will also be made
    /// active.
    ///
    /// Note that closing the returned handle will not cancel the thread. An
    /// internal handle is created that keeps the thread alive.
    pub fn register_thread(&mut self, mut thread: Thread) -> Handle<Thread> {
        let id = self.ps.next_thread_id;
        thread.id = id;
        self.ps.next_thread_id += 1;

        debug!("register_thread: {:?}", thread);
        let state = thread.saved_state();
        debug!("register_thread: entry={:#010X} ebp={:#010X} esp={:#010X} stack_start={:#010X}", state.eip(), state.ebp(), state.esp(), thread.stack_start);
        let handle = self.register_object(thread);
        info!("created new thread: id={} handle={:?}", id, handle);
        let internal = self.dup_handle(&handle).expect("handle should be valid");
        self.ps.threads.push(internal);

        handle
    }

    /// Get a reference to the currently active thread.
    pub fn current_thread(&self) -> Option<&Thread> {
        self.objects.get(&self.ps.threads[self.ps.current_thread])
    }

    /// Searches the thread list for a runnable thread, starting at the given
    /// index.
    ///
    /// Returns the index into `ps.threads` of the first runnable thread found,
    /// or `None` if no runnable thread was found.
    pub fn find_runnable_thread(&mut self, start_index: usize) -> Option<usize> {
        // Starting at the thread at `start_index`, search the thread list for a
        // runnable one.
        let first_part = self.ps.threads.iter().enumerate().skip(start_index);
        let second_part = self.ps.threads[..start_index].iter().enumerate();

        first_part.chain(second_part).find(|(idx, handle)| {
            let thread: &Thread = self.objects.get(handle).expect("not a thread handle");
            if thread.is_runnable() {
                debug!("found runnable thread at index {}: {:?}", idx, thread);
                true
            } else {
                false
            }
        }).map(|(idx, _)| idx)
    }

    pub fn terminate_current_thread(&mut self) {
        let handle = self.ps.threads.swap_remove(self.ps.current_thread);
        self.close_handle(handle).expect("couldn't close handle to current thread");

        let start_idx = self.ps.current_thread+1;
        if let Some(idx) = self.find_runnable_thread(start_idx) {
            self.ps.current_thread = idx;
        } else {
            info!("last runnable thread exited");
            self.ps.current_thread = 0;
        }
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

    pub fn is_runnable(&self) -> bool {
        if let ThreadState::Running = self.state {
            true
        } else {
            false
        }
    }

    /// Gets the ID of this thread.
    ///
    /// While multiple handles referring to the same thread can be created, the
    /// thread ID is always unique.
    pub fn id(&self) -> u32 {
        self.id
    }
}

/// The thread environment block (TEB).
///
/// This is still in need of documentation. If you know what certain fields are
/// used for, please contribute some documentation. Existing documentation,
/// field names or types might also be incorrect.
///
/// The first 3 fields are equivalent to the fields of the Win32 TEB. The total
/// size of the Xbox TEB is at least 44 Bytes. This was determined from the
/// occurrence of 32-bit `mov`s with an offset of `0x28` (40 Bytes).
#[repr(C, packed)]
#[allow(unused)]    // FIXME
struct Teb {
    /// `0x00`: Pointer to the SEH record list.
    seh_record: XPtr<()>,
    /// `0x04`: First address past the allocated stack (high address).
    stack_base: u32,
    /// `0x08`: Lowest address allocated for the stack.
    stack_limit: u32,

    unknown: [u8; 44 - 3*4],

    // Other offsets of interest:
    // 0x20, 0x24, 0x28
    // 0x28 is a pointer to a structure seemingly related to SEH or debugging?
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
        let mut thread = Thread::new(start_routine.raw_addr(), stack_start, *stack_size);
        super::push(thread.saved_state_mut(), self.mem, ctx2.raw_addr())?;
        super::push(thread.saved_state_mut(), self.mem, ctx1.raw_addr())?;
        let ebp = thread.saved_state.esp();
        // Return address
        super::push(thread.saved_state_mut(), self.mem, super::THREAD_EXIT_SENTINEL)?;
        thread.saved_state_mut().set_ebp(ebp);

        if *create_suspended {
            thread.set_state(ThreadState::Suspended);
        }

        // Register thread, allocate thread ID
        let handle = self.kernel.register_thread(thread);
        let id = self.kernel.objects.get::<Thread>(&handle).unwrap().id();

        // Write back ID and handle
        self.mem.store_u32(thread_handle.raw_addr(), handle.raw_addr())?;
        if thread_id.raw_addr() != 0 {
            self.mem.store_u32(thread_id.raw_addr(), id)?;
        }

        Ok(())
    }

    pub fn PsTerminateSystemThread(&mut self, (exit_status,): (u32,)) {
        info!("thread #{} terminating with code {} ({:?})", self.kernel.ps.current_thread, exit_status, NtStatus::from_u32(exit_status));

        self.kernel.terminate_current_thread();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::size_of;

    #[test]
    fn teb_size() {
        // If you change the size of `Teb`, adjust this test and the doc comment
        // on `Teb` accordingly.
        assert_eq!(size_of::<Teb>(), 44);
    }
}
