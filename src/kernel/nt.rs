//! `Nt*` functions (system services, mainly concerned with I/O).

use memory::VirtualMemory;
use kernel::types::*;
use kernel::object::Object;

/// The NT subsystem (provides system services and file I/O).
#[derive(Debug)]
pub struct Subsystem {}

impl Subsystem {
    pub fn init() -> Self {
        Subsystem {}
    }
}

#[allow(non_snake_case)]
impl<'a, M: VirtualMemory> super::Syscall<'a, M> {
    /// Closes a handle to an object.
    ///
    /// If the handle is the only handle referring to the object, the object
    /// will be destroyed (the consequences depending on the type of the
    /// object).
    ///
    /// This can not only be used to close open files, but also to close handles
    /// to other threads, events and synchronization objects and other types of
    /// objects.
    pub fn NtClose(&mut self, (handle,): (Handle<Object>,)) -> Result<(), NtStatus> {
        self.kernel.close_handle(handle)
            .map_err(|()| NtStatus::STATUS_INVALID_HANDLE)
    }
}
