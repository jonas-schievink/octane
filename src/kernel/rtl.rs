//! `Rtl*` kernel functions (**R**un**t**ime **L**ibrary).

use memory::VirtualMemory;
use kernel::types::*;

use num_traits::FromPrimitive;

#[allow(non_snake_case)]
impl<'a, M: VirtualMemory> super::Syscall<'a, M> {
    /// Converts an `NTSTATUS` code to the corresponding system error code.
    ///
    /// This is mostly an approximation/guesswork since the real mapping is not
    /// (easily) available. No game should rely on this, however (famous last
    /// words).
    ///
    /// See Microsoft docs on [`RtlNtStatusToDosError`][msdocs] and [system
    /// error codes][syserr].
    ///
    /// [msdocs]: https://docs.microsoft.com/en-us/windows/desktop/api/winternl/nf-winternl-rtlntstatustodoserror
    /// [syserr]: https://docs.microsoft.com/en-us/windows/desktop/Debug/system-error-codes
    pub fn RtlNtStatusToDosError(&mut self, (ntstatus,): (u32,)) -> SysError {
        let ntstatus = match NtStatus::from_u32(ntstatus) {
            Some(s) => s,
            None => return SysError::ERROR_MR_MID_NOT_FOUND,
        };

        match ntstatus {
            NtStatus::STATUS_SUCCESS => SysError::ERROR_SUCCESS,
            NtStatus::STATUS_ACCESS_VIOLATION => SysError::ERROR_INVALID_ADDRESS,
            NtStatus::STATUS_INVALID_HANDLE => SysError::ERROR_INVALID_HANDLE,
            NtStatus::STATUS_NO_MEMORY => SysError::ERROR_OUTOFMEMORY,
        }
    }
}
