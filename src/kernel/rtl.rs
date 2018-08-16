//! `Rtl*` kernel functions.

#![allow(non_snake_case)]

use memory::VirtualMemory;
use kernel::types::*;

impl<'a, M: VirtualMemory> super::Syscall<'a, M> {
    pub fn _RtlCompareMemory(&mut self, addr1: XPtr<()>, addr2: XPtr<()>, len: u32) -> u32 {
        let _ = (addr1, addr2, len);
        unimplemented!()
    }
}
