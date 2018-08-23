//! `Rtl*` kernel functions (**R**un**t**ime **L**ibrary).

use memory::VirtualMemory;
use kernel::types::*;

#[allow(non_snake_case)]
impl<'a, M: VirtualMemory> super::Syscall<'a, M> {
    pub fn _RtlCompareMemory(&mut self, addr1: XPtr<()>, addr2: XPtr<()>, len: u32) -> u32 {
        let _ = (addr1, addr2, len);
        unimplemented!()
    }
}
