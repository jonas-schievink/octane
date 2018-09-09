use cpu::ExecutionEngine;
use memory::VirtualMemory;

use gdbstub::StubCalls;
use gdbstub::targets::{x86, TargetDesc};
use cpu::State;
use cpu::Flags;

use std::u32;

/// Wraps any `ExecutionEngine` and implements `gdbstub`'s `StubCalls` trait.
#[derive(Debug)]
pub struct Debugger<EE: ExecutionEngine> {
    ee: EE,
}

impl<EE: ExecutionEngine> Debugger<EE> {
    pub fn new(exec_engine: EE) -> Self {
        Self {
            ee: exec_engine,
        }
    }

    pub fn execution_engine(&mut self) -> &mut EE {
        &mut self.ee
    }

    fn state(&mut self) -> &mut State {
        self.ee.state()
    }
}

impl<EE: ExecutionEngine> StubCalls for Debugger<EE> {
    type Target = x86::I386;

    fn read_registers(&mut self) -> <Self::Target as TargetDesc>::Registers {
        let s = self.state();
        x86::X86Registers {
            eax: s.eax(),
            ebx: s.ebx(),
            ecx: s.ecx(),
            edx: s.edx(),
            esp: s.esp(),
            ebp: s.ebp(),
            esi: s.esi(),
            edi: s.edi(),
            eip: s.eip(),
            eflags: s.flags().to_eflags(),
            // Segment selector:
            // MSb                            LSb
            // 13-bit Index into G/LDT | TI | RPL
            // TI = Table indicator (0 = GDT, 1 = LDT)
            // RPL = Requested privilege level
            // Index = 1 (0 = null descriptor)
            // Note that we might need to model this at least partially because
            // games *can* modify the GDT.
            cs: 0x10,
            ss: 0x10,
            ds: 0x10,
            es: 0x10,
            fs: 0x10,
            gs: 0x10,
            st0: [0; 10],
            st1: [0; 10],
            st2: [0; 10],
            st3: [0; 10],
            st4: [0; 10],
            st5: [0; 10],
            st6: [0; 10],
            st7: [0; 10],
            fctrl: 0,
            fstat: 0,
            ftag: 0,
            fiseg: 0,
            fioff: 0,
            foseg: 0,
            fooff: 0,
            fop: 0,
            xmm0: 0,
            xmm1: 0,
            xmm2: 0,
            xmm3: 0,
            xmm4: 0,
            xmm5: 0,
            xmm6: 0,
            xmm7: 0,
            mxcsr: 0,
        }
    }

    fn write_registers(&mut self, regs: <Self::Target as TargetDesc>::Registers) {
        let s = self.state();
        s.set_eax(regs.eax);
        s.set_ebx(regs.ebx);
        s.set_ecx(regs.ecx);
        s.set_edx(regs.edx);
        s.set_esp(regs.esp);
        s.set_ebp(regs.ebp);
        s.set_esi(regs.esi);
        s.set_edi(regs.edi);
        s.set_eip(regs.eip);
        s.set_flags(Flags::from_eflags(regs.eflags));
    }

    fn read_mem(&mut self, addr: u64) -> Result<u8, ()> {
        if addr > u32::MAX.into() {
            Err(())
        } else {
            self.ee.memory().load(addr as u32).map_err(|_| ())
        }
    }

    fn write_mem(&mut self, addr: u64, byte: u8) -> Result<(), ()> {
        if addr > u32::MAX.into() {
            Err(())
        } else {
            self.ee.memory().store(addr as u32, byte).map_err(|_| ())
        }
    }

    fn cont(&mut self) {
        self.ee.run().ok(); // FIXME handle error, convert to appropriate signal
    }

    fn step(&mut self) {
        self.ee.step().ok(); // FIXME handle error, convert to signal
    }
}
