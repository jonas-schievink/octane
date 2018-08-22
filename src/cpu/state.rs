//! x86 processor state.

use cpu::instr::{Register, Immediate};
use cpu::Flags;

/// CPU state consisting of the emulated registers.
#[derive(Debug, Clone)]
pub struct State {
    eax: u32,
    ebx: u32,
    ecx: u32,
    edx: u32,

    esi: u32,
    edi: u32,
    ebp: u32,
    esp: u32,

    eip: u32,

    flags: Flags,
}

impl State {
    /// Create a new state, suitable for running an XBE.
    ///
    /// # Parameters
    ///
    /// * `eip`: Initial value for `eip` (address of the first instruction).
    /// * `esp`: Initial value for the stack pointer. Should normally point just
    ///   behind the allocated stack memory.
    pub fn new(eip: u32, esp: u32) -> Self {
        Self {
            eax: 0,
            ebx: 0,
            ecx: 0,
            edx: 0,
            esi: 0,
            edi: 0,
            ebp: 0,
            esp,
            eip,
            flags: Flags::empty(),
        }
    }

    pub fn flags(&self) -> &Flags { &self.flags }

    /// Sets all status flags `flags` to the value of `bit`.
    pub fn update_flags(&mut self, flags: Flags, bit: bool) {
        self.flags.set(flags, bit);
    }

    /// Retrieve the value of the given register.
    pub fn get_register(&self, reg: Register) -> Immediate {
        use cpu::instr::Register::*;

        match reg {
            Al => self.al().into(),
            Ah => self.ah().into(),
            Bl => self.bl().into(),
            Bh => self.bh().into(),
            Cl => self.cl().into(),
            Ch => self.ch().into(),
            Dl => self.dl().into(),
            Dh => self.dh().into(),
            Ax => self.ax().into(),
            Bx => self.bx().into(),
            Cx => self.cx().into(),
            Dx => self.dx().into(),
            Eax => self.eax().into(),
            Ebx => self.ebx().into(),
            Ecx => self.ecx().into(),
            Edx => self.edx().into(),
            Si => self.si().into(),
            Esi => self.esi().into(),
            Di => self.di().into(),
            Edi => self.edi().into(),
            Bp => self.bp().into(),
            Ebp => self.ebp().into(),
            Sp => self.sp().into(),
            Esp => self.esp().into(),
        }
    }
}

/// Generates accessor methods for registers and their subregisters.
macro_rules! accessors {
    (
        $base:ident: [ $getter32:ident/$setter32:ident ]
    ) => {
        pub fn $getter32(&self) -> u32 { self.$base }
        pub fn $setter32(&mut self, value: u32) { self.$base = value; }
    };
    (
        $base:ident: [ $getter32:ident/$setter32:ident, $getter16:ident/$setter16:ident ]
    ) => {
        pub fn $getter32(&self) -> u32 { self.$base }
        pub fn $setter32(&mut self, value: u32) { self.$base = value; }
        pub fn $getter16(&self) -> u16 { self.$base as u16 }
        pub fn $setter16(&mut self, value: u16) { self.$base = value.into(); }
    };
    (
        $base:ident: [ $getter32:ident/$setter32:ident, $getter16:ident/$setter16:ident, $getter8h:ident/$setter8h:ident, $getter8l:ident/$setter8l:ident ]
    ) => {
        pub fn $getter32(&self) -> u32 { self.$base }
        pub fn $setter32(&mut self, value: u32) { self.$base = value; }
        pub fn $getter16(&self) -> u16 { self.$base as u16 }
        pub fn $setter16(&mut self, value: u16) { self.$base = (self.$base & 0xFFFF0000) | (value as u32); }
        pub fn $getter8h(&self) -> u8 { (self.$base >> 8) as u8 }
        pub fn $setter8h(&mut self, value: u8) { self.$base = (self.$base & 0xFFFF00FF) | (value as u32) << 8; }
        pub fn $getter8l(&self) -> u8 { self.$base as u8 }
        pub fn $setter8l(&mut self, value: u8) { self.$base = (self.$base & 0xFFFFFF00) | (value as u32); }
    };
}

impl State {
    accessors!(eax: [eax/set_eax, ax/set_ax, ah/set_ah, al/set_al]);
    accessors!(ebx: [ebx/set_ebx, bx/set_bx, bh/set_bh, bl/set_bl]);
    accessors!(ecx: [ecx/set_ecx, cx/set_cx, ch/set_ch, cl/set_cl]);
    accessors!(edx: [edx/set_edx, dx/set_dx, dh/set_dh, dl/set_dl]);
    accessors!(esi: [esi/set_esi, si/set_si]);
    accessors!(edi: [edi/set_edi, di/set_di]);
    accessors!(ebp: [ebp/set_ebp, bp/set_bp]);
    accessors!(esp: [esp/set_esp, sp/set_sp]);
    accessors!(eip: [eip/set_eip]);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn accessors() {
        let mut state = State::new(0, 0);
        assert_eq!(state.eax(), 0);
        assert_eq!(state.ax(), 0);
        assert_eq!(state.ah(), 0);
        assert_eq!(state.al(), 0);
        state.set_eax(!0);
        assert_eq!(state.eax(), !0);
        assert_eq!(state.ax(), !0);
        assert_eq!(state.ah(), !0);
        assert_eq!(state.al(), !0);
        state.set_al(0);
        assert_eq!(state.ah(), !0);
        assert_eq!(state.al(), 0);
        state.set_eax(!0);
        state.set_ah(0);
        assert_eq!(state.ah(), 0);
        assert_eq!(state.al(), !0);
    }
}
