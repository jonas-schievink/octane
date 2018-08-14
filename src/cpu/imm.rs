//! Immediate value manipulation.
//!
//! An immediate is an 8-, 16- or 32-bit value used as the operand or result of
//! an instruction. This includes "actual" immediates encoded in the instruction
//! stream, but also fetched memory operands or register contents, because using
//! the same structure for those makes manipulations very easy.

use cpu::instr::OpSize;

use std::fmt;

/// An 8, 16 or 32-bit immediate value.
///
/// Note that while `Immediate` only stores signed values, whether the sign is
/// meaningful depends on the operation performed on the values.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Immediate {
    Imm8(i8),
    Imm16(i16),
    Imm32(i32),
}

impl Immediate {
    /// Sign-extend or truncate the immediate to a different size.
    pub fn sign_ext_to(&self, size: OpSize) -> Immediate {
        let extended = self.sign_extended();
        match size {
            OpSize::Bits8 => Immediate::Imm8(extended as i8),
            OpSize::Bits16 => Immediate::Imm16(extended as i16),
            OpSize::Bits32 => Immediate::Imm32(extended as i32),
        }
    }

    /// Zero-extend or truncate the immediate to a different size.
    pub fn zero_ext_to(&self, size: OpSize) -> Immediate {
        let extended = self.zero_extended();
        match size {
            OpSize::Bits8 => Immediate::Imm8(extended as u8 as i8),
            OpSize::Bits16 => Immediate::Imm16(extended as u16 as i16),
            OpSize::Bits32 => Immediate::Imm32(extended as i32),
        }
    }

    /// Returns the sign-extended immediate value as an `i32`.
    pub fn sign_extended(&self) -> i32 {
        match *self {
            Immediate::Imm8(imm) => imm as i32,
            Immediate::Imm16(imm) => imm as i32,
            Immediate::Imm32(imm) => imm,
        }
    }

    /// Returns the zero-extended immediate value as a `u32`.
    pub fn zero_extended(&self) -> u32 {
        match *self {
            Immediate::Imm8(imm) => imm as u8 as u32,
            Immediate::Imm16(imm) => imm as i16 as u32,
            Immediate::Imm32(imm) => imm as u32,
        }
    }

    /// Returns the size of the immediate value.
    pub fn size(&self) -> OpSize {
        match self {
            Immediate::Imm8(_) => OpSize::Bits8,
            Immediate::Imm16(_) => OpSize::Bits16,
            Immediate::Imm32(_) => OpSize::Bits32,
        }
    }

    /// If this immediate is 8 bits in size, returns it as a `u8`.
    pub fn as_u8(&self) -> Option<u8> {
        if let Immediate::Imm8(n) = self {
            Some(*n as u8)
        } else {
            None
        }
    }

    pub fn as_u16(&self) -> Option<u16> {
        if let Immediate::Imm16(n) = self {
            Some(*n as u16)
        } else {
            None
        }
    }

    pub fn as_u32(&self) -> Option<u32> {
        if let Immediate::Imm32(n) = self {
            Some(*n as u32)
        } else {
            None
        }
    }
}

impl From<u8> for Immediate {
    fn from(imm: u8) -> Self {
        Immediate::Imm8(imm as i8)
    }
}

impl From<u16> for Immediate {
    fn from(imm: u16) -> Self {
        Immediate::Imm16(imm as i16)
    }
}

impl From<u32> for Immediate {
    fn from(imm: u32) -> Self {
        Immediate::Imm32(imm as i32)
    }
}

impl From<i8> for Immediate {
    fn from(imm: i8) -> Self {
        Immediate::Imm8(imm)
    }
}

impl From<i16> for Immediate {
    fn from(imm: i16) -> Self {
        Immediate::Imm16(imm)
    }
}

impl From<i32> for Immediate {
    fn from(imm: i32) -> Self {
        Immediate::Imm32(imm)
    }
}

impl fmt::UpperHex for Immediate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Immediate::Imm8(imm) => imm.fmt(f),
            Immediate::Imm16(imm) => imm.fmt(f),
            Immediate::Imm32(imm) => imm.fmt(f),
        }
    }
}

impl fmt::LowerHex for Immediate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Immediate::Imm8(imm) => imm.fmt(f),
            Immediate::Imm16(imm) => imm.fmt(f),
            Immediate::Imm32(imm) => imm.fmt(f),
        }
    }
}

/// Prints the signed decimal value of the immediate.
impl fmt::Display for Immediate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Immediate::Imm8(imm) => imm.fmt(f),
            Immediate::Imm16(imm) => imm.fmt(f),
            Immediate::Imm32(imm) => imm.fmt(f),
        }
    }
}
