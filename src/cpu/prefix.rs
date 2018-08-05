//! Instruction prefix byte decoding.

use cpu::instr::{OpSize, Segment};
use cpu::decode::DecoderError;

/// Enumeration of all prefixes.
#[allow(unused)]
pub enum Prefix {
    Lock,
    RepRepe,
    Repne,
    OverrideCs,
    OverrideSs,
    OverrideDs,
    OverrideEs,
    OverrideFs,
    OverrideGs,
    OperandSize,
    AddressSize,
}

bitflags! {
    /// Flags for prefixes where duplicates have no effect and different
    /// prefixes do not collide.
    pub struct PrefixFlags: u8 {
        /// `0x66` - Operand size override.
        ///
        /// In 32-bit mode, this changes the size of register operands back to
        /// 16-bit registers.
        const OVERRIDE_OPERAND = 0x01;
        /// `0x67` - Address size override.
        ///
        /// In 32-bit mode, this changes the interpretation of the Mod-Reg-R/M
        /// byte back to what it is in 16-bit mode, with all consequences.
        const OVERRIDE_ADDRESS = 0x02;
        /// `0xF0`
        const LOCK             = 0x04;
        /// `0xF2`
        const REPNE            = 0x08;
        /// `0xF3` - `rep` or `repe` prefix, depending on the instruction.
        const REP_REPE         = 0x10;
    }
}

/// Collects decoded prefixes for use by the decoder.
#[derive(Debug)]
pub struct Prefixes {
    /// The last segment override prefix we saw.
    segment: Option<Segment>,
    flags: PrefixFlags,
}

impl Prefixes {
    pub fn empty() -> Self {
        Self {
            segment: None,
            flags: PrefixFlags::empty(),
        }
    }

    /// No prefix flags or segment overrides stored.
    pub fn is_empty(&self) -> bool {
        self.flags.is_empty() && self.segment.is_none()
    }

    /// Decodes a prefix byte and merges it with `self`.
    ///
    /// Returns `Ok` when the byte is a valid prefix, and `Err` if not.
    pub fn decode(&mut self, prefix: u8) -> Result<(), ()> {
        match prefix {
            0x66 => self.flags |= PrefixFlags::OVERRIDE_OPERAND,
            0x67 => self.flags |= PrefixFlags::OVERRIDE_ADDRESS,
            0xF0 => self.flags |= PrefixFlags::LOCK,
            0xF3 => self.flags |= PrefixFlags::REP_REPE,
            0xF2 => self.flags |= PrefixFlags::REPNE,
            0x2E => self.segment = Some(Segment::Cs),
            0x36 => self.segment = Some(Segment::Ss),
            0x3E => self.segment = Some(Segment::Ds),
            0x26 => self.segment = Some(Segment::Es),
            0x64 => self.segment = Some(Segment::Fs),
            0x65 => self.segment = Some(Segment::Gs),
            _ => return Err(()),
        }
        Ok(())
    }

    /// Determine the operand size, given the size bit from the opcode.
    ///
    /// If an operand override prefix byte was specified, the size will be
    /// adjusted accordingly and the flag will be removed.
    ///
    /// If `size_bit == true`, default size is assumed to be 32 bits, if
    /// `size_bit == false`, size is assumed to be 8 bits.
    ///
    /// Returns an error if this combination of prefix bytes and size bit is
    /// invalid.
    pub fn size(&mut self, size_bit: bool) -> Result<OpSize, DecoderError> {
        let has_prefix = self.take(PrefixFlags::OVERRIDE_OPERAND);

        Ok(match (size_bit, has_prefix) {
            (false, false) => OpSize::Bits8,
            (false, true) => return Err(DecoderError::ud("use of operand override prefix with 8-bit op")),   // FIXME check
            (true, false) => OpSize::Bits32,
            (true, true) => OpSize::Bits16,
        })
    }

    /// If `self` contains `other`, removes `other` from `self` and returns
    /// `true`.
    pub fn take(&mut self, other: PrefixFlags) -> bool {
        if self.flags.contains(other) {
            self.flags.remove(other);
            true
        } else {
            false
        }
    }

    /// Determine the segment to use for an instruction, given the default.
    pub fn segment(&mut self, dfl: Segment) -> Segment {
        if let Some(seg) = self.segment.take() {
            seg
        } else {
            dfl
        }
    }
}
