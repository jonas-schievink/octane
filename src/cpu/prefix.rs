//! Instruction prefix byte decoding.

use cpu::instr::OpSize;
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
    /// Bitmask of raw instruction prefixes.
    ///
    /// Their meaning depends on the instruction opcode. In (dis)assembly, not
    /// all prefixes show up as actual prefix words before the mnemonic.
    ///
    /// We collect prefix byte in this bitmask and later build a completed
    /// instruction with instruction-specific prefix from this.
    pub struct RawPrefixes: u16 {
        /// `0xF0`
        const LOCK       = 0x0100;
        /// `0xF2`
        const REPNE      = 0x0400;
        /// `0xF3` - `rep` or `repe` prefix, depending on the instruction.
        const REP_REPE   = 0x0800;
        /// `0x26`
        const OVERRIDE_ES = 0x0010;
        /// `0x2E` - Use CS instead of the instruction's default segment.
        ///
        /// This prefix doubles as a "not taken" branch hint.
        const OVERRIDE_CS = 0x0020;
        /// `0x36`
        const OVERRIDE_SS = 0x0040;
        /// `0x3E` - Use DS instead of the instruction's default segment.
        ///
        /// This prefix doubles as a "taken" branch hint.
        const OVERRIDE_DS = 0x0080;
        /// `0x64`
        const OVERRIDE_FS = 0x0001;
        /// `0x65`
        const OVERRIDE_GS = 0x0002;
        /// `0x66` - Operand size override.
        ///
        /// In 32-bit mode, this changes the size of register operands back to
        /// 16-bit registers.
        const OVERRIDE_OPERAND = 0x0004;
        /// `0x67` - Address size override.
        ///
        /// In 32-bit mode, this changes the interpretation of the Mod-Reg-R/M
        /// byte back to what it is in 16-bit mode, with all consequences.
        const OVERRIDE_ADDRESS = 0x0008;
    }
}

impl RawPrefixes {
    /// Decodes a prefix byte and merges it with `self`.
    ///
    /// Returns `Ok` when the byte is a valid prefix, and `Err` if not.
    pub fn decode(self, prefix: u8) -> Result<Self, Self> {
        Ok(self | match prefix {
            0xF0 => RawPrefixes::LOCK,
            0xF3 => RawPrefixes::REP_REPE,
            0xF2 => RawPrefixes::REPNE,
            0x2E => RawPrefixes::OVERRIDE_CS,
            0x36 => RawPrefixes::OVERRIDE_SS,
            0x3E => RawPrefixes::OVERRIDE_DS,
            0x26 => RawPrefixes::OVERRIDE_ES,
            0x64 => RawPrefixes::OVERRIDE_FS,
            0x65 => RawPrefixes::OVERRIDE_GS,
            0x66 => RawPrefixes::OVERRIDE_OPERAND,
            0x67 => RawPrefixes::OVERRIDE_ADDRESS,
            _ => return Err(self),
        })
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
        let has_prefix = self.take(Self::OVERRIDE_OPERAND);

        Ok(match (size_bit, has_prefix) {
            (false, false) => OpSize::Bits8,
            (false, true) => return Err(DecoderError::ud("use of operand override prefix with 8-bit op")),   // FIXME check
            (true, false) => OpSize::Bits32,
            (true, true) => OpSize::Bits16,
        })
    }

    /// If `self` contains `other`, removes `other` from `self` and returns
    /// `true`.
    pub fn take(&mut self, other: RawPrefixes) -> bool {
        if self.contains(other) {
            self.remove(other);
            true
        } else {
            false
        }
    }
}

