//! x86 status flag (`EFLAGS`) structures and utilities.
//!
//! Note that we don't model all flags in the register. The important ones are
//! those read and written by instructions.

bitflags! {
    /// A bitmask carrying any combination of x86 status flags.
    ///
    /// This is used, for example, to determine the flags that are read or
    /// written by an instruction, or to store the current `EFLAGS` value of the
    /// CPU.
    pub struct FlagSet: u32 {
        /// Carry flag.
        const CF = 1 << 0;
        /// Parity flag.
        const PF = 1 << 2;
        /// Adjust flag (or Auxiliary Carry Flag).
        ///
        /// Set to whether a carry from the lower 4 bits of an operation has
        /// been generated.
        const AF = 1 << 4;
        /// Zero flag.
        ///
        /// Set to whether the result of an operation consists entirely of
        /// 0-bits.
        const ZF = 1 << 6;
        /// Sign flag.
        const SF = 1 << 7;
        /// Overflow.
        const OF = 1 << 11;

        // Currently, the indices correspond to the bit indices in EFLAGS, but
        // we could pack this into a `u8` instead.
    }
}

// prototypal table-based decoder flags, doesn't really belong in here but w/e
bitflags! {
    struct OpFlags: u16 {
        /// Whether this is a prefix byte (and which one), sets one of the 11
        /// prefix bits if it's not 0b1111.
        const PREFIX_SET    = 0b1111_0000_0000_0000;
        /// Instruction size (number of bytes to skip after this byte) not
        /// including prefixes.
        ///
        /// There's a hard limit of 15 Bytes including prefixes for any
        /// instruction. If exceeded, `#UD` is generated.
        const SIZE          = 0b0000_1111_0000_0000;
        /// Whether the instruction can change control flow in any way (cond. or
        /// uncond. branch, call, ret, etc.).
        ///
        /// Basically this is set whenever there's a possibility that executing
        /// the op results in `eip` not pointing at the next instr. (excluding
        /// exceptions).
        const SPECIAL_CFLOW = 0b0000_0000_1000_0000;

        /// 0, 1, 2 or 3 operands.
        const OPERAND_COUNT = 0b0000_0000_0110_0000;
    }
}
