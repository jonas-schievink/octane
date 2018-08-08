//! Decoded x86 instruction representation.

use std::fmt;

/// A decoded x86 instruction.
///
/// Note that these decoded instructions do not carry *all* information from the
/// raw machine code stream: For example, shorthand encodings are
/// indistinguishable from their longer forms, and duplicate prefix bytes are
/// dropped. Apart from that, one `Instr` still corresponds to one x86
/// instruction.
///
/// Unless otherwise noted, an `Operand` called `dest` may not be an immediate.
///
/// `Instr` also implements `Display`, which prints a basic disassembly of the
/// instruction. Since no context is available, all jumps and calls will be
/// displayed using the relative address instead of the target address.
#[derive(Debug)]
pub enum Instr {
    /// Perform a binary arithmetic or logic function on `dest` and `src` and
    /// store the result in `dest`.
    ///
    /// Reads and modifies flags according to `op`.
    Alu {
        op: AluOp,
        dest: Operand,
        src: Operand,
    },

    /// Shift-group operation.
    Shift {
        op: ShiftOp,
        /// Shifted value and destination.
        dest: Operand,
        /// 8-bit shift amount.
        src: Operand,
    },

    /// Store `src` in `dest`.
    ///
    /// Does not read or modify any flags.
    Mov {
        dest: Operand,
        src: Operand,
    },

    /// Move with zero-extend. `src` may be smaller than `dest`.
    MovZx {
        dest: Register,
        src: Operand,
    },

    /// Move with sign-extend. `src` may be smaller than `dest`.
    MovSx {
        dest: Register,
        src: Operand,
    },

    /// Branch if the flag specified by `cc` is set to 1.
    ///
    /// If the status flags `cc` is 1, this sets `EIP = target`.
    ///
    /// Reads the flag specified by `cc`. Does not modify any flags.
    JumpIf {
        cc: ConditionCode,
        target: Operand,
    },

    /// Unconditional jump.
    Jump {
        target: Operand,
    },

    /// Set byte to 0 or 1, depending on status flag.
    ///
    /// This is **not** a conditional store - it will always overwrite the
    /// operand.
    SetIf {
        /// Flag to test for.
        cc: ConditionCode,
        operand: Operand,
    },

    /// Call a procedure.
    ///
    /// Pushes `EIP` (after the `call` instruction) onto the stack, decrements
    /// `ESP` by 4, then sets `EIP = target`.
    Call {
        /// Target address. Might be stored in register or memory.
        target: Operand,
    },

    /// Near return to caller.
    ///
    /// First, pops the 32-bit return address `addr` off the stack. Then,
    /// `ESP = ESP + pop` is done in order to remove `pop` bytes of arguments
    /// from the stack (where `pop` may be 0). Finally, sets `EIP = addr`.
    ///
    /// This is an imprecise approximation of a "real" near return `0xC3` in
    /// that it might not check stack limits and assumes a stack address size
    /// of 32 bits. It also assumes no segmentation, like all instructions here.
    Ret {
        pop: u16,
    },

    /// Push a 16- or 32-bit value onto the stack and decrement `ESP`.
    ///
    /// First, `ESP` is decremented by the size of `value` in bytes, rounded up
    /// to 2 or 4 Bytes (16 or 32 bits). Then, `value` is stored to memory at
    /// address `ESP`.
    ///
    /// If `value` is the `ESP` register itself, the value *before* `ESP` is
    /// decremented is pushed onto the stack.
    ///
    /// Does not read or modify any flags.
    Push {
        operand: Operand,
    },

    /// Pop a 16- or 32-bit value from the stack into a register or memory
    /// location.
    ///
    /// First, a value of the right size (2 or 4 bytes) is read from the address
    /// in `ESP`. Then, `ESP` is incremented by that size.
    ///
    /// Does not read or modify any flags.
    Pop {
        operand: Operand,
    },

    /// Load Effective Address.
    ///
    /// Calculate the address specified by `src` (which must not be a register
    /// or immediate operand - only memory locations are supported) and store it
    /// in `dest`.
    ///
    /// Does not read or modify any flags.
    Lea {
        /// May be a 16-bit register, in which case the low 16-bit of the
        /// address are stored.
        dest: Register,
        /// The memory location whose address to compute.
        src: MemoryLocation,
    },

    /// Calculate the bitwise AND of `lhs` and `rhs` and set the flags
    /// accordingly, but do not modify `lhs` or `rhs`.
    Test {
        lhs: Operand,
        rhs: Operand,
    },

    /// Invert every bit in `operand` and store the result back in `operand`.
    ///
    /// `operand` may not be an immediate.
    ///
    /// Does not read or modify any flags.
    Not {
        operand: Operand,
    },

    /// Replace `operand` with its two's complement.
    ///
    /// `operand = 0 - operand`
    ///
    /// Modifies all flags. Does not read any flags.
    Neg {
        operand: Operand,
    },

    /// Unsigned multiplication of `A` register with `operand`.
    ///
    /// Depending on the size of `operand`, multiplies it with `AL`, `AX` or
    /// `EAX`. The result has up to 64 bits and is stored in `AX`, `DX:AX`, or
    /// `EDX:EAX`, respectively.
    ///
    /// Modifies OF and CF (all other flags are undefined). Does not read any
    /// flags.
    Mul {
        operand: Operand,
    },

    /// `imul` with a single operand.
    Imul {
        operand: Operand,
    },

    /// Truncating `imul` with up to 3 operands.
    ///
    /// `dest = src1 * src2`.
    ImulTrunc {
        dest: Register,
        src1: Operand,
        src2: Operand,
    },

    Div {
        operand: Operand,
    },

    Idiv {
        operand: Operand,
    },
    // There's no idiv with more than 1 operand

    Inc {
        operand: Operand,
    },

    Dec {
        operand: Operand,
    },

    /// String memory operation (`ins`, `outs`, `movs`, `lods`, `stos`).
    StrMem {
        /// The operation to perform.
        op: StrMemOp,
        /// Whether a `rep` prefix was present.
        ///
        /// This will perform the operation as many times as specified in
        /// `CX`/`ECX`, decrementing its value each time.
        rep: bool,
        /// The amount of data to move (per iteration). Also specifies the part
        /// of the `a` register to use.
        size: OpSize,
    },

    /// Raise interrupt.
    Int {
        vector: u8,
    },

    /// Determine the bit index of the least or most significant bit set in the
    /// operand.
    BitScan {
        /// If `true`, search for the most significant set bit. If `false`, find
        /// the least significant bit.
        reverse: bool,
        dest: Register,
        src: Operand,
    },

    /// Raise interrupt 4 if overflow flag is set.
    IntO,

    /// Convert Word to Double word.
    ///
    /// Write the most significant bit of `AX` into all of `DX`.
    ///
    /// Does not read or modify any flags.
    Cwd,

    /// Convert Double Word to Quad Word.
    ///
    /// Write the most significant bit of `EAX` into all of `EDX`.
    ///
    /// Does not read or modify any flags.
    Cdq,

    /// Remove the procedure's stack frame.
    ///
    /// Equivalent to:
    ///
    /// ```notrust
    /// mov esp, ebp
    /// pop ebp
    /// ```
    ///
    /// With a 16-bit operand size, the `pop ebp` instead becomes `pop bp`.
    Leave {
        /// Whether to pop `ebp` or `bp` (32-bit / 16-bit operand size).
        size: OpSize,
    },
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use cpu::disasm::print_instr;

        let mut s = String::new();
        print_instr(self, &mut s);
        f.write_str(&s)
    }
}

/// A decoded operand, usually from a Mod-Reg-R/M byte and the following bytes.
///
/// This is mostly equivalent to an addressing mode (along with any data
/// required by the addr. mode).
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Operand {
    /// The operand is the value inside a register, or the register itself.
    Reg(Register),

    /// Immediate value.
    Imm(Immediate),

    /// The operand is stored in memory.
    Mem(MemoryLocation),
}

impl Operand {
    pub fn size(&self) -> OpSize {
        match self {
            Operand::Reg(reg) => reg.size(),
            Operand::Imm(imm) => imm.size(),
            Operand::Mem(mem) => mem.size,
        }
    }
}

impl From<Register> for Operand {
    fn from(reg: Register) -> Self {
        Operand::Reg(reg)
    }
}

impl From<Immediate> for Operand {
    fn from(imm: Immediate) -> Self {
        Operand::Imm(imm)
    }
}

impl From<MemoryLocation> for Operand {
    fn from(mem: MemoryLocation) -> Self {
        Operand::Mem(mem)
    }
}

/// A location in virtual (possibly segmented) memory.
///
/// This represents all addressing modes except register-direct and immediate
/// addressing, which aren't really addressing modes but other kinds of
/// operands.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MemoryLocation {
    /// The operand's size. Since we only store an address, we wouldn't know
    /// this if we didn't store it.
    pub size: OpSize,
    /// The base segment of the operation. In almost all cases, this is the
    /// default segment of the instruction. In rare cases this is overridden
    /// with `FS` to access the Thread Information Block.
    ///
    /// Note that handling of segments, particularly default segments, is broken
    /// in the decoder. This shouldn't be an issue in practice, since segments
    /// are only used to access thread-local information via the TIB.
    pub base_segment: Segment,
    /// The addressing mode used to calculate the memory address.
    pub addressing: Addressing,
}

/// An x86 segment or segment register.
///
/// Note that Windows NT (luckily) uses a flat memory space, which effectively
/// disables segmentation. The only use of the segment registers is thread-local
/// information, which drastically reduces the amount of segmentation we have to
/// emulate.
///
/// For that reason, handling of segments might be buggy or broken in the
/// decoder and elsewhere.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Segment {
    Cs,
    Ds,
    Es,
    /// Used by Windows NT for the [TIB].
    ///
    /// [TIB]: https://en.wikipedia.org/wiki/Win32_Thread_Information_Block
    Fs,
    Gs,
    Ss,
}

impl Segment {
    /// Whether the segment might be used for non-flat addressing.
    ///
    /// Code, data and stack segments (`CS`, `DS`, and `SS`) are used to build a
    /// flat virtual address space. The extra segment `ES` is used by string
    /// instructions, so it also has to describe the same flat address space.
    ///
    /// This leaves only `FS` and `GS` for custom use by the OS, so those
    /// return `true` here.
    pub fn may_be_used(&self) -> bool {
        match self {
            Segment::Fs | Segment::Gs => true,
            _ => false,
        }
    }
}

/// Addressing modes for operands in memory.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Addressing {
    /// Register-indirect addressing with optional displacement.
    ///
    /// The operand is in memory, at the address specified by the sum of the
    /// value stored in the `base` register and the address displacement `disp`.
    ///
    /// x86 has support for 32-bit displacement, 8-bit displacement, and no
    /// displacement at all (the latter saving instruction size). All of these
    /// are collapsed into `Disp` when decoded. It is also possible to use a
    /// "displacement-only" mode without a base register, which is also
    /// represented as this variant.
    Disp {
        /// 32-bit base register. Might be `None` if "displacement-only"
        /// addressing is used.
        base: Option<Register>,
        /// Fixed displacement added to the base register contents.
        disp: i32,
    },

    /// Address calculation using a Scale Index Byte.
    ///
    /// The address is computed using the following formula:
    ///
    /// `base + index * scale + disp`
    ///
    /// Where `base` and `index` represent the value stored inside the `base`
    /// and `index` registers, respectively.
    ///
    /// Note that at least one of `index` and `base` must be specified.
    Sib {
        /// The scale value multiplied with the value of the `index` register.
        ///
        /// 1, 2, 4, or 8.
        scale: u8,
        /// Index register multiplied with `scale`. If `None`, just base
        /// register and displacement are used to calculate the address.
        index: Option<Register>,
        /// Base offset register.
        ///
        /// Might be `None` if `Mod=00` and `Base=101`, which is
        /// "displacement-only" SIB mode.
        base: Option<Register>,
        /// Fixed displacement.
        disp: i32,
    },
}

impl Addressing {
    pub fn absolute(virt_addr: u32) -> Self {
        Addressing::Disp {
            base: None,
            disp: virt_addr as i32,
        }
    }
}

/// Operand or operation size (in 32-bit mode).
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum OpSize {
    Bits8,
    Bits16,
    Bits32,
}

/// An 8, 16 or 32-bit immediate value.
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

    pub fn size(&self) -> OpSize {
        match self {
            Immediate::Imm8(_) => OpSize::Bits8,
            Immediate::Imm16(_) => OpSize::Bits16,
            Immediate::Imm32(_) => OpSize::Bits32,
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

/// An enumeration of all x86 registers with *any* size.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Register {
    // 8, 16, 32 bit GP registers:
    Al,
    Ah,
    Bl,
    Bh,
    Cl,
    Ch,
    Dl,
    Dh,
    Ax,
    Bx,
    Cx,
    Dx,
    Eax,
    Ebx,
    Ecx,
    Edx,

    /// 16-bit Source Register.
    Si,
    Esi,
    /// 16-bit Destination Register.
    Di,
    Edi,
    /// 16-bit Base Pointer.
    Bp,
    Ebp,
    Sp,
    Esp,
}

impl Register {
    pub fn name(&self) -> &'static str {
        use self::Register::*;
        match self {
            Al => "al",
            Ah => "ah",
            Bl => "bl",
            Bh => "bh",
            Cl => "cl",
            Ch => "ch",
            Dl => "dl",
            Dh => "dh",
            Ax => "ax",
            Bx => "bx",
            Cx => "cx",
            Dx => "dx",
            Eax => "eax",
            Ebx => "ebx",
            Ecx => "ecx",
            Edx => "edx",
            Si => "si",
            Esi => "esi",
            Di => "di",
            Edi => "edi",
            Bp => "bp",
            Ebp => "ebp",
            Sp => "sp",
            Esp => "esp",
        }
    }

    pub fn size(&self) -> OpSize {
        use self::Register::*;
        match self {
            Al | Ah | Bl | Bh | Cl | Ch | Dl | Dh => OpSize::Bits8,
            Ax | Bx | Cx | Dx | Si | Di | Bp | Sp => OpSize::Bits16,
            Eax | Ebx | Ecx | Edx | Esi | Edi | Ebp | Esp => OpSize::Bits32,
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// The opcode/kind of an instruction, without operands or prefixes.
///
/// This can be derived from the instruction's opcode byte(s) and serves to
/// group instructions with similar behaviour together.
#[derive(Debug)]
pub enum Opcode {
    Alu(AluOp),
    Push,
    Pop,
    Shld,
    Mov,
    /// Move with zero-extend.
    Movzx,
    /// Move with sign-extend.
    Movsy,
}

/// Specifies the operation to perform for ALU instructions.
///
/// All of these share a common encoding (but might have a shorter aliased
/// encoding in some cases):
///
/// ```notrust
/// +-----------------+
/// | 0 0 A A A 0 D S |
/// +-----------------+
/// ```
///
/// (but note that instructions that take an immediate are encoded differently)
///
/// Where:
///
/// * `A`: Value of `AluOp` (3-bit ALU operation).
/// * `D`: Direction bit in ModR/M byte (0 = Add Reg to R/M field, 1 = Add R/M
///   field to Reg field).
/// * `S`: Size bit (0 = 8-bit operands, 1 = 16- or 32-bit operands) - sometimes
///   called `W` bit (width).
/// * `0`: Fixed 0 bit for ALU opcode.
///
/// Since we're always in 32-bit protected mode, choosing `S=1` will result in
/// 32-bit operands. 16-bit operands can still be selected by using an
/// operand-size prefix byte (`0x66`) before the opcode.
#[derive(Debug, FromPrimitive)]
pub enum AluOp {
    Add = 0,
    Or = 1,
    /// Add with carry.
    Adc = 2,
    /// Subtract with borrow.
    Sbb = 3,
    And = 4,
    Sub = 5,
    Xor = 6,
    /// Set `EFLAGS` like `sub` would, but do not change `dest`.
    Cmp = 7,
}

/// Shift instruction group opcode.
///
/// Stored in the `Reg` field of the Mod-Reg-R/M byte.
#[derive(Debug, FromPrimitive)]
pub enum ShiftOp {
    Rol = 0,
    Ror = 1,
    Rcl = 2,
    Rcr = 3,
    Shl = 4,
    Shr = 5,
    /// Same as `Shl`.
    Sal = 6,
    Sar = 7,
}

/// A condition for conditional branches or `cmov`.
///
/// Note that these have a lot of alternate names.
///
/// The discriminant is set according to the x86 instruction encoding. Condition
/// codes are encoded in 4 bits.
#[derive(Debug, Copy, Clone, FromPrimitive)]
#[repr(u8)]
pub enum ConditionCode {
    /// CF=0 and ZF=0
    Above = 0x7,
    /// CF=0
    ///
    /// Aka "above or equal".
    NotCarry = 0x3,
    /// CF=1
    ///
    /// Aka "below".
    Carry = 0x2,
    /// CF=1 or ZF=1
    BelowOrEqual = 0x6,
    /// ZF=1
    Equal = 0x4,
    /// ZF=0 and SF=OF
    Greater = 0xF,
    /// SF=OF
    GreaterOrEqual = 0xD,
    /// SF!=OF
    Less = 0xC,
    /// ZF=1 or SF!=OF
    LessOrEqual = 0xE,
    /// ZF=0
    ///
    /// Aka "not zero".
    NotEqual = 0x5,
    /// OF=0
    NotOverflow = 0x1,
    /// PF=0
    ///
    /// Aka "parity odd".
    NotParity = 0xB,
    /// SF=0
    NotSign = 0x9,
    /// OF=1
    Overflow = 0x0,
    /// PF=1
    ///
    /// Aka "parity even".
    Parity = 0xA,
    /// SF=1
    Sign = 0x8,
}

/// "String" operation for data movement (more like batch memory operation).
#[derive(Debug, Clone)]
pub enum StrMemOp {
    /// Input string from port.
    Ins(u16),
    /// Output string to port.
    Outs(u16),
    /// Move string within memory.
    ///
    /// The source address is defined as `ds:(e)si`, the destination as
    /// `es:(e)si`. A segment prefix overrides only the `ds` segment.
    ///
    /// After a byte, word or dword is moved, both `(e)si` and `(e)di` are
    /// incremented by the number of bytes copied if the `DF` flag is 0, and
    /// decremented if it is 1.
    Movs,
    /// Load string from memory into `al`/`ax`/`eax`.
    ///
    /// Always uses the `ds` segment for memory access. Segment override
    /// prefixes are not allowed.
    ///
    /// After a value is stored, `di`/`edi` is incremented by the number of
    /// bytes stored if the `DF` flag is 0, and decremented if it's 1.
    Lods,
    /// Store string data from `al`/`ax`/`eax` to memory at `es:edi` or `es:di`.
    ///
    /// Always uses the `es` segment for memory access. Segment override
    /// prefixes are not allowed.
    ///
    /// After a value is stored, `di`/`edi` is incremented by the number of
    /// bytes stored if the `DF` flag is 0, and decremented if it's 1.
    Stos,
}

/*

later idea, after eflags data flow is ready:
warn as soon as code relies on flags being preserved across a `call`
...or on any flag specified as undefined at that point

*/
