//! Enum definitions from the Intel manual (currently unused).
//!
//! Most of the documentation is straight from Intel's own manual:
//!
//! https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf
//!
//! The way to encode the opcode maps is also inspired by the [panopticon]
//! project.
//!
//! [panopticon]: https://github.com/das-labor/panopticon/blob/33ffec0d6d379d51b38d6ea00d040f54b1356ae4/amd64/src/tables.rs

#![allow(unused)]   // TODO remove

use cpu::instr::{ConditionCode, OpSize, AluOp};
use cpu::instr::ConditionCode::*;
use cpu::instr::AluOp::*;
use cpu::prefix::Prefix;

/// Addressing methods.
///
/// According to "A.2.1 Codes for Addressing Method".
#[repr(u8)]
pub enum AddrMethod {
    /// Direct address: the instruction has no ModR/M byte; the address of the
    /// operand is encoded in the instruction. No base register, index register,
    /// or scaling factor can be applied (for example, far JMP (EA)).
    A,
    /// The VEX.vvvv field of the VEX prefix selects a general purpose
    /// register.
    B,
    /// The reg field of the ModR/M byte selects a control register (for
    /// example, MOV (0F20, 0F22)).
    C,
    /// The reg field of the ModR/M byte selects a debug register (for example,
    /// MOV (0F21,0F23)).
    D,
    /// A ModR/M byte follows the opcode and specifies the operand. The operand
    /// is either a general-purpose register or a memory address. If it is a
    /// memory address, the address is computed from a segment register and any
    /// of the following values: a base register, an index register, a scaling
    /// factor, a displacement.
    ///
    /// `r/m`
    E,
    /// EFLAGS/RFLAGS Register.
    F,
    /// The reg field of the ModR/M byte selects a general register (for
    /// example, AX (000)).
    ///
    /// `r`
    G,
    /// The VEX.vvvv field of the VEX prefix selects a 128-bit XMM register or a
    /// 256-bit YMM register, determined by operand type. For legacy SSE
    /// encodings this operand does not exist, changing the instruction to
    /// destructive form.
    H,
    /// Immediate data: the operand value is encoded in subsequent bytes of the
    /// instruction.
    I,
    /// The instruction contains a relative offset to be added to the
    /// instruction pointer register (for example, JMP (0E9), LOOP).
    J,
    //K,
    /// The upper 4 bits of the 8-bit immediate selects a 128-bit XMM register
    /// or a 256-bit YMM register, determined by operand type. (the MSB is
    /// ignored in 32-bit mode)
    L,
    /// The ModR/M byte may refer only to memory (for example, BOUND, LES, LDS,
    /// LSS, LFS, LGS, CMPXCHG8B).
    M,
    /// The R/M field of the ModR/M byte selects a packed-quadword, MMX
    /// technology register.
    N,
    /// The instruction has no ModR/M byte. The offset of the operand is coded
    /// as a word or double word (depending on address size attribute) in the
    /// instruction. No base register, index register, or scaling factor can be
    /// applied (for example, MOV (A0–A3)).
    O,
    /// The reg field of the ModR/M byte selects a packed quadword MMX
    /// technology register.
    P,
    /// A ModR/M byte follows the opcode and specifies the operand. The operand
    /// is either an MMX technology register or a memory address. If it is a
    /// memory address, the address is computed from a segment register and any
    /// of the following values: a base register, an index register, a scaling
    /// factor, and a displacement.
    Q,
    /// The R/M field of the ModR/M byte may refer only to a general register
    /// (for example, MOV (0F20-0F23)).
    R,
    /// The reg field of the ModR/M byte selects a segment register (for
    /// example, MOV (8C,8E)).
    S,
    //T,
    /// The R/M field of the ModR/M byte selects a 128-bit XMM register or a
    /// 256-bit YMM register, determined by operand type.
    U,
    /// The reg field of the ModR/M byte selects a 128-bit XMM register or a
    /// 256-bit YMM register, determined by operand type.
    V,
    /// A ModR/M byte follows the opcode and specifies the operand. The operand
    /// is either a 128-bit XMM register, a 256-bit YMM register (determined by
    /// operand type), or a memory address. If it is a memory address, the
    /// address is computed from a segment register and any of the following
    /// values: a base register, an index register, a scaling factor, and a
    /// displacement.
    W,
    /// Memory addressed by the DS:rSI register pair (for example, MOVS, CMPS,
    /// OUTS, or LODS).
    X,
    /// Memory addressed by the ES:rDI register pair (for example, MOVS, CMPS,
    /// INS, STOS, or SCAS).
    Y,
}

/// According to "A.2.2 Codes for Operand Type".
#[repr(u8)]
#[allow(bad_style)]
pub enum OperandType {
    /// Two one-word operands in memory or two double-word operands in memory,
    /// depending on operand-size attribute (used only by the BOUND
    /// instruction).
    a,
    /// Byte, regardless of operand-size attribute.
    b,
    /// Byte or word, depending on operand-size attribute.
    c,
    /// Doubleword, regardless of operand-size attribute
    d,
    /// Double-quadword, regardless of operand-size attribute.
    dq,
    /// 32-bit, 48-bit, or 80-bit pointer, depending on operand-size attribute.
    p,
    /// 128-bit or 256-bit packed double-precision floating-point data.
    pd,
    /// Quadword MMX technology register (for example: mm0).
    pi,
    /// 128-bit or 256-bit packed single-precision floating-point data.
    ps,
    /// Quadword, regardless of operand-size attribute.
    q,
    /// Quad-Quadword (256-bits), regardless of operand-size attribute.
    qq,
    /// 6-byte or 10-byte pseudo-descriptor
    s,
    /// Scalar element of a 128-bit double-precision floating data.
    sd,
    /// Scalar element of a 128-bit single-precision floating data.
    ss,
    /// Doubleword integer register (for example: eax).
    si,
    /// Word, doubleword or quadword (in 64-bit mode), depending on operand-size
    /// attribute.
    v,
    /// Word, regardless of operand-size attribute.
    w,
    /// dq or qq based on the operand-size attribute.
    x,
    /// Doubleword or quadword (in 64-bit mode), depending on operand-size
    /// attribute.
    y,
    /// Word for 16-bit operand-size or doubleword for 32 or 64-bit
    /// operand-size.
    z,
}

/// Refers to either a fixed register, or a 16- or 32-bit register depending on
/// the presence of an operand override prefix.
#[allow(bad_style)]
pub enum RegisterCode {
    AL,
    AH,
    eAX,
    BL,
    BH,
    eBX,
    CL,
    CH,
    eCX,
    DL,
    DH,
    eDX,
    eSP,
    eBP,
    eSI,
    eDI,
    ES,
    SS,
    CS,
    DS,
}

enum TableEntry {
    /// The byte is a prefix byte, not an instruction opcode.
    Prefix(Prefix),
    /// Instruction or instruction group descriptor.
    InstrDef(InstrDef),
    /// Special entry handled by decoder (eg. extension/escape byte).
    Special,
}

enum InstrDef {
    Arity0 {
        op: Arity0Op,
    },
    Arity1 {
        op: Arity1Op,
        operand: OperandSpec,
    },
    Arity2 {
        op: Arity2Op,
        operand1: OperandSpec,
        operand2: OperandSpec,
    },
    Arity3 {
        op: Arity3Op,
        operand1: OperandSpec,
        operand2: OperandSpec,
        operand3: OperandSpec,
    },
}

enum Arity0Op {
    Daa,
    Das,
    Aaa,
    Aas,
    Pusha,
    Popa,
    Nop,
    /// Or `CBW` with operand prefix.
    Cwde,
    /// Or `CWD` with operand prefix.
    Cdq,
    Fwait,
    /// Push EFLAGS
    Pushfd,
    Popfd,
    /// Copy AH into the low byte of the EFLAGS register.
    Sahf,
    /// Copy the low byte of the EFLAGS register into AH.
    Lahf,
    /// `ret`
    Ret,
}

enum Arity1Op {
    Push,
    Pop,
    Inc,
    Dec,
    Jcc {
        cc: ConditionCode,
    },
    Call,
    RetN,
    /// POP group 1A.
    Grp1A,
}

enum Arity2Op {
    Alu {
        op: AluOp,
    },
    Test,
    Xchg,
    Mov,
    Lea,
    /// Immediate Group 1.
    Grp1,
    /// Shift Group 2.
    Grp2,
}

enum Arity3Op {
    Imul,
}

struct OperandSpec {
    size: OpSize,
    kind: OperandKind,
}

/// Specifies how an instruction's operand is encoded.
enum OperandKind {
    /// `rN` - The operand is an `N`-bit register stored in the Mod-Reg-R/M
    /// byte's `Reg` field.
    Reg,
    /// The operand is a segment register stored in the `Reg` field of the
    /// Mod-Reg-R/M byte.
    SegmentReg,
    /// `r/mN` - The operand is an `N`-bit register or memory location stored in
    /// the Mod-Reg-R/M byte's `Mod` and `R/M` fields (SIB addressing is
    /// possible).
    ModRM,
    /*/// `mN` - The operand is in memory. This is the same as `ModRM`, except
    /// that only memory locations are accepted, no registers.
    MemRM,*/
    /// An immediate value follows the instruction.
    Imm,
    /// Signed 8-bit offset.
    Off8,
    /// Absolute 32-bit jump/call target or operand address.
    Abs32,
    /// The instruction uses a fixed register as the operand.
    ///
    /// Which specific register depends on the operand size.
    FixedReg(RegisterCode),
}

/// Converts an addressing method and operand type to an operand spec.
macro_rules! parse_opspec {
    ($m:ident $s:ident) => {
        OperandSpec {
            size: optype_to_default_opsize!($s),
            kind: method_to_opkind!($m),
        }
    };
    ($reg:ident) => {
        OperandSpec {
            size: reg_to_size!($reg),
            kind: OperandKind::FixedReg(RegisterCode::$reg),
        }
    };
}

macro_rules! method_to_opkind {
    (E) => {OperandKind::ModRM};
    (G) => {OperandKind::Reg};
    (I) => {OperandKind::Imm};
    (J) => {OperandKind::Off8};
    (A) => {OperandKind::Abs32};
    (S) => {OperandKind::SegmentReg};
    (O) => {OperandKind::Abs32};
    //(M) => {OperandKind::MemRM};
}

/// Converts an operand type symbol to the default operand size.
///
/// If the result is 32-bit, it can be changed to 16-bit using an operand
/// override prefix.
macro_rules! optype_to_default_opsize {
    (b) => {OpSize::Bits8};
    (w) => {OpSize::Bits16};
    (p) => {OpSize::Bits32};    // Fixed!
    (v) => {OpSize::Bits32};
    (z) => {OpSize::Bits32};    // v and z are equivalent in 32-bit mode
}

/// Returns the `OpSize` of a fixed register operand.
///
/// The register may be one of the `e_X` variants, in which case this returns
/// `Bits32` - this can be changed to the 16-bit counterpart by using an operand
/// override prefix, though.
macro_rules! reg_to_size {
    (AL) => {OpSize::Bits8};
    (AH) => {OpSize::Bits8};
    (BL) => {OpSize::Bits8};
    (BH) => {OpSize::Bits8};
    (CL) => {OpSize::Bits8};
    (CH) => {OpSize::Bits8};
    (DL) => {OpSize::Bits8};
    (DH) => {OpSize::Bits8};
    (ES) => {OpSize::Bits16};
    (SS) => {OpSize::Bits16};
    (CS) => {OpSize::Bits16};
    (DS) => {OpSize::Bits16};
    (eAX) => {OpSize::Bits32};
    (eBX) => {OpSize::Bits32};
    (eCX) => {OpSize::Bits32};
    (eDX) => {OpSize::Bits32};
    (eSP) => {OpSize::Bits32};
    (eBP) => {OpSize::Bits32};
    (eSI) => {OpSize::Bits32};
    (eDI) => {OpSize::Bits32};
}

macro_rules! opcode {
    ($op:ident) => {
        // 0-operand instruction
        TableEntry::InstrDef(InstrDef::Arity0 {
            op: Arity0Op::$op,
        })
    };
    ($op:ident: $($operand:ident)+) => {
        TableEntry::InstrDef(InstrDef::Arity1 {
            op: Arity1Op::$op,
            operand: parse_opspec!($($operand)+),
        })
    };
    ($op:ident: $($operand:ident)+ => { $($field:ident: $val:expr),+ }) => {
        TableEntry::InstrDef(InstrDef::Arity1 {
            op: Arity1Op::$op { $($field: $val),+ },
            operand: parse_opspec!($($operand)+),
        })
    };
    ($op:ident: $($operand1:ident)+, $($operand2:ident)+) => {
        TableEntry::InstrDef(InstrDef::Arity2 {
            op: Arity2Op::$op,
            operand1: parse_opspec!($($operand1)+),
            operand2: parse_opspec!($($operand2)+),
        })
    };
    ($op:ident: $($operand1:ident)+, $($operand2:ident)+ => { $($field:ident: $val:expr),+ }) => {
        TableEntry::InstrDef(InstrDef::Arity2 {
            op: Arity2Op::$op { $($field: $val),+ },
            operand1: parse_opspec!($($operand1)+),
            operand2: parse_opspec!($($operand2)+),
        })
    };
    ($op:ident: $($operand1:ident)+, $($operand2:ident)+, $($operand3:ident)+) => {
        TableEntry::InstrDef(InstrDef::Arity3 {
            op: Arity3Op::$op,
            operand1: parse_opspec!($($operand1)+),
            operand2: parse_opspec!($($operand2)+),
            operand3: parse_opspec!($($operand3)+),
        })
    };
}

macro_rules! prefix {
    ($prefix:ident) => {
        TableEntry::Prefix(Prefix::$prefix)
    };
}

macro_rules! special {
    () => {
        TableEntry::Special
    };
}

/// From "Table A-2. One-byte Opcode Map" (actually 2 tables with an odd split,
/// watch out!).
static ONE_BYTE_MAP: &[TableEntry] = &[
    opcode!(Alu: E b, G b => { op: Add }), // 0x00    8-bit
    opcode!(Alu: E v, G v => { op: Add }), // 0x01    16-/32-bit
    opcode!(Alu: G b, E b => { op: Add }), // 0x02
    opcode!(Alu: G v, E v => { op: Add }), // 0x03
    opcode!(Alu: AL,  I b => { op: Add }), // 0x04
    opcode!(Alu: eAX, I z => { op: Add }), // 0x05
    opcode!(Push: ES),      // 0x06
    opcode!(Pop: ES),       // 0x07
    opcode!(Alu: E b, G b => { op: Or }),  // 0x08
    opcode!(Alu: E v, G v => { op: Or }),  // 0x09
    opcode!(Alu: G b, E b => { op: Or }),  // 0x0A
    opcode!(Alu: G v, E v => { op: Or }),  // 0x0B
    opcode!(Alu: AL,  I b => { op: Or }),  // 0x0C
    opcode!(Alu: eAX, I z => { op: Or }),  // 0x0D
    opcode!(Push: CS),      // 0x0E
    special!(),             // 0x0F
    opcode!(Alu: E b, G b => { op: Adc }), // 0x10
    opcode!(Alu: E v, G v => { op: Adc }), // 0x11
    opcode!(Alu: G b, E b => { op: Adc }), // 0x12
    opcode!(Alu: G v, E v => { op: Adc }), // 0x13
    opcode!(Alu: AL,  I b => { op: Adc }), // 0x14
    opcode!(Alu: eAX, I z => { op: Adc }), // 0x15
    opcode!(Push: SS),      // 0x16
    opcode!(Pop: SS),       // 0x17
    opcode!(Alu: E b, G b => { op: Sbb }), // 0x18
    opcode!(Alu: E v, G v => { op: Sbb }), // 0x19
    opcode!(Alu: G b, E b => { op: Sbb }), // 0x1A
    opcode!(Alu: G v, E v => { op: Sbb }), // 0x1B
    opcode!(Alu: AL,  I b => { op: Sbb }), // 0x1C
    opcode!(Alu: eAX, I z => { op: Sbb }), // 0x1D
    opcode!(Push: DS),      // 0x1E
    opcode!(Pop: DS),       // 0x1F
    opcode!(Alu: E b, G b => { op: And }), // 0x20
    opcode!(Alu: E v, G v => { op: And }), // 0x21
    opcode!(Alu: G b, E b => { op: And }), // 0x22
    opcode!(Alu: G v, E v => { op: And }), // 0x23
    opcode!(Alu: AL,  I b => { op: And }), // 0x24
    opcode!(Alu: eAX, I z => { op: And }), // 0x25
    prefix!(OverrideEs),    // 0x26
    opcode!(Daa),           // 0x27
    opcode!(Alu: E b, G b => { op: Sub }), // 0x28
    opcode!(Alu: E v, G v => { op: Sub }), // 0x29
    opcode!(Alu: G b, E b => { op: Sub }), // 0x2A
    opcode!(Alu: G v, E v => { op: Sub }), // 0x2B
    opcode!(Alu: AL,  I b => { op: Sub }), // 0x2C
    opcode!(Alu: eAX, I z => { op: Sub }), // 0x2D
    prefix!(OverrideCs),    // 0x2E
    opcode!(Das),           // 0x2F
    opcode!(Alu: E b, G b => { op: Xor }), // 0x30
    opcode!(Alu: E v, G v => { op: Xor }), // 0x31
    opcode!(Alu: G b, E b => { op: Xor }), // 0x32
    opcode!(Alu: G v, E v => { op: Xor }), // 0x33
    opcode!(Alu: AL,  I b => { op: Xor }), // 0x34
    opcode!(Alu: eAX, I z => { op: Xor }), // 0x35
    prefix!(OverrideSs),    // 0x36
    opcode!(Aaa),           // 0x37
    opcode!(Alu: E b, G b => { op: Cmp }), // 0x38
    opcode!(Alu: E v, G v => { op: Cmp }), // 0x39
    opcode!(Alu: G b, E b => { op: Cmp }), // 0x3A
    opcode!(Alu: G v, E v => { op: Cmp }), // 0x3B
    opcode!(Alu: AL,  I b => { op: Cmp }), // 0x3C
    opcode!(Alu: eAX, I z => { op: Cmp }), // 0x3D
    prefix!(OverrideDs),    // 0x3E
    opcode!(Aas),           // 0x3F
    opcode!(Inc: eAX),      // 0x40
    opcode!(Inc: eCX),      // 0x41
    opcode!(Inc: eDX),      // 0x42
    opcode!(Inc: eBX),      // 0x43
    opcode!(Inc: eSP),      // 0x44
    opcode!(Inc: eBP),      // 0x45
    opcode!(Inc: eSI),      // 0x46
    opcode!(Inc: eDI),      // 0x47
    opcode!(Dec: eAX),      // 0x48
    opcode!(Dec: eCX),      // 0x49
    opcode!(Dec: eDX),      // 0x4A
    opcode!(Dec: eBX),      // 0x4B
    opcode!(Dec: eSP),      // 0x4C
    opcode!(Dec: eBP),      // 0x4D
    opcode!(Dec: eSI),      // 0x4E
    opcode!(Dec: eDI),      // 0x4F
    opcode!(Push: eAX),     // 0x50
    opcode!(Push: eCX),     // 0x51
    opcode!(Push: eDX),     // 0x52
    opcode!(Push: eBX),     // 0x53
    opcode!(Push: eSP),     // 0x54
    opcode!(Push: eBP),     // 0x55
    opcode!(Push: eSI),     // 0x56
    opcode!(Push: eDI),     // 0x57
    opcode!(Pop: eAX),      // 0x58
    opcode!(Pop: eCX),      // 0x59
    opcode!(Pop: eDX),      // 0x5A
    opcode!(Pop: eBX),      // 0x5B
    opcode!(Pop: eSP),      // 0x5C
    opcode!(Pop: eBP),      // 0x5D
    opcode!(Pop: eSI),      // 0x5E
    opcode!(Pop: eDI),      // 0x5F
    opcode!(Pusha),         // 0x60
    opcode!(Popa),          // 0x61
    special!(), // bound
    special!(), // ARPL nyi
    prefix!(OverrideFs),    // 0x64
    prefix!(OverrideGs),    // 0x65
    prefix!(OperandSize),   // 0x66
    prefix!(AddressSize),   // 0x67
    opcode!(Push: I z),     // 0x68
    opcode!(Imul: G v, E v, I z),   // 0x69
    opcode!(Push: I b),     // 0x6A
    opcode!(Imul: G v, E v, I z),   // 0x6B
    special!(), // INSB nyi (lol)
    special!(), // INSW nyi (lol)
    special!(), // OUTSB nyi (lol)
    special!(), // OUTSW nyi (lol)
    opcode!(Jcc: J b => { cc: Overflow }),
    opcode!(Jcc: J b => { cc: NotOverflow }),
    opcode!(Jcc: J b => { cc: Carry }),     // Below
    opcode!(Jcc: J b => { cc: NotCarry }),  // Above or Equal
    opcode!(Jcc: J b => { cc: Equal }),
    opcode!(Jcc: J b => { cc: NotEqual }),
    opcode!(Jcc: J b => { cc: BelowOrEqual }),
    opcode!(Jcc: J b => { cc: Above }),
    opcode!(Jcc: J b => { cc: Sign }),
    opcode!(Jcc: J b => { cc: NotSign }),
    opcode!(Jcc: J b => { cc: Parity }),
    opcode!(Jcc: J b => { cc: NotParity }),
    opcode!(Jcc: J b => { cc: Less }),
    opcode!(Jcc: J b => { cc: GreaterOrEqual }),
    opcode!(Jcc: J b => { cc: LessOrEqual }),
    opcode!(Jcc: J b => { cc: Greater }),
    opcode!(Grp1: E b, I b),  // 0x80 - immediate group 1
    opcode!(Grp1: E v, I z),  // 0x81
    opcode!(Grp1: E b, I b),  // 0x82
    opcode!(Grp1: E v, I b),  // 0x83
    opcode!(Test: E b, G b),    // 0x84
    opcode!(Test: E v, G v),    // 0x85
    opcode!(Xchg: E b, G b),    // 0x86
    opcode!(Xchg: E v, G v),    // 0x87
    opcode!(Mov : E b, G b),     // 0x88
    opcode!(Mov : E v, G v),     // 0x89
    opcode!(Mov : G b, E b),     // 0x8A
    opcode!(Mov : G v, E v),     // 0x8B
    opcode!(Mov : E v, S w),     // 0x8C
    opcode!(Lea : G v, E v),     // 0x8D (technically the src is `M`)
    opcode!(Mov : S w, E w),     // 0x8E
    opcode!(Grp1A: E v),        // 0x8F - group 1A pop
    opcode!(Nop),               // 0x90
    opcode!(Xchg: eCX, eAX),    // 0x91
    opcode!(Xchg: eDX, eAX),    // 0x92
    opcode!(Xchg: eBX, eAX),    // 0x93
    opcode!(Xchg: eSP, eAX),    // 0x94
    opcode!(Xchg: eBP, eAX),    // 0x95
    opcode!(Xchg: eSI, eAX),    // 0x96
    opcode!(Xchg: eDI, eAX),    // 0x97
    opcode!(Cwde),              // 0x98 (cbw with opsize prefix)
    opcode!(Cdq),               // 0x99 (cwd with opsize prefix)
    opcode!(Call: A p),         // 0x9A
    opcode!(Fwait),             // 0x9B
    opcode!(Pushfd),            // 0x9C
    opcode!(Popfd),             // 0x9D
    opcode!(Sahf),              // 0x9E
    opcode!(Lahf),              // 0x9F
    opcode!(Mov: AL, O b),      // 0xA0
    opcode!(Mov: eAX, O w),     // 0xA1
    opcode!(Mov: O b, AL),      // 0xA2
    opcode!(Mov: O w, eAX),     // 0xA3
    special!(), // NYI: movs
    special!(), // NYI: movs
    special!(), // NYI: cmps
    special!(), // NYI: cmps
    opcode!(Test: AL, I b),     // 0xA8
    opcode!(Test: eAX, I w),    // 0xA9
    //opcode!(Stos: Y/b, AL),
    special!(), // NYI: stos
    special!(), // NYI: stos
    special!(), // NYI: lods
    special!(), // NYI: lods
    special!(), // NYI: scas
    special!(), // NYI: scas
    opcode!(Mov: AL, I b),      // 0xB0
    opcode!(Mov: CL, I b),
    opcode!(Mov: DL, I b),
    opcode!(Mov: BL, I b),
    opcode!(Mov: AH, I b),
    opcode!(Mov: CH, I b),
    opcode!(Mov: DH, I b),
    opcode!(Mov: BH, I b),      // 0xB7
    opcode!(Mov: eAX, I w),     // 0xB8
    opcode!(Mov: eCX, I w),
    opcode!(Mov: eDX, I w),
    opcode!(Mov: eBX, I w),
    opcode!(Mov: eAX, I w),
    opcode!(Mov: eCX, I w),
    opcode!(Mov: eDX, I w),
    opcode!(Mov: eBX, I w),     // 0xBF

    opcode!(Grp2: E b, I b),
    opcode!(Grp2: E v, I v),

    opcode!(RetN: I w),
    opcode!(Ret),
];

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;

    #[test]
    fn mem_size() {
        assert_eq!(mem::size_of::<TableEntry>(), 8);    // 8 Bytes = 2KB for whole table
    }
}
