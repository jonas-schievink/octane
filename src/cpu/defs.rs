//! Enum definitions from the Intel manual.
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

use cpu::instr::OpSize;
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

enum Group {
    /// Immediate Group 1.
    Grp1 {
        dest: OperandSpec,
        src: OperandSpec,
    },
    /// POP group 1A.
    Grp1A {
        operand: OperandSpec,
    },
    /// Shift Group 2.
    Grp2 {
        dest: OperandSpec,
        src: OperandSpec,
    },
}

enum TableEntry {
    Prefix(Prefix),
    Opcode(Opcode),
    /// Special entry handled by decoder (eg. extension/escape byte).
    Special,
    /// The opcode byte denotes a group of opcodes.
    Group(Group),
}

enum Opcode {
    Add {
        dest: OperandSpec,
        src: OperandSpec,
    },
    Adc {
        dest: OperandSpec,
        src: OperandSpec,
    },
    Or {
        dest: OperandSpec,
        src: OperandSpec,
    },
    Sbb {
        dest: OperandSpec,
        src: OperandSpec,
    },
    Sub {
        dest: OperandSpec,
        src: OperandSpec,
    },
    And {
        dest: OperandSpec,
        src: OperandSpec,
    },
    Xor {
        dest: OperandSpec,
        src: OperandSpec,
    },
    Cmp {
        dest: OperandSpec,
        src: OperandSpec,
    },
    Test {
        dest: OperandSpec,
        src: OperandSpec,
    },
    Push {
        operand: OperandSpec,
    },
    Pop {
        /// Destination.
        operand: OperandSpec,
    },
    Inc {
        operand: OperandSpec,
    },
    Dec {
        operand: OperandSpec,
    },
    /*Bound {
        /// Not really a destination - This is the index to check.
        // FIXME consider renaming `dest` to `op1` and `src to `op2` or something
        dest: OperandSpec,
        /// Memory operand containing upper and lower bound.
        src: OperandSpec,
    },*/
    Imul {
        dest: OperandSpec,
        src1: OperandSpec,
        src2: OperandSpec,
    },
    Jo {
        operand: OperandSpec,
    },
    Jno {
        operand: OperandSpec,
    },
    Jb {
        operand: OperandSpec,
    },
    Jae {
        operand: OperandSpec,
    },
    Je {
        operand: OperandSpec,
    },
    Jne {
        operand: OperandSpec,
    },
    Jbe {
        operand: OperandSpec,
    },
    Ja {
        operand: OperandSpec,
    },
    Js {
        operand: OperandSpec,
    },
    Jns {
        operand: OperandSpec,
    },
    Jp {
        operand: OperandSpec,
    },
    Jnp {
        operand: OperandSpec,
    },
    Jl {
        operand: OperandSpec,
    },
    Jge {
        operand: OperandSpec,
    },
    Jle {
        operand: OperandSpec,
    },
    Jg {
        operand: OperandSpec,
    },
    Call {
        operand: OperandSpec,
    },
    Xchg {
        dest: OperandSpec,
        src: OperandSpec,
    },
    Mov {
        dest: OperandSpec,
        src: OperandSpec,
    },
    Lea {
        dest: OperandSpec,
        src: OperandSpec,
    },
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
    /// `ret N`
    RetN {
        operand: OperandSpec,
    },
    /// `ret`
    Ret,
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
macro_rules! method_type_to_opspec {
    ($m:ident/$s:ident) => {
        OperandSpec {
            size: optype_to_default_opsize!($s),
            kind: method_to_opkind!($m),
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
    (group $n:ident: $a1:ident/$o1:ident) => {
        // group instruction with one operand
        TableEntry::Group(Group::$n {
            operand: method_type_to_opspec!($a1/$o1),
        })
    };
    (group $n:ident: $a1:ident/$o1:ident, $a2:ident/$o2:ident) => {
        // group instruction with 2 operands
        TableEntry::Group(Group::$n {
            dest: method_type_to_opspec!($a1/$o1),
            src: method_type_to_opspec!($a2/$o2),
        })
    };
    ($op:ident) => {
        // 0-operand instruction
        TableEntry::Opcode(Opcode::$op)
    };
    ($op:ident: $reg1:ident) => {
        // one-operand instruction with fixed register operand
        TableEntry::Opcode(Opcode::$op {
            operand: OperandSpec {
                size: reg_to_size!($reg1),
                kind: OperandKind::FixedReg(RegisterCode::$reg1),
            }
        })
    };
    ($op:ident: $a1:ident/$o1:ident) => {
        // one-operand instruction
        TableEntry::Opcode(Opcode::$op {
            operand: method_type_to_opspec!($a1/$o1),
        })
    };
    ($op:ident: $a1:ident/$o1:ident, $a2:ident/$o2:ident) => {
        // two-operand instruction
        TableEntry::Opcode(Opcode::$op {
            dest: method_type_to_opspec!($a1/$o1),
            src: method_type_to_opspec!($a2/$o2),
        })
    };
    ($op:ident: $reg1:ident, $a2:ident/$o2:ident) => {
        // two-operand instruction, dest is register
        TableEntry::Opcode(Opcode::$op {
            dest: OperandSpec {
                size: optype_to_default_opsize!($o2),   // FIXME can this use reg_to_size?
                kind: OperandKind::FixedReg(RegisterCode::$reg1),
            },
            src: method_type_to_opspec!($a2/$o2),
        })
    };
    ($op:ident: $a1:ident/$o1:ident, $reg2:ident) => {
        // two-operand instruction, src is register
        TableEntry::Opcode(Opcode::$op {
            dest: method_type_to_opspec!($a1/$o1),
            src: OperandSpec {
                size: optype_to_default_opsize!($o1),   // FIXME can this use reg_to_size?
                kind: OperandKind::FixedReg(RegisterCode::$reg2),
            },
        })
    };
    ($op:ident: $reg1:ident, $reg2:ident) => {
        // two-operand instruction, both registers
        TableEntry::Opcode(Opcode::$op {
            dest: OperandSpec {
                size: reg_to_size!($reg1),
                kind: OperandKind::FixedReg(RegisterCode::$reg1),
            },
            src: OperandSpec {
                size: reg_to_size!($reg2),
                kind: OperandKind::FixedReg(RegisterCode::$reg2),
            },
        })
    };
    ($op:ident: $a1:ident/$o1:ident, $a2:ident/$o2:ident, $a3:ident/$o3:ident) => {
        // three-operand instruction
        TableEntry::Opcode(Opcode::$op {
            dest: method_type_to_opspec!($a1/$o1),
            src1: method_type_to_opspec!($a2/$o2),
            src2: method_type_to_opspec!($a3/$o3),
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
    opcode!(Add: E/b, G/b), // 0x00    8-bit
    opcode!(Add: E/v, G/v), // 0x01    16-/32-bit
    opcode!(Add: G/b, E/b), // 0x02
    opcode!(Add: G/v, E/v), // 0x03
    opcode!(Add: AL,  I/b), // 0x04
    opcode!(Add: eAX, I/z), // 0x05
    opcode!(Push: ES),      // 0x06
    opcode!(Pop: ES),       // 0x07
    opcode!(Or: E/b, G/b),  // 0x08
    opcode!(Or: E/v, G/v),  // 0x09
    opcode!(Or: G/b, E/b),  // 0x0A
    opcode!(Or: G/v, E/v),  // 0x0B
    opcode!(Or: AL,  I/b),  // 0x0C
    opcode!(Or: eAX, I/z),  // 0x0D
    opcode!(Push: CS),      // 0x0E
    special!(),             // 0x0F
    opcode!(Adc: E/b, G/b), // 0x10
    opcode!(Adc: E/v, G/v), // 0x11
    opcode!(Adc: G/b, E/b), // 0x12
    opcode!(Adc: G/v, E/v), // 0x13
    opcode!(Adc: AL,  I/b), // 0x14
    opcode!(Adc: eAX, I/z), // 0x15
    opcode!(Push: SS),      // 0x16
    opcode!(Pop: SS),       // 0x17
    opcode!(Sbb: E/b, G/b), // 0x18
    opcode!(Sbb: E/v, G/v), // 0x19
    opcode!(Sbb: G/b, E/b), // 0x1A
    opcode!(Sbb: G/v, E/v), // 0x1B
    opcode!(Sbb: AL,  I/b), // 0x1C
    opcode!(Sbb: eAX, I/z), // 0x1D
    opcode!(Push: DS),      // 0x1E
    opcode!(Pop: DS),       // 0x1F
    opcode!(And: E/b, G/b), // 0x20
    opcode!(And: E/v, G/v), // 0x21
    opcode!(And: G/b, E/b), // 0x22
    opcode!(And: G/v, E/v), // 0x23
    opcode!(And: AL,  I/b), // 0x24
    opcode!(And: eAX, I/z), // 0x25
    prefix!(OverrideEs),    // 0x26
    opcode!(Daa),           // 0x27
    opcode!(Sub: E/b, G/b), // 0x28
    opcode!(Sub: E/v, G/v), // 0x29
    opcode!(Sub: G/b, E/b), // 0x2A
    opcode!(Sub: G/v, E/v), // 0x2B
    opcode!(Sub: AL,  I/b), // 0x2C
    opcode!(Sub: eAX, I/z), // 0x2D
    prefix!(OverrideCs),    // 0x2E
    opcode!(Das),           // 0x2F
    opcode!(Xor: E/b, G/b), // 0x30
    opcode!(Xor: E/v, G/v), // 0x31
    opcode!(Xor: G/b, E/b), // 0x32
    opcode!(Xor: G/v, E/v), // 0x33
    opcode!(Xor: AL,  I/b), // 0x34
    opcode!(Xor: eAX, I/z), // 0x35
    prefix!(OverrideSs),    // 0x36
    opcode!(Aaa),           // 0x37
    opcode!(Cmp: E/b, G/b), // 0x38
    opcode!(Cmp: E/v, G/v), // 0x39
    opcode!(Cmp: G/b, E/b), // 0x3A
    opcode!(Cmp: G/v, E/v), // 0x3B
    opcode!(Cmp: AL,  I/b), // 0x3C
    opcode!(Cmp: eAX, I/z), // 0x3D
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
    special!(), //opcode!(Bound: G/v, M/a),
    special!(), // ARPL nyi
    prefix!(OverrideFs),    // 0x64
    prefix!(OverrideGs),    // 0x65
    prefix!(OperandSize),   // 0x66
    prefix!(AddressSize),   // 0x67
    opcode!(Push: I/z),     // 0x68
    opcode!(Imul: G/v, E/v, I/z),   // 0x69
    opcode!(Push: I/b),     // 0x6A
    opcode!(Imul: G/v, E/v, I/z),   // 0x6B
    special!(), // INSB nyi (lol)
    special!(), // INSW nyi (lol)
    special!(), // OUTSB nyi (lol)
    special!(), // OUTSW nyi (lol)
    opcode!(Jo: J/b),
    opcode!(Jno: J/b),
    opcode!(Jb: J/b),
    opcode!(Jae: J/b),
    opcode!(Je: J/b),
    opcode!(Jne: J/b),
    opcode!(Jbe: J/b),
    opcode!(Ja: J/b),
    opcode!(Js: J/b),
    opcode!(Jns: J/b),
    opcode!(Jp: J/b),
    opcode!(Jnp: J/b),
    opcode!(Jl: J/b),
    opcode!(Jge: J/b),
    opcode!(Jle: J/b),
    opcode!(Jg: J/b),       // 0x7F
    opcode!(group Grp1: E/b, I/b),  // 0x80
    opcode!(group Grp1: E/v, I/z),  // 0x81
    opcode!(group Grp1: E/b, I/b),  // 0x82
    opcode!(group Grp1: E/v, I/b),  // 0x83
    opcode!(Test: E/b, G/b),    // 0x84
    opcode!(Test: E/v, G/v),    // 0x85
    opcode!(Xchg: E/b, G/b),    // 0x86
    opcode!(Xchg: E/v, G/v),    // 0x87
    opcode!(Mov: E/b, G/b),     // 0x88
    opcode!(Mov: E/v, G/v),     // 0x89
    opcode!(Mov: G/b, E/b),     // 0x8A
    opcode!(Mov: G/v, E/v),     // 0x8B
    opcode!(Mov: E/v, S/w),     // 0x8C
    opcode!(Lea: G/v, E/v),     // 0x8D (technically the src is `M`)
    opcode!(Mov: S/w, E/w),     // 0x8E
    opcode!(group Grp1A: E/v),  // 0x8F
    opcode!(Nop),               // 0x90
    opcode!(Xchg: eCX, eAX),    // 0x91
    opcode!(Xchg: eDX, eAX),    // 0x92
    opcode!(Xchg: eBX, eAX),    // 0x93
    opcode!(Xchg: eSP, eAX),    // 0x94
    opcode!(Xchg: eBP, eAX),    // 0x95
    opcode!(Xchg: eSI, eAX),    // 0x96
    opcode!(Xchg: eDI, eAX),    // 0x97
    opcode!(Cwde),              // 0x98
    opcode!(Cdq),               // 0x99
    opcode!(Call: A/p),         // 0x9A
    opcode!(Fwait),             // 0x9B
    opcode!(Pushfd),            // 0x9C
    opcode!(Popfd),             // 0x9D
    opcode!(Sahf),              // 0x9E
    opcode!(Lahf),              // 0x9F
    opcode!(Mov: AL, O/b),      // 0xA0
    opcode!(Mov: eAX, O/w),     // 0xA1
    opcode!(Mov: O/b, AL),      // 0xA2
    opcode!(Mov: O/w, eAX),     // 0xA3
    special!(), // NYI: movs
    special!(), // NYI: movs
    special!(), // NYI: cmps
    special!(), // NYI: cmps
    opcode!(Test: AL, I/b),     // 0xA8
    opcode!(Test: eAX, I/w),    // 0xA9
    special!(), // NYI: stos
    special!(), // NYI: stos
    special!(), // NYI: lods
    special!(), // NYI: lods
    special!(), // NYI: scas
    special!(), // NYI: scas
    opcode!(Mov: AL, I/b),      // 0xB0
    opcode!(Mov: CL, I/b),
    opcode!(Mov: DL, I/b),
    opcode!(Mov: BL, I/b),
    opcode!(Mov: AH, I/b),
    opcode!(Mov: CH, I/b),
    opcode!(Mov: DH, I/b),
    opcode!(Mov: BH, I/b),      // 0xB7
    opcode!(Mov: eAX, I/w),     // 0xB8
    opcode!(Mov: eCX, I/w),
    opcode!(Mov: eDX, I/w),
    opcode!(Mov: eBX, I/w),
    opcode!(Mov: eAX, I/w),
    opcode!(Mov: eCX, I/w),
    opcode!(Mov: eDX, I/w),
    opcode!(Mov: eBX, I/w),     // 0xBF

    opcode!(group Grp2: E/b, I/b),
    opcode!(group Grp2: E/v, I/v),

    opcode!(RetN: I/w),
    opcode!(Ret),
];

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;

    #[test]
    fn mem_size() {
        assert_eq!(mem::size_of::<TableEntry>(), 8);
    }
}
