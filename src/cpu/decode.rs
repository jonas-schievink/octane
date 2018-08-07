//! x86 instruction decoder.

use cpu::instr::*;
use cpu::prefix::{Prefixes, PrefixFlags};
use memory::{VirtualMemory, MemoryError};

use num_traits::FromPrimitive;

/// x86 machine instruction decoder.
#[derive(Debug)]
pub struct Decoder<'a, M: VirtualMemory + 'a> {
    /// Virtual address of the next byte that will be loaded from memory.
    pos: u32,
    /// Length of the currently decoded instruction.
    len: u32,
    prefixes: Prefixes,
    mem: &'a M,
}

impl<'a, M: VirtualMemory> Decoder<'a, M> {
    /// Creates a new instruction decoder.
    ///
    /// # Parameters
    ///
    /// * `mem`: The virtual memory to read from.
    /// * `pc`: The virtual address at which to start decoding.
    pub fn new(mem: &'a M, pc: u32) -> Self {
        Self {
            pos: pc,
            len: 0,
            prefixes: Prefixes::empty(),
            mem,
        }
    }

    /// Returns the value of the program counter, the virtual address of the
    /// first byte of the next instruction we're going to decode.
    ///
    /// This is incremented as `decode_next` is called.
    pub fn current_address(&self) -> u32 {
        self.pos
    }

    /// Read and decode the next instruction in the stream.
    ///
    /// If this returns an error, the decoder's address most likely points into
    /// the middle of the instruction and the decoder should not be used for
    /// further instruction decoding.
    pub fn decode_next(&mut self) -> Result<Instr, DecoderError> {
        self.len = 0;
        self.prefixes = Prefixes::empty();

        let mut byte = self.read()?;

        // Collect prefix bytes
        loop {
            match self.prefixes.decode(byte) {
                Ok(new) => new,
                Err(_) => break,
            };
            byte = self.read()?;
        }

        // Many instrs look like this: X X X X X X D S
        // Pull out D and S bits for convenience
        let default_dir_bit  = (byte & 0b10) >> 1 != 0; // false = Reg to R/M, true = R/M to Reg
        let default_size_bit = (byte & 0b01) != 0;      // false = 8 bit, true = 16/32 bit

        let instr = match byte {
            _ if bitpat!(0 0 _ _ _ 0 _ _)(byte) => {
                // "Normal" ALU opcode with Mod-Reg-R/M byte
                let alu_op = AluOp::from_u8((byte & 0b00111000) >> 3)
                    .expect("couldn't turn 3-bit u8 into AluOp");
                let size = self.prefixes.size(default_size_bit)?;
                let modrm = self.read_modrm()?;
                let reg = modrm.reg(size).into();
                let rm = self.read_addressing(modrm, size)?;
                let (src, dest) = if default_dir_bit {
                    (rm, reg)
                } else {
                    (reg, rm)
                };

                Instr::Alu { op: alu_op, dest, src }
            }
            _ if bitpat!(0 0 _ _ _ 1 0 _)(byte) => {
                // ALU op with immediate and eax/ax
                let alu_op = AluOp::from_u8((byte & 0b00111000) >> 3)
                    .expect("couldn't turn 3-bit u8 into AluOp");
                let size = self.prefixes.size(default_size_bit)?;
                let dest = match size {
                    OpSize::Bits8 => Register::Al,
                    OpSize::Bits16 => Register::Ax,
                    OpSize::Bits32 => Register::Eax,
                }.into();
                let src = self.read_immediate(size)?.into();

                Instr::Alu { op: alu_op, dest, src }
            }
            _ if bitpat!(1 0 0 0 0 0 _ _)(byte) => {
                // ALU opcode with immediate

                // direction can't be changed here, so it encodes something else:
                // 0=imm. same size as operand, 1=imm. 8-bit sign-extended to op
                let sign_ext_imm = default_dir_bit;
                let size = self.prefixes.size(default_size_bit)?;
                let modrm = self.read_modrm()?;
                let op = AluOp::from_u8(modrm.reg_raw())
                    .expect("couldn't determine ALU op");
                let dest = self.read_addressing(modrm, size)?;
                let src = if sign_ext_imm {
                    // 8-bit immediate sign-extended to `size`
                    Immediate::from(self.read()?).sign_ext_to(size).into()
                } else {
                    self.read_immediate(size)?.into()
                };

                Instr::Alu { op, dest, src }
            }
            _ if bitpat!(1 0 1 1 _ _ _ _)(byte) => {  // 0xB_
                // 0xB_ = load immediate to register
                let size = byte & 0b0000_1000 != 0;
                let size = self.prefixes.size(size)?;
                let dest = ModRegRm::conv_reg(byte & 0b111, size).into();
                let src = self.read_immediate(size)?.into();

                Instr::Mov { dest, src }
            }
            _ if bitpat!(1 0 0 0 1 0 _ _)(byte) => {
                // mov reg/mem <-> GP reg (basically, load/store)
                let size = self.prefixes.size(default_size_bit)?;
                let modrm = self.read_modrm()?;
                let reg = modrm.reg(size).into();
                let rm = self.read_addressing(modrm, size)?;
                let (src, dest) = if default_dir_bit {
                    (rm, reg)
                } else {
                    (reg, rm)
                };

                Instr::Mov { dest, src }
            }
            _ if bitpat!(1 0 1 0 0 0 _ _)(byte) => {
                // mov between al/ax/eax and abs. memory address
                let size = self.prefixes.size(default_size_bit)?;
                let rm = MemoryLocation {
                    size,
                    base_segment: self.prefixes.segment(Segment::Ds),
                    addressing: Addressing::absolute(self.read_i32()? as u32),
                }.into();
                let reg = match size {
                    OpSize::Bits8 => Register::Al,
                    OpSize::Bits16 => Register::Ax,
                    OpSize::Bits32 => Register::Eax,
                }.into();
                let (src, dest) = if default_dir_bit {
                    // inverted!
                    (reg, rm)
                } else {
                    (rm, reg)
                };

                Instr::Mov { dest, src }
            }
            _ if bitpat!(0 1 1 1 _ _ _ _)(byte) => {  // 0x7_
                // conditional short jumps

                // get rid of branch hint prefixes
                match self.prefixes.segment(Segment::Cs) {
                    Segment::Cs | Segment::Ds => {},
                    _ => return Err(DecoderError::ud("invalid prefix for conditional jump")),
                }

                let cc = ConditionCode::from_u8(byte & 0x0F)
                    .expect("can't get condition code from 4-bit encoding");
                let offset = self.read()? as i8;
                let target = Immediate::Imm32(self.pos.wrapping_add(offset as u32) as i32).into();

                Instr::JumpIf { cc, target }
            }
            _ if bitpat!(0 1 0 1 _ _ _ _)(byte) => {  // 0x5_
                // push or pop 16- or 32-bit register
                let pop = byte & 0b0000_1000 != 0;
                let size = self.prefixes.size(true)?; // default 32-bit
                let reg = ModRegRm::conv_reg(byte & 0b111, size);

                if pop {
                    Instr::Pop { reg }
                } else {
                    Instr::Push { operand: reg.into() }
                }
            }
            _ if bitpat!(0 1 0 0 _ _ _ _)(byte) => {  // 0x4_
                // inc/dec register
                let size = if self.prefixes.take(PrefixFlags::OVERRIDE_OPERAND) {
                    OpSize::Bits16
                } else {
                    OpSize::Bits32
                };
                let is_dec = byte & 0b1000 != 0;
                let operand = ModRegRm::conv_reg(byte & 0b111, size).into();

                if is_dec {
                    Instr::Dec { operand }
                } else {
                    Instr::Inc { operand }
                }
            }
            _ if bitpat!(0 1 1 0 1 0 _ 0)(byte) => {  // 0x68 / 0x6A
                // push immediate

                // size bit inverted and in different position
                let smol = byte & 0b10 != 0;
                let size = self.prefixes.size(!smol)?;
                let imm = self.read_immediate(size)?.into();

                Instr::Push { operand: imm }
            }
            _ if bitpat!(1 1 1 1 0 1 1 _)(byte) => {  // 0xF6 / 0xF7
                // test/not/neg/mul/imul/div/idiv
                // operand is R/M part of modrm, while `Reg` is the opcode extension
                let size = self.prefixes.size(default_size_bit)?;
                let modrm = self.read_modrm()?;
                let operand = self.read_addressing(modrm, size)?;

                match modrm.reg_raw() {
                    0 => Instr::Test { lhs: operand, rhs: self.read_immediate(size)?.into() },
                    1 => return Err(DecoderError::ud("use of 0xF6/0xF7 op with ext. opcode 1")),
                    2 => Instr::Not { operand },
                    3 => Instr::Neg { operand },
                    4 => Instr::Mul { operand },
                    5 => Instr::Imul { operand },
                    6 => Instr::Div { operand },
                    7 => Instr::Idiv { operand },
                    _ => unreachable!()
                }
            }
            _ if bitpat!(1 0 0 0 0 1 0 _)(byte) => {  // 0x84 / 0x85
                // test
                let size = self.prefixes.size(default_size_bit)?;
                let modrm = self.read_modrm()?;
                let lhs = self.read_addressing(modrm, size)?;
                let rhs = modrm.reg(size).into();

                Instr::Test { lhs, rhs }
            }
            _ if bitpat!(1 1 0 0 0 0 0 _)(byte) || bitpat!(1 1 0 1 0 0 _ _)(byte) => {    // C0 / C1 / D0-D3
                // shift group 2
                let size = self.prefixes.size(default_size_bit)?;
                let modrm = self.read_modrm()?;
                let dest = self.read_addressing(modrm, size)?;

                let src: Operand;   // 8-bit shift amount
                if byte & 0xF0 == 0xC0 {
                    // 0xC0/0xC1 -  8-bit immediate
                    src = self.read_immediate(OpSize::Bits8)?.into();
                } else {
                    // 0xD_
                    if byte & 0b10 == 0 {
                        // 0xD0/D1 - constant 1
                        src = Immediate::Imm8(1).into();
                    } else {
                        // 0xD2/D3 - CL register
                        src = Register::Cl.into();
                    }
                }

                let op = ShiftOp::from_u8(modrm.reg_raw())
                    .expect("couldn't turn reg field into ShiftOp");

                Instr::Shift { op, dest, src }
            }
            0xC6 | 0xC7 => {
                // grp 11 - mov
                let size = self.prefixes.size(default_size_bit)?;
                let modrm = self.read_modrm()?;
                let dest = self.read_addressing(modrm, size)?;
                if modrm.reg_raw() != 0 {
                    return Err(DecoderError::ud("0xC6/0xC7 with non-0 Reg field"));
                }
                let src = self.read_immediate(size)?.into();

                Instr::Mov { dest, src }
            }
            0xC9 => {
                let size = if self.prefixes.take(PrefixFlags::OVERRIDE_OPERAND) {
                    OpSize::Bits16
                } else {
                    OpSize::Bits32
                };

                Instr::Leave { size }
            }
            0xE8 => {
                // call with eip-relative offset (FIXME can be 16-bit)
                let offset = self.read_i32()?;

                // EIP after the call instr.
                let eip = self.pos;
                let target = Immediate::Imm32(eip.wrapping_add(offset as u32) as i32).into();

                Instr::Call { target }
            }
            0xE9 => {
                // jmp with eip-relative offset (FIXME can be 16-bit)
                let offset = self.read_i32()?;

                // EIP after the instr.
                let eip = self.pos;
                let target = Immediate::Imm32(eip.wrapping_add(offset as u32) as i32).into();

                Instr::Jump { target }
            }
            0xEB => {
                // unconditional jump with 8-bit relative offset
                let offset = self.read()? as i8;

                // EIP after the instr.
                let eip = self.pos;
                let target = Immediate::Imm32(eip.wrapping_add(offset as u32) as i32).into();

                Instr::Jump { target }
            }
            _ if bitpat!(1 1 0 0 0 0 1 _)(byte) => {
                // ret
                let has_imm = byte & 1 == 0;
                let pop = if has_imm {
                    self.read_i16()? as u16
                } else {
                    0
                };

                Instr::Ret { pop }
            }
            0x8D => {
                // lea
                let modrm = self.read_modrm()?;
                let size = if self.prefixes.take(PrefixFlags::OVERRIDE_OPERAND) {
                    OpSize::Bits16
                } else {
                    OpSize::Bits32
                };
                let dest = modrm.reg(size);
                let src = self.read_addressing(modrm, OpSize::Bits8 /* doesn't matter */)?;
                match src {
                    Operand::Reg(_) | Operand::Imm(_) => {
                        return Err(DecoderError::ud("use of `lea` with non-memory operand"));
                    }
                    Operand::Mem(src) => Instr::Lea { dest, src },
                }
            }
            0x99 => {
                // cwd/cdq
                if self.prefixes.take(PrefixFlags::OVERRIDE_OPERAND) {
                    // 16-bit
                    Instr::Cwd
                } else {
                    // 32-bit
                    Instr::Cdq
                }
            }
            0x6B => {
                // 3-operand imul with 8-bit immediate
                let size = if self.prefixes.take(PrefixFlags::OVERRIDE_OPERAND) {
                    OpSize::Bits16
                } else {
                    OpSize::Bits32
                };
                let modrm = self.read_modrm()?;
                let dest = modrm.reg(size);
                let src1 = self.read_addressing(modrm, size)?;
                let src2 = self.read_immediate(OpSize::Bits8)?.sign_ext_to(size).into();

                Instr::ImulTrunc { dest, src1, src2 }
            }
            0xA4 | 0xA5 => {    // movs
                let size = self.prefixes.size(default_size_bit)?;
                let rep = self.prefixes.take(PrefixFlags::REP_REPE);

                Instr::StrMem { op: StrMemOp::Movs, rep, size }
            }
            // A6-A7 = cmps
            0xA8 | 0xA9 => {    // test al/ax/eax
                let size = self.prefixes.size(default_size_bit)?;
                let rhs = self.read_immediate(size)?.into();
                let lhs = match size {
                    OpSize::Bits8 => Register::Al,
                    OpSize::Bits16 => Register::Ax,
                    OpSize::Bits32 => Register::Eax,
                }.into();

                Instr::Test { lhs, rhs }
            }
            0xAA | 0xAB => {    // stos (stosb, stosw, stosd)
                let size = self.prefixes.size(default_size_bit)?;
                let rep = self.prefixes.take(PrefixFlags::REP_REPE);

                Instr::StrMem { op: StrMemOp::Stos, rep, size }
            }
            0xFF => {
                // Inc/Dec/Push group 4
                let size = if self.prefixes.take(PrefixFlags::OVERRIDE_OPERAND) {
                    OpSize::Bits16
                } else {
                    OpSize::Bits32
                };
                let modrm = self.read_modrm()?;
                let operand = self.read_addressing(modrm, size)?;

                match modrm.reg_raw() {
                    0 => Instr::Inc { operand },
                    1 => Instr::Dec { operand },
                    2 => Instr::Call { target: operand },
                    3 => unimplemented!("far call"),
                    4 => Instr::Jump { target: operand },
                    5 => unimplemented!("far jump"),
                    6 => Instr::Push { operand },
                    7 => return Err(DecoderError::ud("0xFF group with opcode 7")),
                    _ => unreachable!(),
                }
            }
            0xCC => Instr::Int { vector: 3 },   // int 3
            0xCD => {
                Instr::Int {
                    vector: self.read()?,
                }
            }
            0xCE => Instr::IntO,
            0x0F => self.decode_0f()?,
            _ => unimplemented!("opcode {:#04X}", byte),
        };

        if !self.prefixes.is_empty() {
            // Unconsumed (assumed invalid) prefix bytes. When the decoder
            // processes a collected prefix byte, it removes it from `prefixes`.
            // By assuming that any unconsumed prefixes are #UD, we ensure that
            // we never decode instructions wrongly because we're ignoring a
            // prefix.
            return Err(DecoderError::ud(format!("undecoded prefix bytes: {:?} (instruction: {})", self.prefixes, instr)));
        }

        Ok(instr)
    }

    /// Decodes a `0x0F` expansion opcode.
    fn decode_0f(&mut self) -> Result<Instr, DecoderError> {
        let byte = self.read()?;

        // Many instrs look like this: X X X X X X D S
        // Pull out D and S bits for convenience
        let _default_dir_bit  = (byte & 0b10) >> 1 != 0; // false = Reg to R/M, true = R/M to Reg
        let default_size_bit = (byte & 0b01) != 0;      // false = 8 bit, true = 16/32 bit

        let instr = match byte {
            0x80 ... 0x8F => {  // jcc - conditional near jump (16/32-bit offset)
                let cc = ConditionCode::from_u8(byte & 0x0F)
                    .expect("cannot convert condition code");
                // assume 32-bit - 16-bit clears the upper half of EIP, which is weird

                // EIP after the instr.
                let offset = self.read_i32()?;
                let eip = self.pos;
                let target = Immediate::Imm32(eip.wrapping_add(offset as u32) as i32).into();

                Instr::JumpIf { cc, target }
            }
            0x90 ... 0x9F => {  // setcc
                let cc = ConditionCode::from_u8(byte & 0x0F)
                    .expect("cannot convert condition code");
                let modrm = self.read_modrm()?;
                let operand = self.read_addressing(modrm, OpSize::Bits8)?;

                Instr::SetIf { cc, operand }
            }
            0xAF => {   // imul
                let size = self.prefixes.size(true)?;
                let modrm = self.read_modrm()?;
                let dest = modrm.reg(size);
                let src = self.read_addressing(modrm, size)?;

                Instr::ImulTrunc { dest, src1: dest.into(), src2: src }
            }
            0xB6 | 0xB7 => {    // movzx
                let src_size = if default_size_bit {
                    OpSize::Bits16
                } else {
                    OpSize::Bits8
                };
                let dest_size = if default_size_bit {
                    // src is 16-bit, dest must be 32
                    OpSize::Bits32
                } else {
                    // src is 8-bit, dest can be 16/32
                    if self.prefixes.take(PrefixFlags::OVERRIDE_OPERAND) {
                        OpSize::Bits16
                    } else {
                        OpSize::Bits32
                    }
                };

                let modrm = self.read_modrm()?;
                let dest = modrm.reg(dest_size);
                let src = self.read_addressing(modrm, src_size)?;

                Instr::MovZx { dest, src }
            }
            0xBE | 0xBF => {    // movsx
                let src_size = if default_size_bit {
                    OpSize::Bits16
                } else {
                    OpSize::Bits8
                };
                let dest_size = if default_size_bit {
                    // src is 16-bit, dest must be 32
                    OpSize::Bits32
                } else {
                    // src is 8-bit, dest can be 16/32
                    if self.prefixes.take(PrefixFlags::OVERRIDE_OPERAND) {
                        OpSize::Bits16
                    } else {
                        OpSize::Bits32
                    }
                };

                let modrm = self.read_modrm()?;
                let dest = modrm.reg(dest_size);
                let src = self.read_addressing(modrm, src_size)?;

                Instr::MovSx { dest, src }
            }
            _ => unimplemented!("0x0F expansion opcode {:#04X}", byte),
        };

        Ok(instr)
    }

    /// Read a single byte from the instruction stream.
    fn read(&mut self) -> Result<u8, DecoderError> {
        if self.len == 15 {
            // This would read the 16th byte, hitting the length limit.
            return Err(DecoderError::ud(format!("instruction length {} hits limit of 16 Bytes", self.len + 1)));
        }

        let b = self.mem.load(self.pos)?;
        self.pos += 1;
        self.len += 1;
        Ok(b)
    }

    /// Reads a Mod-Reg-R/M byte from the instruction stream.
    fn read_modrm(&mut self) -> Result<ModRegRm, DecoderError> {
        Ok(ModRegRm(self.read()?))
    }

    /// Reads additional addressing mode data from the stream, as specified in
    /// a Mod-Reg-R/M byte.
    ///
    /// The result of this is an operand of the instruction. Which one (source
    /// or dest) usually depends on the `D` (direction) bit in the opcode.
    ///
    /// # Parameters
    ///
    /// * `modrm`: The Mod-Reg-R/M byte.
    /// * `size`: The operation's size (used for register addr. mode).
    fn read_addressing(&mut self, modrm: ModRegRm, size: OpSize) -> Result<Operand, DecoderError> {
        use self::AddressingMode::*;

        // We always use `DS` as the default segment, which is wrong, but should
        // suffice for this use case.
        let base_segment = self.prefixes.segment(Segment::Ds);

        // use Mod and R/M fields to determine addressing mode
        let mode = modrm.addressing_mode();
        if mode == Register {
            return Ok(modrm.rm_as_reg(size).into());
        }

        if mode == RegIndirect && modrm.rm_raw() == 0b101 {
            // displacement-only mode
            return Ok(MemoryLocation {
                size,
                base_segment,
                addressing: Addressing::absolute(self.read_i32()? as u32),
            }.into());
        }

        if modrm.rm_raw() == 0b100 {
            // SIB
            let sib = Sib::decode(self.read()?, mode)?;
            return Ok(MemoryLocation {
                size,
                base_segment,
                addressing: Addressing::Sib {
                    scale: sib.scale_val,
                    index: sib.index,
                    base: sib.base,
                    disp: self.read_disp(sib.disp_mode)?,
                },
            }.into());
        }

        // Register-indirect addressing with optional displacement
        Ok(MemoryLocation {
            size,
            base_segment,
            addressing: Addressing::Disp {
                base: Some(modrm.rm_as_reg(OpSize::Bits32)),
                disp: self.read_disp(mode)?,
            },
        }.into())
    }

    /// Read a displacement
    fn read_disp(&mut self, mode: AddressingMode) -> Result<i32, DecoderError> {
        use self::AddressingMode::*;

        Ok(match mode {
            RegIndirect => 0,
            OneByteDisplacement => self.read()? as i8 as i32,
            FourByteDisplacement => self.read_i32()?,
            _ => unreachable!(),
        })
    }

    fn read_i32(&mut self) -> Result<i32, DecoderError> {
        let w = self.mem.load_i32(self.pos)?;
        self.pos += 4;
        Ok(w)
    }

    fn read_i16(&mut self) -> Result<i16, DecoderError> {
        let w = self.mem.load_i16(self.pos)?;
        self.pos += 2;
        Ok(w)
    }

    fn read_immediate(&mut self, size: OpSize) -> Result<Immediate, DecoderError> {
        Ok(match size {
            OpSize::Bits8 => self.read()?.into(),
            OpSize::Bits16 => self.read_i16()?.into(),
            OpSize::Bits32 => self.read_i32()?.into(),
        })
    }
}

/// A Mod-Reg-R/M byte (also called Mod-R/M).
///
/// This is used by many opcodes to define their source and destination operands
/// and, if present, follows right after the opcode bytes.
#[derive(Debug, Copy, Clone)]
struct ModRegRm(u8);

impl ModRegRm {
    /// Get the addressing mode, specified by the `Mod` field.
    ///
    /// This influences the interpretation of the `R/M` field.
    fn addressing_mode(&self) -> AddressingMode {
        match (self.0 & 0b11000000) >> 6 {
            0b00 => AddressingMode::RegIndirect,
            0b01 => AddressingMode::OneByteDisplacement,
            0b10 => AddressingMode::FourByteDisplacement,
            0b11 => AddressingMode::Register,
            _ => unreachable!(),
        }
    }

    /// Get the register specified in the `Reg` field, given the operand size.
    fn reg(&self, size: OpSize) -> Register {
        Self::conv_reg(self.reg_raw(), size)
    }

    /// Gets the raw value of the `Reg` field.
    ///
    /// This is used to encode ALU opcodes that take an immediate, for example.
    fn reg_raw(&self) -> u8 {
        (self.0 & 0b00111000) >> 3
    }

    /// Interprets the R/M field as a register (like Reg) for when using the
    /// register addressing mode.
    fn rm_as_reg(&self, size: OpSize) -> Register {
        Self::conv_reg(self.0 & 0b00000111, size)
    }

    /// Returns the raw R/M field value.
    fn rm_raw(&self) -> u8 {
        self.0 & 0b111
    }

    /// Convert a 3-bit register encoding to the register, given the register
    /// size.
    fn conv_reg(reg: u8, size: OpSize) -> Register {
        use cpu::instr::Register::*;
        use self::OpSize::*;

        match (reg & 0b111, size) {
            (0b000, Bits8) => Al,
            (0b000, Bits16) => Ax,
            (0b000, Bits32) => Eax,
            (0b001, Bits8) => Cl,
            (0b001, Bits16) => Cx,
            (0b001, Bits32) => Ecx,
            (0b010, Bits8) => Dl,
            (0b010, Bits16) => Dx,
            (0b010, Bits32) => Edx,
            (0b011, Bits8) => Bl,
            (0b011, Bits16) => Bx,
            (0b011, Bits32) => Ebx,
            (0b100, Bits8) => Ah,
            (0b100, Bits16) => Sp,
            (0b100, Bits32) => Esp,
            (0b101, Bits8) => Ch,
            (0b101, Bits16) => Bp,
            (0b101, Bits32) => Ebp,
            (0b110, Bits8) => Dh,
            (0b110, Bits16) => Si,
            (0b110, Bits32) => Esi,
            (0b111, Bits8) => Bh,
            (0b111, Bits16) => Di,
            (0b111, Bits32) => Edi,
            _ => unreachable!(),    // FIXME: unchecked, probably?
        }
    }
}

/// A decoded Scaled Index Byte (SIB).
///
/// The SIB is followed by a displacement according to the addressing mode
/// specified in the Mod-Reg-R/M byte.
struct Sib {
    /// 1, 2, 4 or 8
    scale_val: u8,
    /// The index register to multiply with the scale value.
    index: Option<Register>,
    /// The base register, or `None` if Base is `0b101` and Mod is `0b00`.
    base: Option<Register>,
    /// Displacement addr. mode. The Mod-Reg-R/M addressing mode can be
    /// overridden.
    disp_mode: AddressingMode,
}

impl Sib {
    fn decode(raw: u8, mut mode: AddressingMode) -> Result<Self, DecoderError> {
        let (scale, index, base) = (
            raw >> 6,
            (raw & 0b00111000) >> 3,
            (raw & 0b00000111),
        );

        let scale = match scale {
            0b00 => 1,
            0b01 => 2,
            0b10 => 4,
            0b11 => 8,
            _ => unreachable!(),
        };

        let index = if index == 0b100 {
            // this would encode ESP, but is special-cased to leave out the index reg
            None
        } else {
            Some(ModRegRm::conv_reg(index, OpSize::Bits32))
        };
        let base = if base == 0b101 && mode == AddressingMode::RegIndirect {
            // displacement-only enforces a 32-bit displacement
            mode = AddressingMode::FourByteDisplacement;    // disp32
            None
        } else {
            Some(ModRegRm::conv_reg(base, OpSize::Bits32))
        };

        Ok(Sib {
            scale_val: scale,
            index,
            base,
            disp_mode: mode,
        })
    }
}

/// The possible values of the Mod-Reg-R/M bytes Mod field.
///
/// This is the interpretation in 32-bit mode, 16-bit mode is not supported and
/// interprets these differently.
///
/// For all addressing modes except `Register`, if `R/M == 0b100`, an SIB byte
/// follows the Mod-R/M byte and replaces the base address (the displacement
/// is still applied to the SIB-determined address).
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum AddressingMode {
    /// `00` - Operand is in memory, its effective address is a register.
    ///
    /// If `R/M == 0b101`, a 32-bit displacement follows the Mod-R/M byte and
    /// is the absolute address where the operand is stored.
    RegIndirect,

    /// `01` - Same as `RegIndirect`, but followed by a 1-byte/8-bit signed
    /// displacement added to the effective address.
    OneByteDisplacement,

    /// `10` - Same as `RegIndirect`, but followed by a 4-byte/32-bit signed
    /// displacement added to the effective address.
    FourByteDisplacement,

    /// `11` - Register-register op, no memory access. R/M is interpreted just
    /// like Reg.
    Register,
}

/// Error type returned by the decoder.
///
/// This can either be an actual instruction encoding problem, which would be
/// equivalent to the CPU raising `#UD` at that point, or a memory access
/// violation. Since all CPU exceptions go through the kernel anyways, this
/// isn't terribly important.
#[derive(Debug)]
pub enum DecoderError {
    /// Memory error while reading machine code bytes.
    Memory(MemoryError),
    /// Undefined instruction error (`#UD`).
    Undefined(String),
}

impl DecoderError {
    #[cold]
    pub(crate) fn ud<S: AsRef<str>>(why: S) -> Self {
        DecoderError::Undefined(why.as_ref().to_string())
    }
}

impl From<MemoryError> for DecoderError {
    #[cold]
    fn from(e: MemoryError) -> Self {
        DecoderError::Memory(e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use memory::ArrayMemory;

    fn decode(rawstr: &str) -> Result<Instr, DecoderError> {
        let bytes: Vec<_> = rawstr.split_whitespace()
            .map(|hexstr| u8::from_str_radix(hexstr, 16).unwrap())
            .collect();

        let mem = ArrayMemory::new(bytes);
        let mut dec = Decoder::new(&mem, 0);
        dec.decode_next()
    }

    fn decodes_as(rawstr: &str, printed: &str) {
        let inst = decode(rawstr).unwrap();
        assert_eq!(inst.to_string(), printed);
    }

    /// Combined decoder and printer test. Interesting patterns are added here
    /// as necessary and can be cross-verified with other disassemblers like
    /// https://onlinedisassembler.com/odaweb/ - don't forget to add tests using
    /// negative displacements and offsets too, those are hard to get right!
    #[test]
    fn disassemble_smoke() {
        decodes_as("8D 44 08 0F", "lea eax,[eax+ecx+0xf]");
        decodes_as("8D 44 08 FE", "lea eax,[eax+ecx-0x2]");
        decodes_as("F7 F9", "idiv ecx");
        decodes_as("73 02", "jnc 0x00000004");  // without context, we only get the abs. target addr
        decodes_as("99", "cdq");
        decodes_as("66 99", "cwd");
        decodes_as("33 F6", "xor esi,esi");
        decodes_as("83 E0 F0", "and eax,0xfffffff0");
        decodes_as("89 01", "mov [ecx],eax");
        decodes_as("68 9B D8 04 00", "push dword 0x4d89b");
        decodes_as("5F", "pop edi");
        decodes_as("C3", "ret");
        decodes_as("C2 10 00", "ret 16");
        decodes_as("81 78 08 00 00 FE FF", "cmp dword [eax+0x8],0xfffe0000");
        decodes_as("81 78 ff 00 00 FE FF", "cmp dword [eax-0x1],0xfffe0000");
        decodes_as("8D BD 00 F4 FF FF", "lea edi,[ebp-0xc00]");
        decodes_as("6b 84 8b ab 00 00 00 02", "imul eax,[ebx+ecx*4+0xab],2");
        decodes_as("85 C0", "test eax,eax");
        decodes_as("C1 E9 02", "shr ecx,2");
        decodes_as("FF 74 24 04", "push dword [esp+0x4]");
        decodes_as("F3 AB", "rep stosd");
        decodes_as("C9", "leave");
        decodes_as("66 C9", "data16 leave");
        decodes_as("C7 45 F4 40 00 00 00", "mov [ebp-0xc],0x40");
        decodes_as("0F AF 45 E8", "imul eax,[ebp-0x18]");
        decodes_as("0F 95 C1", "setne cl");
        decodes_as("0F 84 AE 00 00 00", "je 0x000000B4");
        decodes_as("FF 24 85 C1 D7 15 00", "jmp dword [eax*4+0x15d7c1]");
        decodes_as("64 0F B6 05 24 00 00 00", "movzx eax,byte [fs:0x24]");
        decodes_as("64 0F BE 05 24 00 00 00", "movsx eax,byte [fs:0x24]");
        decodes_as("A8 82", "test al,0x82");
        // TODO shift/rotate instrs
        // TODO: `call` w/ destination
    }

    #[test]
    fn length_limit() {
        decodes_as(
            "66 66 66 66 66 66 66 66 66 66 66 66 66 66 99",
            "cwd"
        );
        decode(
            "66 66 66 66 66 66 66 66 66 66 66 66 66 66 66 99"
        ).unwrap_err();
    }
}
