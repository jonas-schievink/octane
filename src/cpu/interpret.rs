//! x86 interpreter.

use cpu::decode::{Decoder, DecoderError};
//use cpu::instr::*;
use cpu::state::State;
use memory::VirtualMemory;

#[derive(Debug)]
pub struct Interpreter<M: VirtualMemory> {
    state: State,
    mem: M,
}

impl<M: VirtualMemory> Interpreter<M> {
    pub fn new(mem: M, eip: u32, esp: u32) -> Self {
        Self {
            state: State::new(eip, esp),
            mem,
        }
    }

    pub fn state_mut(&mut self) -> &mut State {
        &mut self.state
    }

    pub fn mem_mut(&mut self) -> &mut M {
        &mut self.mem
    }

    /// Execute the next instruction.
    pub fn step(&mut self) -> Result<(), InterpreterError> {
        let _instr = {
            let mut decoder = Decoder::new(&self.mem, self.state.eip());
            let instr = decoder.decode_next()?;
            self.state.set_eip(decoder.current_address());
            instr
        };

        unimplemented!()
    }
}

#[derive(Debug)]
pub enum InterpreterError {
    /// Error while decoding an instruction.
    Decode(DecoderError),
}

impl From<DecoderError> for InterpreterError {
    fn from(e: DecoderError) -> Self {
        InterpreterError::Decode(e)
    }
}
