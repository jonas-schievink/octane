mod prefix;
mod flags;
mod defs;
mod state;
pub mod decode;
pub mod disasm;
pub mod instr;
pub mod interpret;
pub mod visit;
pub mod imm;

pub use self::flags::Flags;
pub use self::state::State;

// general goal that might be useful: replace all `as` casts with safer alternatives
