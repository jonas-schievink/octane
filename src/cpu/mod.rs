pub mod decode;
mod defs;
pub mod disasm;
pub mod flags;
pub mod instr;
mod prefix;
pub mod interpret;
pub mod state;
pub mod visit;

// general goal that might be useful: replace all `as` casts with safer alternatives
