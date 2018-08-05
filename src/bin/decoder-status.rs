extern crate xe;
extern crate termcolor;

use xe::cpu::decode::Decoder;
use xe::memory::ArrayMemory;

use termcolor::{ColorChoice, Color, ColorSpec, StandardStream, WriteColor};
use std::io::Write;
use std::panic::{set_hook, catch_unwind};

fn main() {
    let mut out = StandardStream::stdout(ColorChoice::Auto);
    let rawmem = vec![0x00; 16];
    let mut mem = ArrayMemory::new(rawmem);

    // silence panic messages
    set_hook(Box::new(|_| {}));

    write!(out, "Single-byte opcodes implemented:").unwrap();
    let mut impl_count = 0;
    for byte in 0x00..=0xff {
        mem.as_vec_mut()[0] = byte;

        let mut decoder = Decoder::new(&mem, 0);
        let success = catch_unwind(move || {
            decoder.decode_next().ok();     // ignore errors
        }).is_ok();

        if success {
            impl_count += 1;
        }

        let color = if success { Color::Green } else { Color::Red };
        out.set_color(ColorSpec::new().set_fg(Some(color))).unwrap();
        write!(out, " {:#04X}", byte).unwrap();
        out.set_color(ColorSpec::new().set_fg(None)).unwrap();
    }

    writeln!(out).unwrap();
    writeln!(out).unwrap();
    let pct = impl_count as f32 / 256.0 * 100.0;
    writeln!(out, "{}/256 bytes implemented ({:.0}%)", impl_count, pct).unwrap();

    writeln!(out).unwrap();
    writeln!(out, "(these number are not accurate - they do not include multi-byte and grouped opcodes and can have false positives)").unwrap();
}
