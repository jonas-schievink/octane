extern crate octane;
extern crate termcolor;

use octane::cpu::decode::Decoder;
use octane::memory::ArrayMemory;

use termcolor::{ColorChoice, Color, ColorSpec, StandardStream, WriteColor};
use std::io::Write;
use std::panic::{set_hook, catch_unwind};

struct Tester {
    /// Byte sequences that did not make the decoder panic.
    implemented: usize,
    /// Total opcodes tested.
    total: usize,
    /// Scratch memory.
    mem: ArrayMemory,
}

impl Tester {
    fn test_group<'a, S, G, P>(&mut self, name: &str, mut state: S, mut gen: G)
    where
        G: FnMut(&mut S, &mut ArrayMemory) -> Option<P>,
        P: FnOnce(&mut StandardStream),
    {
        let mut out = StandardStream::stdout(ColorChoice::Auto);

        write!(out, "{}:", name).unwrap();

        while let Some(printer) = gen(&mut state, &mut self.mem) {
            let mut decoder = Decoder::new(&self.mem, 0);
            let success = catch_unwind(move || {
                decoder.decode_next().ok();     // ignore errors
            }).is_ok();

            self.total += 1;
            if success {
                self.implemented += 1;
            }

            let color = if success { Color::Green } else { Color::Red };
            out.set_color(ColorSpec::new().set_fg(Some(color))).unwrap();
            write!(out, " ").unwrap();
            printer(&mut out);
            out.set_color(ColorSpec::new().set_fg(None)).unwrap();
        }

        writeln!(out).unwrap();
    }

    fn print_summary(&mut self) {
        let mut out = StandardStream::stdout(ColorChoice::Auto);

        writeln!(out).unwrap();
        writeln!(out).unwrap();
        let pct = self.implemented as f32 / self.total as f32 * 100.0;
        writeln!(out, "{}/{} opcodes implemented ({:.0}%)", self.implemented, self.total, pct).unwrap();

        writeln!(out).unwrap();
        writeln!(out, "(these number are not very accurate)").unwrap();
    }
}

fn main() {
    let rawmem = vec![0x00; 50];
    let mem = ArrayMemory::new(rawmem);

    // silence panic messages
    set_hook(Box::new(|_| {}));

    let mut tester = Tester {
        implemented: 0,
        total: 0,
        mem,
    };

    tester.test_group("single-byte opcodes", 0x00..=0xFF, |bytes, mem| {
        let byte = bytes.next()?;
        mem.as_vec_mut()[0] = byte;
        Some(move |out: &mut StandardStream| write!(out, "{:#04X}", byte).unwrap())
    });

    let groups: Vec<(&str, Box<Iterator<Item=u8>>)> = vec![
        ("immediate group 1", Box::new(0x80..=0x83)),
        ("shift group 2", Box::new((0xC0..=0xC1).chain(0xD0..=0xD3))),
        ("group 11 - mov", Box::new(0xC6..=0xC7)),
        ("unary group 3", Box::new(0xF6..=0xF7)),
        ("group 1A - pop", Box::new(0x8F..=0x8F)),
        ("inc/dec group 4", Box::new(0xFE..=0xFE)),
        ("inc/dec group 5", Box::new(0xFF..=0xFF)),
    ];
    for (name, range) in groups {
        tester.test_group(name, range.flat_map(|opcode| (0..=7).map(move |reg| (opcode, reg))), |code, mem| {
            let (opcode, reg) = code.next()?;
            mem.as_vec_mut()[0] = opcode;
            mem.as_vec_mut()[1] = 0b11_000_000 | reg << 3;  // Mod-Reg-R/M
            Some(move |out: &mut StandardStream| write!(out, "{:#04X}-{}", opcode, reg).unwrap())
        });
    }

    tester.test_group("0x0F extension opcodes", 0x00..=0xFF, |bytes, mem| {
        let byte = bytes.next()?;
        mem.as_vec_mut()[0] = 0x0F;
        mem.as_vec_mut()[1] = byte;
        Some(move |out: &mut StandardStream| write!(out, "{:#04X}", byte).unwrap())
    });

    tester.print_summary();
}
