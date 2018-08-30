//! Benchmarks instruction decoder performance.

#[macro_use] extern crate criterion;
extern crate octane;

use criterion::{Benchmark, Criterion, Throughput};
use octane::cpu::decode::Decoder;
use octane::memory::{MmapMemory, VirtualMemory};

/// Test data taken from entry point of Limp Ninja's Demoplayer.
///
/// One instruction per line.
static DATA: &str = r#"
8B 0D 18 01 01 00
A1 08 01 01 00
2B C1
05 00 00 01 00
3B 01
73 02
89 01
A1 60 94 25 00
2B 05 50 94 25 00
8B 0D 54 94 25 00
56
8D 44 08 0F
57
83 E0 F0
6A FC
59
83 C0 04
A3 94 28 2D 00
99
F7 F9
8B 0D 58 94 25 00
33 F6
56
56
56
68 9B D8 04 00
56
56
89 01
E8 45 1B 00 00
8B F8
3B FE
75 0A
56
6A 01
6A 01
E8 46 12 00 00
57
E8 14 16 00 00
5F
5E
C3
"#;

fn decode_limp_ninja_demoplayer(c: &mut Criterion) {
    // expected instr count
    let icount = DATA.lines().filter(|line| !line.trim().is_empty()).count();
    let data: Vec<_> = DATA.split_whitespace()
        .map(|b| u8::from_str_radix(b, 16).unwrap())
        .collect();
    let bytes = data.len() as u32;

    // We use mmap-base memory to avoid bounds checks (like we expect the
    // emulator to later do). They can take up a lot of time.
    let mut mem = MmapMemory::new();
    mem.add_mapping(0..=bytes, &data, ".text").unwrap();

    c.bench("decode", Benchmark::new("limp ninja demoplayer", move |b| {
        b.iter(|| {
            let mut decoder = Decoder::new(&mem, 0);
            for _ in 0..icount {
                criterion::black_box(&decoder.decode_next().unwrap());
            }
        })
    }).throughput(Throughput::Bytes(bytes)));
}

criterion_group!(decode, decode_limp_ninja_demoplayer);
criterion_main!(decode);
