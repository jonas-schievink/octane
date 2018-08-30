# Ooctane Xbox emulator

[![Build Status](https://travis-ci.org/jonas-schievink/octane.svg?branch=master)](https://travis-ci.org/jonas-schievink/octane)

Please refer to the [changelog](CHANGELOG.md) to see what changed in the last
releases.

This project tracks current stable Rust. It will never require nightly, but
currently doesn't guarantee a minimum supported Rust version.

# Project Goals

The goal of this project is to create an Xbox emulator that is as independent
from the host system as practically possible. This means that the emulator, once
it's further along, will work on any reasonable operating system and can be
relatively easily ported to other architectures.

The emulator will not implement any features using the hosting Windows operating
system, since that would mean that it only works on Windows, nor will it attempt
to directly execute the Xbox code on the host's CPU, which would force it to be
a 32-bit x86 application. It also won't try to replace libraries used by the
game with the host system's implementation (eg. DirectX 8).

The emulator will emulate the hardware on a very low level, while the Xbox
kernel uses high-level emulation (HLE) and is effectively reimplemented by the
emulator. Since games statically link in all used libraries except the kernel,
this should be sufficient to accurately emulate all games. Of course, it's
still far too early to draw any conclusions here.

As another goal, the emulator also aims to be extensively documented and
accessible to people intimidated by the apparent complexity and "magic" behind
emulators.
