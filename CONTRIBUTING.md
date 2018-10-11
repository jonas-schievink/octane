# Contributing

TODO: Write this

## Coding Guidelines

* Use the logging macros liberally (`error!`, `warn!`, `info!`, `debug!`,
  `trace!`).
  
  Use `trace!` for logging in hot paths that can produce messages every frame or
  even more often than that.
  
  Use `warn!` if something is knowingly emulated inaccurately or incorrectly
  (for example, a parameter was specified, but is ignored by the
  implementation).

* Make use of assertions (`assert!`, `assert_eq!`) whenever an internal
  invariant must hold.
  
  Generally, the `debug_assert*` macros should only be used if using `assert*`
  would cause a measurable slowdown.

## Resources

This section collects a few potentially useful links that document parts of the
Xbox.

* [Deconstructing the Xbox Boot ROM](https://mborgerson.com/deconstructing-the-xbox-boot-rom/)

  This excellent article documents what the Xbox's stage 1 bootloader (stored in
  MCPX ROM) does.

* [17 Mistakes Microsoft Made in the Xbox Security System](https://events.ccc.de/congress/2005/fahrplan/attachments/591-paper_xbox.pdf) (PDF)

  An awesome writeup detailing the history of Xbox exploits.

## Internals

### Physical memory map

The following is the physical memory map used by the console (to be implemented
by the emulator):

```
    Address range     |   Size   |
----------------------+----------+---------------------------------------
0x00000000-0x10000000 | 256 MiB  | RAM
                      |          | Retail Xbox only has 64 MiB
                      |          | Dev kits have 128 MiB
                      |          | (rest is apparently just unused)
----------------------+----------+---------------------------------------
0xFF000000-0xFFFFFFFF | 16 MiB   | Flash
                      |          | (the Flash ROM is 256 KiB or 1 MiB
                      |          | and is repeated throughout this area)
----------------------+----------+---------------------------------------
0xFD000000-0xFDFFFFFF | 16 MiB   | GPU control registers
----------------------+----------+---------------------------------------
```

Sources:
[http://xboxdevwiki.net/Boot_Process#Paging](http://xboxdevwiki.net/Boot_Process#Paging)
(lists the virtual memory setup created by the bootloader),
[http://xboxdevwiki.net/BIOS](http://xboxdevwiki.net/BIOS),
[http://xboxdevwiki.net/Flash_ROM](http://xboxdevwiki.net/Flash_ROM).

FIXME: Information in the "Flash ROM" and "BIOS" wiki pages contradict, figure
out which one is right (currently trusting the "BIOS" page).

### Virtual memory map (Emulator) 

Here's the virtual memory map that is currently created by the emulator (as seen
by the running program). Most kernel data structures are stored in host memory
and are invisible to the program.

An explanation of the objects follows below.

```
        / 0xFFFFFFFF \
        |             |- Handle space (>1 billion handles)
Kernel  | 0xC0000000 /
space   | 0xB0000000 --- Thread exit sentinel
        |
        \ 0x80000000
        / 0x7FFFFFFF
        |
        | 0x???????? \
User    |             |- XBE (headers + sections)
space   | 0x00010000 /
        |
        | 0x0000???? \
        |             |- Zero page
        \ 0x00000000 /
```

Note that there isn't really a large distinction between user and kernel space
as the program has access to both.

* **Handle space**: The kernel allocates handles for all objects (Mutexes,
  Files, Threads, ...) managed by it. They are 32-bit values comparable to file
  descriptors on Unix. All handles have values in the range noted above and are
  mapped to the actual object data in maps stored on the host system.

* **Thread exit sentinel**: The sentinel value `0xB0000000` is pushed onto the
  main thread's stack when it is created. When a return to this address is
  attempted, the kernel kills the main thread (other threads are left running).
  
  The real kernel likely calls into a helper that will call the XBE's entry
  point, and, upon return, calls `PsTerminateSystemThread`. We could do the same
  thing, but it would mean having to put custom x86 assembly in the Xbox memory,
  which I'd like to avoid for now.

* **XBE**: XBE's *usually* specify `0x10000` as their base address, but don't
  have to, so theoretically this could be at a different address.

* **Zero page**: The first page of memory is reserved and should trap any memory
  access (once proper MMU emulation is in place). I don't yet know if a real
  Xbox does this, but it probably does. The size of the zero page is 4 KB on an
  Xbox and any x86-64 host, but might be larger on ARM, which is why it's marked
  as `???`. Note that this means that we don't pretend to have 4 KB pages when
  the host doesn't have them. An inaccuracy that hopefully won't cause too many
  problems later on (famous last words).

All other memory areas can be allocated dynamically by the kernel. None of this
was verified against hardware (yet).
