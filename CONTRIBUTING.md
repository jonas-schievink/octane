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

  This excellent article documents what the stage 1 bootloader of the Xbox does.

* [17 Mistakes Microsoft Made in the Xbox Security System](https://events.ccc.de/congress/2005/fahrplan/attachments/591-paper_xbox.pdf) (PDF)

  An awesome writeup detailing the history of Xbox exploits.
