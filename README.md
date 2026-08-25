# Sam Coupé MasterDOS Source

Sam Coupé MasterDOS v2.3 Source

Version 2.2 and 2.3 are almost identical, the differences between v2.2 and v2.3 are labeled **Fix_** in the source.

## Contents

| Folder | |
|---|---|
| [src/](src/) | The original source, for the Comet assembler |
| [annotated-src/](annotated-src/) | The same source documented as a modern codebase, assembling to a byte-identical binary — see [its README](annotated-src/README.md) |
| [docs/](docs/) | Reference and user documentation, derived from the source — see below |
| [res/](res/) | The released binary and a disk image |

## Documentation

| Document | Contents |
|---|---|
| [docs/user-guide.md](docs/user-guide.md) | **Start here.** Data files, subdirectories, RAM discs, the clock, and recovery |
| [docs/commands.md](docs/commands.md) | Every command: syntax, arguments, behaviour and errors |
| [docs/functions.md](docs/functions.md) | `TIME$`, `DATE$`, `DIR$`, `FSTAT`, `DSTAT`, `FPAGES`, `INP$`, `PATH$`, `DVAR`, `EOF`, `PTR`, `LENGTH` |
| [docs/disk-format.md](docs/disk-format.md) | Disk geometry, the directory, sector chaining, the sector map, disk identity |
| [docs/file-formats.md](docs/file-formats.md) | File types, the nine-byte header, the 48-byte header, per-type data |
| [docs/subdirectories.md](docs/subdirectories.md) | How a tree is stored in a flat directory, and how paths work |
| [docs/ram-discs.md](docs/ram-discs.md) | Drives 3–7, MegaRAM, and how a page pretends to be a disk |
| [docs/open-files.md](docs/open-files.md) | Streams, channels, random access, `POINT`, and the `D` channel layout |
| [docs/hook-interface.md](docs/hook-interface.md) | The `RST &08` hook codes 128–174 |
| [docs/errors.md](docs/errors.md) | Error codes and messages |
| [docs/dos-variables.md](docs/dos-variables.md) | The `DVAR` block |

> [!WARNING]
> The `annotated-src/` and `docs/` trees were produced with AI assistance and have not been run on real
> hardware. The annotated source is proven byte-identical by `annotated-src/check.sh`; the commentary and
> documentation are a well-evidenced reading of the code, not the author's own words.
