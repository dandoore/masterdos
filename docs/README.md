# MasterDOS 2.3 — Documentation

Reference and user documentation for **MasterDOS**, the enhanced disk operating system for the SAM Coupé,
derived from the annotated source in [annotated-src/](../annotated-src/).

> [!WARNING]
> This documentation was produced with AI assistance by reading the source. Almost everything here is derived
> directly from the code and cites the routine it came from; the few claims taken from outside sources are
> labelled **[external]** and listed in [Sources](#sources). Nothing here has been checked on real hardware.

## The documents

**Start here if you are using MasterDOS:** [user-guide.md](user-guide.md).

| Document | Contents |
|---|---|
| [user-guide.md](user-guide.md) | A practical guide: data files, subdirectories, RAM discs, the clock, and recovery |
| [commands.md](commands.md) | Every command: syntax, arguments, behaviour and errors |
| [functions.md](functions.md) | `TIME$`, `DATE$`, `DIR$`, `FSTAT`, `DSTAT`, `FPAGES`, `INP$`, `PATH$`, `DVAR`, `EOF`, `PTR`, `LENGTH` |
| [disk-format.md](disk-format.md) | Disk geometry, the directory, sector chaining, the sector map, disk identity |
| [file-formats.md](file-formats.md) | File types, the nine-byte header, the 48-byte header, per-type data |
| [subdirectories.md](subdirectories.md) | How a tree is stored in a flat directory, and how paths work |
| [ram-discs.md](ram-discs.md) | Drives 3–7, MegaRAM, and how a page pretends to be a disk |
| [open-files.md](open-files.md) | Streams, channels, record access, `POINT`, and the `D` channel layout |
| [hook-interface.md](hook-interface.md) | The `RST &08` hook codes 128–174 |
| [errors.md](errors.md) | Error codes and messages |
| [dos-variables.md](dos-variables.md) | The `DVAR` block: 150-odd variables and what each does |

## What MasterDOS is

MasterDOS replaces SAMDOS. It was written by **Andrew J. A. Wright**, the author of the SAM Coupé ROM itself,
and it shows: it calls internal ROM addresses that are not in the published jump table, under the ROM's own
label names, because the author knew where they were.

It keeps SAMDOS's disk format and its interface to the ROM, and adds:

| Addition | Where |
|---|---|
| Subdirectories | [subdirectories.md](subdirectories.md) |
| Open files, read and written a byte at a time, sequentially or randomly | [open-files.md](open-files.md) |
| RAM discs on drives 3–7, including external MegaRAM | [ram-discs.md](ram-discs.md) |
| A real-time clock, and date-stamped files | [commands.md](commands.md#time-and-date) |
| Ten more commands and seven new BASIC functions | [commands.md](commands.md), [functions.md](functions.md) |
| A variable-size directory, up to 39 tracks | [disk-format.md](disk-format.md#the-directory) |
| `MOVE` between any two channels, and a fast whole-disk `BACKUP` | [commands.md](commands.md#move) |

**It also extends the BASIC interpreter.** SAMDOS could only claim tokens the ROM already had. MasterDOS
copies a block into the system page at boot and repoints four of the ROM's vectors — for printing a token,
tokenising, evaluating an expression and dispatching a command — so its own keywords are recognised
everywhere the ROM's are. That is how `FSTAT` and `TIME$` come to exist as real keywords rather than as
pokes.

## Compatibility with SAMDOS

The disk format is unchanged; every MasterDOS addition lives in bytes of the directory entry that SAMDOS
leaves at zero. So:

| | |
|---|---|
| A SAMDOS disk under MasterDOS | Works completely. Zeros read as "no extra directory tracks, no disk name, root directory" |
| A MasterDOS disk under SAMDOS | Works **if** the disk has four directory tracks and no subdirectories |
| A MasterDOS disk with extra directory tracks, under SAMDOS | **Unsafe.** SAMDOS's directory size is a constant 4, so it will allocate data over the extra directory tracks |
| A MasterDOS subdirectory, under SAMDOS | Appears as a zero-length file of unknown type (`WHAT?`). Erasing it orphans its contents |

A program can tell which DOS it is running under from `DVAR 7`: 20 for SAMDOS 2, 42 for MasterDOS 2.2, 43 for
2.3. The version is that byte less 20, divided by 10.

## Version

This is MasterDOS **2.3**. The source carries version 2.2 alongside it: where they differ, the 2.2 form is
present commented out under a `Fix_..._42` label and the 2.3 form live under `Fix_..._43`. The differences
amount to a handful of `DVAR` defaults and one address.

## Sources

Everything not marked below is derived from the annotated source, and cites the routine it came from.

* **[external]** [MGT filesystem — Sinclair Wiki](https://sinclair.wiki.zxnet.co.uk/wiki/MGT_filesystem) —
  cross-check of the directory entry layout, the sector map's bit ordering, and the file type numbers against
  the DISCiPLE/+D originals. Its statements about MasterDOS's own fields (250 subdirectory ID, 252–253 disk
  ID, 254 parent ID, 255 extra directory tracks) agree exactly with what the source does.
* **[external]** [MasterDOS hook codes — Sam Coupé Scrapbook](http://www.mono.org/~unc/Coupe/Tech/mdhook.html)
  — an independently written table of the hook codes, used to corroborate
  [hook-interface.md](hook-interface.md). Where it and the source disagree, the source is followed and the
  difference noted.
* **[external]** The [MasterDOS manual](http://ftp.nvg.ntnu.no/pub/sam-coupe/docs/manuals/software/SAM%20Coupe%20MasterDOS%20Manual%20V1.pdf)
  is the user-facing authority and is worth reading alongside this. This documentation was written from the
  source rather than from the manual, so where the two differ, the source describes what the code actually
  does.
* **[image]** The disk image `res/master_dos_v2-3.mgt` in this repository was parsed byte by byte against the
  layout in [disk-format.md](disk-format.md). Findings from it are marked **[image]**.

Related documentation in the [SAM Coupé ROM repository](https://github.com/stefandrissen/samrom) —
`docs/file-formats.md`, `docs/dos-and-extensions.md`, `docs/machine-code-interface.md` and
`docs/extending-basic.md` — describes the ROM side of everything here, and the
[SAMDOS repository](https://github.com/stefandrissen/samdos) documents the shared foundations at greater
length.
