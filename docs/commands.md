# MasterDOS 2.3 — Command Reference

Every command MasterDOS adds to SAM BASIC, from `CTAB` and `SYNTAX` in
[masterdos23.asm](../annotated-src/masterdos23.asm).

## How a DOS command reaches MasterDOS

The ROM raises error 29, *Not understood*, for a statement it cannot parse — and error 53, *No DOS* — and
before reporting either it calls the DOS at `JP SYNTAX`. `SYNTAX` rewinds `CHADD` to the start of the
statement and looks the token up in `CTAB`, a nineteen-entry table of token-and-address triples. A token of
zero ends the table and never matches, so the search always terminates on `CNF`, which handles `POINT` and
then the external vector.

Three things distinguish this from SAMDOS's arrangement:

* **`NRFLG` guards against recursion.** The DOS calls back into the ROM to evaluate expressions; if one of
  those evaluations itself fails with *Not understood*, the ROM would call `SYNTAX` again from inside the DOS.
  The flag makes the second call return the error unchanged.
* **A leading colon is skipped**, so `ON x: DIR: PRINT` works.
* **The external vector may live above &8000**, in which case the page in `DVAR 36` is mapped before the jump.

### The command table

| Token | Command | Token | Command |
|---|---|---|---|
| &86 | [`WRITE`](#read-and-write) | &B8 | [`READ`](#read-and-write) |
| &90 | [`DIR`](#dir) | &CF | [`COPY`](#copy) |
| &91 | [`FORMAT`](#format) | &E3 | [`RENAME`](#rename) |
| &92 | [`ERASE`](#erase) | &E4 | [`CALL`](#call) |
| &93 | [`MOVE`](#move) | &F1 | [`PROTECT`](#protect-and-hide) |
| &95 | [`LOAD`](#load) | &F2 | [`HIDE`](#protect-and-hide) |
| &98 | [`OPEN`](#open) | &F7 | [`BACKUP`](#backup) |
| &99 | [`CLOSE`](#close-and-clear) | &F8 | [`TIME`](#time-and-date) |
| &B3 | [`CLEAR`](#close-and-clear) | &F9 | [`DATE`](#time-and-date) |

[`POINT`](#point) is not in the table: it is spelled `FF 3D`, a two-byte function-style token, and is
recognised in `CNF` after the table search has failed.

`BACKUP` (&F7), `TIME` (&F8) and `DATE` (&F9) are **new keywords**, not ones the ROM already had. MasterDOS
adds them to the tokeniser through its BASIC extension — see [functions.md](functions.md#new-keywords).

## Summary

| Command | Purpose | New in MasterDOS |
|---|---|---|
| [`DIR`](#dir) | List the catalogue; also change directory and format the listing | Extended |
| [`FORMAT`](#format) | Format a disk or RAM disc, with a chosen directory size | Extended |
| [`ERASE`](#erase) | Delete files, or an empty subdirectory | Extended |
| [`RENAME`](#rename) | Rename a file, a directory, or the disk itself | Extended |
| [`COPY`](#copy) | Copy files, in as few passes as free memory allows | Extended |
| [`BACKUP`](#backup) | Duplicate a whole disk, transferring only the sectors in use | ✓ |
| [`MOVE`](#move) | Copy a byte stream between any two channels | ✓ |
| [`OPEN`](#open) | Open a stream onto a file, or create a subdirectory | ✓ |
| [`CLOSE`](#close-and-clear) | Close a stream, writing the file back | ✓ |
| [`CLEAR`](#close-and-clear) | Discard all streams without writing anything | ✓ |
| [`POINT`](#point) | Move the file pointer of an open stream | ✓ |
| [`TIME`](#time-and-date) | Read or set the clock | ✓ |
| [`DATE`](#time-and-date) | Read or set the date | ✓ |
| [`PROTECT` / `HIDE`](#protect-and-hide) | Set or clear the file flags | As SAMDOS |
| [`LOAD`](#load) | Load by directory number, or load a snapshot | As SAMDOS |
| [`READ` / `WRITE`](#read-and-write) | Raw sector access | Extended |
| [`CALL`](#call) | Resume a snapshot | As SAMDOS |

## File names, drives and paths

```text
[ device [ drive ] ":" ] [ path ] name
```

| Part | Rules |
|---|---|
| device | A single letter; only `D` is a disk. Defaults to the `DEVICE` letter |
| drive | **1 to 7.** 1 and 2 are floppies, 3–7 are [RAM discs](ram-discs.md). Defaults to the `DEVICE` number, falling back to `DVAR 15` (`ODEF`) |
| path | Directory names separated by `\` or `/`, with `^` meaning the parent. A leading separator starts from the root. See [subdirectories.md](subdirectories.md) |
| name | Ten characters as stored. Up to 38 characters of path and name are accepted (`MPL`) |

Two conveniences are built into `EVFILE`:

* A **null name** becomes `T:`, so `LOAD ""` still means tape as it always did.
* A bare **`D1` or `D1:`** becomes `D1:*`, so `DIR "D1"` lists everything.

**Wildcards.** `?` matches one character; `*` matches to the end of the name, or up to a `.` if one follows
it. `CKNAM` masks bit 5, so matching is case-insensitive for letters.

**Drive aliasing.** `CODN` maps every drive number through `DRPT` (`DVAR 111–117`) before use, so a drive can
be made to pretend to be another. `POKE DVAR 112, 3` makes every reference to drive 2 go to RAM disc 3 —
which is how a program written for two floppies is pointed at a RAM disc without changing it.

---

## DIR

```text
DIR [#stream] [drive] ["pattern"] [DATE] [!] [?]
DIR = "path"
```

Implemented at `DIR` in [masterdos23.asm](../annotated-src/masterdos23.asm), with `DITOB`, `ORDER` and
`PCNML` doing the listing.

| Form | Effect |
|---|---|
| `DIR` | **Names only, sorted, in as many columns as the window allows** — the default listing is the short one, unlike SAMDOS |
| `DIR 2` or `DIR "name"` | The detailed listing: number, name, sectors, type |
| `DIR ... !` | Force the short form |
| `DIR DATE` | The detailed listing with each file's date and time |
| `DIR #s` | To a stream other than 2 |
| `DIR ?` | Include files from **every** subdirectory, not just the current one |
| `DIR = "path"` | Change the current directory — see [subdirectories.md](subdirectories.md#changing-directory) |

**The short form cannot print as it goes**, because the names have to be sorted first: `DITOB` collects them
into the screen page, `ORDER` sorts them, and `PCNML` prints them in columns. Sorting is controlled by
`DVAR 9` (`SRTFG`), which defaults to 1 — set it to zero for directory order. The column count is chosen
automatically and left in `DVAR 8`.

The listing prints the disk name, the current path, and the free space. `DVAR 19` (`DTFLG`) is set by
`DIR DATE` and cleared at the start of every `DIR`.

`DIR$` returns the same listing as a string — see [functions.md](functions.md#dir).

---

## FORMAT

```text
FORMAT "name"
FORMAT "name", dirtracks
FORMAT "d3:name", dirtracks, tracks
FORMAT "name" TO "name"
FORMAT TO "name"
FORMAT "d3:", 0
```

Implemented at `WFOD`.

| Form | Effect |
|---|---|
| `FORMAT "d1:work"` | An ordinary disk with four directory tracks, named `work` |
| `FORMAT "d1:work", 10` | The same with a ten-track directory — 200 entries instead of 80 |
| `FORMAT "d3:ram", 1, 20` | A twenty-track RAM disc on drive 3, one directory track |
| `FORMAT "d1:new" TO "d2:old"` | Format, then copy the second disk onto the first |
| `FORMAT TO "d2:old"` | The copy alone |
| `FORMAT "d3:", 0` | **Erase RAM disc 3**, returning its pages to the system |

**The name is stored.** Unlike SAMDOS, MasterDOS keeps a ten-character disk name in the first directory
entry, and `DIR` prints it. It also stamps the disk with a random identifying word, made from the refresh
register and the frame counter, which is how a disk swap is detected later.

### Limits

Most of `WFOD` is validation:

| Target | Directory tracks | Total tracks |
|---|---|---|
| Real disk | 4 to 39 | From `DVAR 1` / `DVAR 2` |
| RAM disc | 1 to 39 | Must be given; directory + at least one data track |

The encoded limits at `WFOD02` express the last rule: with one directory track the total may be 2 to 157;
with four, 5 to 160.

Each directory track holds 20 entries, so `dirtracks` × 20 is the file capacity. The count stored on disk is
the number of tracks **beyond** the standard four (`entry0[255]`), which is why a four-track disk stores zero
and is readable by SAMDOS.

> [!WARNING]
> A disk with more than four directory tracks must not be written by SAMDOS, which assumes four and will
> allocate file data over the extra directory. Reading is safe; writing is not.

---

## ERASE

```text
ERASE [OVER] "pattern"
ERASE DIR "name"
```

Implemented at `ERAZ`. Deletes each matching file by zeroing the first byte of its entry.

| Case | Behaviour |
|---|---|
| Ordinary file | Deleted |
| Protected file | Skipped with a beep; *PROTECTED file* if nothing else was erased |
| `ERASE OVER` | Protected files deleted too |
| `ERASE DIR "x"` | Deletes the subdirectory `x` — **only if it is empty** |
| Directory not empty | *Directory not empty* |
| Nothing matched | *File not found* |

`ERASE DIR` checks emptiness by putting the directory's own tag into `CDIRT` and searching the catalogue for
any file carrying it. Because a subdirectory has no data sectors, deleting one frees nothing.

---

## RENAME

```text
RENAME "old" TO "new"
RENAME DIR "old" TO "new"
RENAME TO "diskname"
```

Implemented at `RENAM`.

`RENAME TO "name"` with no source **renames the disk itself**. It rewrites the name field of the first
directory entry and gives the disk a **fresh random word**, so that anything caching the disk's identity —
including MasterDOS's own per-drive record — notices the change.

Otherwise it is file to file, and unlike SAMDOS the target acts as a **template**: `TRX0` applies it to each
matching name, so wildcards rename in bulk.

```basic
RENAME "*.bak" TO "*.old"
```

*File name used* if the new name already exists.

---

## COPY

```text
COPY [OVER] "source" TO "target"
```

Implemented at `COPY`. As in SAMDOS, the target is a template and every match is copied.

The difference is how a large file is handled. A file may be bigger than any free block of RAM, so the copy
runs in **passes**: `FFPG` finds the largest run of free pages in the machine, as much of the file as will fit
is loaded into it, that much is written to the target, and the loop repeats. `TEMPB1` and `TEMPW4` carry the
remainder between passes; flag bit 1 means "more still to come", which suppresses the close; flag bit 3 means
the target is already open, so later passes append.

The practical effect is that **copying is limited by disk space, not by memory** — a 300K file copies on a
machine with 32K free, just more slowly.

*No pages free* if there is no free memory at all.

---

## BACKUP

```text
BACKUP "source" TO "target"
```

Duplicates a whole disk. Implemented at `BACKUP`.

Unlike SAMDOS's `FORMAT TO`, this is not a blind track-by-track copy. The free-sector map says how much of the
disk is actually occupied: scanning it backwards for the last non-zero byte gives the **highest sector in
use**, and everything above that is skipped. On a disk holding a few small files this turns a whole-disk copy
into a very short one.

The transfer runs in passes sized by free RAM (`FFPG` again), 32 sectors to a page. The first pass stamps the
target with a **fresh random word** and the disk name, so the copy is a distinct disk rather than a duplicate
identity — which matters, because two disks with the same random word would defeat the disk-change detection.

With one drive, the user is prompted to swap disks between each read and write. *No pages free* if there is no
free memory.

---

## MOVE

```text
MOVE source TO target
```

where each of source and target is either `#stream` or `"filename"`. Implemented at `MOVE` / `OPMOV`.

`MOVE` copies a stream of bytes from anything to anything: a file to the screen, the keyboard to a file, one
file to another, a stream to the printer. Both ends are opened as channels — a named file gets a temporary
channel, marked by a channel letter with bit 7 set so `CLTEMP` can reclaim it — and the body is a
two-instruction loop.

```basic
MOVE "readme" TO #2          : REM display a text file
MOVE #2 TO "d1:log"          : REM capture the screen
MOVE "d1:x" TO "d2:x"        : REM copy a file
MOVE "notes" TO #3           : REM print it
```

### Making the output readable

The interesting case is a disk file moved to the screen, keyboard or printer, where MasterDOS interprets the
file rather than copying it literally:

| File type | What is sent |
|---|---|
| BASIC program | It is **listed**: the line number is printed as a number, the two length bytes are dropped, and the invisible five-byte number forms the ROM compiles into each line are skipped |
| Open-type (10) | Byte for byte, since it has no structure |
| Anything else | The nine-byte header is dropped, characters above 127 are shown in inverse video, and unprintable characters are replaced by `DVAR 25` (`MSUPC`, normally `.`) |

`DVAR 24` (`MSFLG`) controls the inversion; set it to 1 to print everything except &FF literally.

This is how `MOVE "prog" TO #2` displays a saved BASIC program without loading it, which is the closest thing
MasterDOS has to a `TYPE` command.

---

## OPEN

```text
OPEN #stream, "filename" [IN | OUT | RND]
OPEN DIR "name"
```

Implemented at `OPEN` / `OPEND`. See [open-files.md](open-files.md) for what an open file actually is.

| Mode | Token | Meaning |
|---|---|---|
| `IN` | `FF 60` | Read only. The file must exist |
| `OUT` | &E0 | Write only, from the beginning. Refused if the file exists |
| `RND` | `FF 3C` | Random access, read and write. Creates the file if it does not exist |
| *(none)* | | `OUT` |

`IN` and `OUT` are ordinary tokens; `RND` borrows the ROM's `RND` function token, there being no better one.

```basic
OPEN #4, "d1:data" RND
PRINT #4; "hello"
POINT #4, 0
INPUT #4; a$
CLOSE #4
```

| Refused when | Error |
|---|---|
| The same file is already open on the same drive | *Channel used* |
| The stream is already attached to a non-standard channel | *Stream used* |
| `OUT` on a file that exists | *Writing a read file* |
| `IN` on a file that does not | *File not found* |
| The device is not `D` | *Nonsense* |

**A protected file is forced to read-only** whatever mode was asked for. The file type used for a new open
file is 10, open-type.

`OPEN DIR "name"` creates a subdirectory — see [subdirectories.md](subdirectories.md#creating-a-directory).

---

## CLOSE and CLEAR

```text
CLOSE *stream
CLOSE *
CLEAR #
```

| Command | Effect |
|---|---|
| `CLOSE *4` | Close stream 4: flush the buffer and **write the directory entry back** |
| `CLOSE *` | Close every stream |
| `CLEAR #` | Discard every stream **without touching the disk** |

The difference is flag bit 1. `CLOSE` writes the file's directory entry back — its new length, its sector map,
its date stamp. `CLEAR` abandons it, which is what a program does when it wants the streams gone but the disk
left alone.

> [!IMPORTANT]
> A file written but not closed does not exist. `CLEAR #` after writing loses the work; so does resetting the
> machine. `CLOSE *` before ending a program is not optional.

Streams 0 to 3 are put back to the channels the ROM gives them by default rather than being left unattached.

Note the syntax: `CLOSE` takes `*`, not `#`. `CLEAR` takes `#` and no stream number — it is all or nothing.

---

## POINT

```text
POINT #stream, position
POINT #stream, OVER count
```

Moves the file pointer of an open stream. Implemented at `POINTC` / `FITS`.

| Form | Effect |
|---|---|
| `POINT #4, 0` | Rewind to the start |
| `POINT #4, 30000` | Move to byte 30000. **The position may exceed 65535** — it is evaluated in two halves |
| `POINT #4, OVER 5` | Skip forward over five delimiters |

The delimiter is `DVAR 10` (`DELIM`), normally carriage return — so `POINT #4, OVER 1` advances one line, and
`OVER n` steps through a text file record by record.

**Seeking costs no disk reads.** The position is divided by 510 to give a sector number, and `FITS` finds
which track and sector that is by counting set bits through the file's own sector map, which is already in
the channel. See [open-files.md](open-files.md#random-access).

*End of file* if the position is past the end.

---

## TIME and DATE

```text
TIME
TIME "hh:mm:ss"
DATE
DATE "dd/mm/yy"
```

Implemented at `TIME` / `DATE` / `TIMDC`, sharing one body.

With no argument the value is printed to stream 2. With a string it is set.

**Setting is forgiving.** Six digits are taken from the string, whatever separators it uses, and a short
string is padded with zeros:

| Written | Means |
|---|---|
| `TIME "12:30:00"` | 12:30:00 |
| `TIME "123000"` | The same |
| `TIME "1"` | 01:00:00 |
| `DATE "12/25"` | 25 December, year 00 |

Values are range-checked against limits stored beside them in `DVAR` (`DVAR 90–95` for the date, `DVAR 105–110`
for the time). Out of range gives an *out of range* error.

**A clock is required.** `DVAR 150` (`CKPT`) holds the clock chip's port, &EF by default; **set it to zero
and everything here returns immediately** and the date and time stay at zero. On a machine with no clock
fitted, reads simply produce nothing rather than hanging — `CKLP` retries a bounded number of times if the
chip refuses to hold while updating.

When a clock is present, every file closed is stamped with the date and time (`DATSET`), and `DIR DATE` shows
it.

---

## PROTECT and HIDE

```text
PROTECT [OFF] "pattern"
HIDE [OFF] "pattern"
```

As SAMDOS. `PROTECT` sets bit 6 of the entry's type byte; `HIDE` sets bits 6 and 7 together, so a hidden file
is always protected as well. `OFF` clears them.

A protected file cannot be erased without `OVER`, and cannot be opened for writing — `OPEND` forces
read-only.

---

## LOAD

```text
LOAD n
```

Reached only when the ROM's own `LOAD` syntax has failed, which is how loading by directory number arrives
here. **The number may be two bytes**, so a disk with more than 255 files can still be indexed — which
matters, since MasterDOS allows up to 780 entries.

As in SAMDOS, a 48K snapshot is loaded and resumed rather than returned from, and a BASIC program has its
start and length worked out from the ROM's pointers before the ROM does the loading.

---

## READ and WRITE

```text
READ  AT drive, track, sector, address [, sectors]
WRITE AT drive, track, sector, address [, sectors]
```

Raw sector access, bypassing the directory entirely. Implemented at `READ` / `WRITE` / `EVPRM`.

MasterDOS adds a **fifth argument**, the number of sectors, so a multi-sector transfer is one command rather
than a loop. Everything after the drive is optional and keeps its previous value if omitted.

| Argument | Range |
|---|---|
| drive | 1–7; 3–7 are RAM discs, which work identically here |
| track | 0–79 side 1, 128–207 side 2 |
| sector | 1–10 |
| address | Any address from &4000 up |
| sectors | Defaults to 1 |

> [!WARNING]
> Neither command consults the directory, the sector map or the free space. `WRITE` will overwrite a
> directory track or the middle of a file without complaint.

---

## CALL

```text
CALL MODE 0
CALL MODE 1
```

As SAMDOS: `MODE 0` pages in the Spectrum image and jumps to &B914; `MODE 1` resumes the snapshot the NMI code
saved.

---

## The NMI button

Pressing NMI freezes whatever is running and reads the keyboard (`NMI`):

| Key | Action |
|---|---|
| `1` or `5` | **Call the user vector at `DVAR 27–28` (`NMIKA`) with page `DVAR 26` (`NMIKP`) mapped.** The default vector is &0004, a bare `RET`, so nothing happens and the question is asked again |
| `2` | Resume, having done nothing |
| `3` | Save the screen as a `SCREEN$` |
| `4` | Save a 48K snapshot |
| `X` | Step the page mapped at &8000, so a page other than the default can be captured |

Keys 1 and 5 are MasterDOS's own addition and are the hook a monitor or debugger installs itself on: poke an
address into `DVAR 27–28` and a page into `DVAR 26`, and the NMI button becomes an entry point into it.

The saved file is named from the drive and the directory position it lands in, so successive snapshots do not
collide.

## Extending the command set

`SYNTAX` calls the external vector at `DVAR 33–34` (`ONERR`) for any token it does not recognise. MasterDOS
improves on SAMDOS here: if the address has bit 15 set, the page in `DVAR 36` (`EAPG`) is mapped at &8000
first, so a handler can live in its own page rather than having to fit in the system area.

| On entry | |
|---|---|
| A | The ROM error code |
| `CHADD` | Restored to where the ROM left it |

Returning without acting lets the error stand.
