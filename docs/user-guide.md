# MasterDOS 2.3 — User Guide

A practical guide to using MasterDOS from BASIC, with emphasis on what it adds over SAMDOS. The
[command reference](commands.md) and [function reference](functions.md) give the full syntax; this shows how
the pieces fit together.

> [!NOTE]
> Everything here is derived from the source, and names the routine where the behaviour is surprising. The
> [MasterDOS manual](README.md#sources) is the user-facing authority; where the two differ, this describes
> what the code actually does.

## Contents

| Section | |
|---|---|
| [Getting started](#getting-started) | Booting, and checking which DOS you have |
| [What is new](#what-is-new) | The short version |
| [Data files](#data-files) | Open files, records and random access |
| [Subdirectories](#subdirectories) | Organising a disk |
| [RAM discs](#ram-discs) | Drives 3 to 7 |
| [The clock](#the-clock) | Dates on files |
| [Asking questions about the disk](#asking-questions-about-the-disk) | `DSTAT`, `FSTAT`, `DIR$` |
| [Copying and backing up](#copying-and-backing-up) | |
| [Displaying and converting files](#displaying-and-converting-files) | `MOVE` |
| [Bigger directories](#bigger-directories) | And the compatibility cost |
| [Tuning the DOS](#tuning-the-dos) | The pokes worth knowing |
| [Writing portable programs](#writing-portable-programs) | Working under either DOS |
| [Recovering from trouble](#recovering-from-trouble) | |

## Getting started

```basic
BOOT
```

`BOOT` loads whichever DOS is on the disk. To find out which one you got:

```basic
PRINT (PEEK DVAR 7 - 20) / 10
```

2.3 is MasterDOS 2.3, 2.2 is MasterDOS 2.2, and **0 means SAMDOS 2** — none of this guide applies.

If you have a second floppy drive, tell the DOS about it; it does not detect drives:

```basic
POKE DVAR 2, 128+80
```

## What is new

Everything SAMDOS does, MasterDOS does the same way — the disk format is unchanged and your existing disks
work. What is added:

| | |
|---|---|
| **Open files** | `OPEN`, `CLOSE`, `POINT`, `EOF`, `PTR`, `INP$` — read and write a file a byte at a time, at any position |
| **Subdirectories** | `OPEN DIR`, `DIR =`, `PATH$`, and paths in every file name |
| **RAM discs** | Drives 3–7, including external MegaRAM |
| **A clock** | `TIME`, `DATE`, `TIME$`, `DATE$`, and a date stamp on every file |
| **Enquiry functions** | `DSTAT`, `FSTAT`, `FPAGES`, `DIR$` — ask about the disk without provoking an error |
| **`MOVE`** | Copy a byte stream between any two channels |
| **`BACKUP`** | Duplicate a disk, transferring only the sectors in use |
| **Bigger directories** | Up to 780 files instead of 80 |
| **Sorted listings** | `DIR` sorts by default |

## Data files

This is the largest addition, and the reason most people wanted MasterDOS.

```basic
10 OPEN #4, "d1:scores" RND
20 PRINT #4; "Alice"
30 PRINT #4; "Bob"
40 POINT #4, 0
50 INPUT #4; a$
60 PRINT a$
70 CLOSE *4
```

Once open, the stream is an ordinary BASIC stream. `PRINT #`, `INPUT #`, `INKEY$ #` all work on it exactly as
they do on the screen.

### The three modes

| Mode | File must exist | Use for |
|---|---|---|
| `IN` | Yes | Reading |
| `OUT` | **No** — refused if it does | Writing a new file from scratch |
| `RND` | No; created if absent | Anything that reads *and* writes, or seeks |

`RND` is the general-purpose one. `OUT` refusing an existing file catches the common mistake of overwriting
data by accident; to rewrite, erase first or use `RND`.

### Moving around

```basic
POINT #4, 0                  : REM rewind
POINT #4, 1000               : REM go to byte 1000
POINT #4, OVER 5             : REM skip forward five lines
PRINT PTR(#4)                : REM where are we?
PRINT LENGTH #4              : REM how big is the file?
IF EOF(#4) THEN ...          : REM at the end?
```

`POINT ... OVER n` skips over $`n`$ delimiters — carriage return by default — which is how you step
through a text file record by record without knowing where the records are.

**Seeking is free.** The file's own sector map is already in memory, so `POINT` finds the right sector by
counting bits rather than by reading the disk. A byte a long way into a file costs exactly one disk read.

### Fixed-length records

MasterDOS has no built-in record length; you do the arithmetic, and `POINT` makes it cheap:

```basic
10 LET rec = 64
20 OPEN #4, "d1:database" RND
30 REM read record n
40 POINT #4, (n-1)*rec
50 LET r$ = INP$(#4, rec)
60 REM write it back
70 POINT #4, (n-1)*rec
80 PRINT #4; r$
90 CLOSE *4
```

`INP$` reads exactly the number of characters asked for, unlike `INPUT #` which stops at a delimiter — so it
is the right tool for fixed-length records.

Positions may exceed 65535; `POINT` evaluates its argument in two halves for that reason.

### Closing is not optional

> [!IMPORTANT]
> **A file written but not closed does not exist.** The directory entry — the new length, the sector map, the
> date stamp — is only written by `CLOSE`. Resetting, or `CLEAR #`, loses everything written since the file
> was opened.

```basic
CLOSE *4                     : REM one stream
CLOSE *                      : REM all of them
CLEAR #                      : REM discard all, writing nothing
```

Note the punctuation: `CLOSE` takes `*`, `CLEAR` takes `#` and no number.

The compensation is that an abandoned write is *safely* lost rather than half-done — the sectors were only
ever claimed in memory, so the disk stays consistent.

### Several files at once

Each open file gets its own 787-byte channel, so as many can be open as memory allows. The same file cannot be
opened twice on the same drive (*Channel used*) — two channels would each have their own idea of the length,
and the second close would undo the first.

## Subdirectories

```basic
OPEN DIR "GAMES"             : REM create one, here
DIR = "GAMES"                : REM go into it
DIR                          : REM ...and this lists only what is in it
DIR = "^"                    : REM back up one level
DIR = "\"                    : REM back to the root
PRINT PATH$                  : REM 1:\GAMES
```

Paths work **anywhere a file name is accepted**, not only in `DIR =`:

```basic
LOAD "\GAMES\CHESS"
ERASE "d2:\OLD\*.bak"
OPEN #4, "^\DATA\log" RND
```

| Symbol | Meaning |
|---|---|
| `\` or `/` | The root. Both are accepted; the first is what gets written into `PATH$` |
| `^` | The parent directory |

**Whether a path replaces or extends depends on its first character**, and this catches people out:

```basic
DIR = "GAMES"                : REM append:  1:\  becomes  1:\GAMES
DIR = "\GAMES"               : REM replace: whatever it was becomes 1:\GAMES
```

Each drive remembers its own current directory, so switching drives does not lose your place.

### Housekeeping

```basic
ERASE DIR "GAMES"            : REM only works if it is empty
RENAME DIR "GAMES" TO "PLAY"
DIR ?                        : REM list every file on the disk, whatever directory
```

There is no recursive delete — empty a tree from the leaves up. `DSTAT(1,5)` gives the total file count and
`DSTAT(1,6)` the count in the current directory; the difference is what is elsewhere in the tree.

### What it costs

A directory is an ordinary directory entry with a number attached, so the whole tree lives in the flat
catalogue ([subdirectories.md](subdirectories.md)). That means:

* directories cost a directory slot each, and **the 80-entry limit is shared by the whole tree**;
* a directory has no data sectors, so creating one uses no disk space;
* under SAMDOS, every file on the disk is visible regardless of directory, and the directory entries themselves
  appear as unknown-type files. **Do not erase them under SAMDOS** — it orphans everything inside.

## RAM discs

Drives 3 to 7 are RAM discs: 16K pages pretending to be floppies. Everything works on them unchanged — a
directory, subdirectories, a name, a path.

```basic
PRINT FPAGES                 : REM how many free 16K pages?
FORMAT "d3:scratch", 1, 20   : REM 1 directory track, 20 tracks total
COPY "d1:*" TO "d3:*"        : REM fill it
DIR 3
FORMAT "d3:", 0              : REM erase it, returning the pages
```

The arguments are **directory tracks** then **total tracks**. A RAM disc may have as few as one directory
track — 20 files — where a floppy needs four.

Sizing: each track is 10 sectors, and a page holds 31 of them, so a 20-track disc needs
$`\lceil 200 / 31 \rceil = 7`$ pages, about 112K.

**A RAM disc does not survive a reset or power off.** Back it up before switching off:

```basic
BACKUP "d3:x" TO "d1:x"
```

### Making an old program use one

`DVAR 111–117` is an alias table: drive $`n`$ actually means whatever is in the table. So a program
written for two floppies can be pointed at a RAM disc without being changed:

```basic
FORMAT "d3:work", 1, 40
POKE DVAR 112, 3             : REM drive 2 now means RAM disc 3
```

Put it back with `POKE DVAR 112, 2`.

### Checking before you use one

```basic
IF DSTAT(3,1) = -1 THEN FORMAT "d3:temp", 1, 20
```

`DSTAT` returns −1 for an unformatted RAM disc rather than raising an error, so this is safe to run every time.

## The clock

```basic
TIME                         : REM print it
TIME "14:30:00"              : REM set it
DATE "09/08/26"
PRINT TIME$, DATE$
```

Setting is forgiving: six digits are taken from the string whatever separators it uses, and short strings are
padded with zeros. `TIME "1"` sets one o'clock; `DATE "12/25"` sets 25 December with the year left at zero.

With a clock fitted, **every file closed is stamped** with the date and time, and:

```basic
DIR DATE
```

shows them. The stamp lives in five bytes of the file header's comment area
([disk-format.md](disk-format.md#the-date-stamp)).

If no clock is fitted, set `POKE DVAR 150, 0` so the clock code returns immediately instead of retrying the
chip.

## Asking questions about the disk

The enquiry functions are MasterDOS's quiet improvement: **none of them raises an error**, so a program can
check before acting instead of trapping afterwards.

```basic
10 IF DSTAT(1,1) = -1 THEN PRINT "No disk in drive 1": STOP
20 IF DSTAT(1,2) THEN PRINT "Write protected": STOP
30 IF DSTAT(1,3) < 20000 THEN PRINT "Not enough room": STOP
40 IF DSTAT(1,4) = 0 THEN PRINT "Directory full": STOP
50 IF FSTAT("data",1) = 0 THEN PRINT "No data file": STOP
60 PRINT "Data file is ";FSTAT("data",2);" bytes"
```

| Call | Answer |
|---|---|
| `DSTAT(d,1)` | Free bytes, or 0 if write protected or no free slots |
| `DSTAT(d,2)` | 1 if write protected |
| `DSTAT(d,3)` | Free bytes regardless of protection |
| `DSTAT(d,4)` | Free directory slots |
| `DSTAT(d,5)` | Total files |
| `DSTAT(d,6)` | Files in the current directory |
| `DSTAT(d,8)` | The current drive |
| `FSTAT(f$,1)` | Directory number, or 0 if absent |
| `FSTAT(f$,2)` | Length in bytes |
| `FSTAT(f$,3)` | Type |
| `FSTAT(f$,4)` | Type with the protect and hidden bits |

**−1 from `DSTAT` always means "drive not ready"** — no disk, or an unformatted RAM disc.

### Processing the catalogue

`DIR$` returns the whole catalogue as one string, ten characters per name:

```basic
10 LET c$ = DIR$("*.bas")
20 FOR i = 1 TO LEN c$ STEP 10
30   PRINT c$(i TO i+9)
40 NEXT i
```

So $`\text{LEN DIR\$} / 10`$ is the file count, and the $`n`$th name is `DIR$(n*10-9 TO n*10)`.
Trailing spaces are part of each name.

## Copying and backing up

```basic
COPY "*" TO "d2:*"           : REM every file
COPY "*.bak" TO "*.old"      : REM rename in bulk
BACKUP "d1:" TO "d2:"        : REM duplicate the whole disk
```

**`COPY` is limited by disk space, not memory.** A file bigger than any free block of RAM is copied in passes,
so a 300K file copies on a machine with 32K free — just more slowly.

**`BACKUP` only transfers the sectors actually in use.** It finds the highest used sector from the free-space
map and skips everything above it, so a lightly filled disk copies quickly. The copy is given a fresh identity
rather than the source's, so the two disks are distinguishable.

With one drive, both prompt for swaps.

## Displaying and converting files

`MOVE` copies a byte stream between any two channels:

```basic
MOVE "readme" TO #2          : REM display a text file
MOVE "prog" TO #2            : REM LIST a saved BASIC program without loading it
MOVE "notes" TO #3           : REM print it
MOVE #2 TO "d1:capture"      : REM capture screen output to a file
MOVE "d1:x" TO "d2:x"        : REM copy a file
```

The second one is worth knowing: moving a **BASIC program** to the screen lists it — the line numbers are
printed as numbers, the length bytes dropped, and the invisible compiled number forms skipped. It is the
closest thing to a `TYPE` command.

For any other type, the nine-byte header is dropped, characters above 127 are shown inverse, and unprintable
ones are replaced by `.` (settable with `DVAR 25`).

## Bigger directories

```basic
FORMAT "d1:work", 10
```

gives ten directory tracks — 200 files instead of 80. The maximum is 39 tracks, 780 files.

The cost is paid twice: the extra tracks are not available for data, and the map that tracks free sectors still
starts at the first data track, so some of it goes unused.

> [!WARNING]
> **A disk with more than four directory tracks must not be written by SAMDOS.** SAMDOS's directory size is a
> fixed four tracks, so it will treat the extra ones as free space and allocate file data over your directory.
> Reading is safe; writing destroys it. A four-track MasterDOS disk is completely safe under SAMDOS.

## Tuning the DOS

`DVAR n` gives the **address** of variable $`n`$, so always `PEEK` or `POKE` it. The ones worth knowing:

```basic
POKE DVAR 2, 128+80          : REM enable the second floppy drive
POKE DVAR 0, 0               : REM stop the border flashing
POKE DVAR 9, 0               : REM unsorted DIR, in directory-number order
POKE DVAR 10, CODE "*"       : REM change the POINT OVER delimiter
POKE DVAR 12, CODE "/"       : REM use / as the root symbol in PATH$
POKE DVAR 112, 3             : REM drive 2 means RAM disc 3
POKE DVAR 150, 0             : REM no clock fitted
```

`DVAR 9` matters more than it looks: `DIR` sorts by default, so the numbers shown are not in the order `LOAD n`
expects. Turn sorting off when you want to load by number.

The full list of 150-odd variables is in [dos-variables.md](dos-variables.md).

## Writing portable programs

A program that must work under either DOS should test first:

```basic
10 LET dos = PEEK DVAR 7
20 IF dos < 42 THEN PRINT "This program needs MasterDOS": STOP
```

Under SAMDOS, `OPEN #`, `CLOSE #`, `EOF`, `PTR` and `PATH$` are silently no-ops — they raise no error and do
nothing, so failure happens later and confusingly. MasterDOS's own keywords (`TIME$`, `FSTAT` and the rest) do
not tokenise at all under SAMDOS, so a program using them cannot even be typed in.

**Error codes differ.** MasterDOS reports end-of-file as the ROM's code **22**, where SAMDOS would use 108; the
same applies to invalid file names (18, not 89) and syntax errors in a DOS command (29, not 81). A program that
traps by code needs to know which DOS it is under. See [errors.md](errors.md#why-so-many-entries-are-blank).

## Recovering from trouble

**"OPEN file" with a beep** — the disk in a drive was changed while a file was open on it. MasterDOS detects
this by comparing the disk's random identity word, and warns rather than refusing, because a swap is sometimes
legitimate. **Writing after this will corrupt both disks.** Close the file, put the right disk back, and start
again.

**"Directory not empty"** — `ERASE DIR` refuses a directory with anything in it, including hidden files. Use
`DIR = "name"` and `DIR` to see what is there.

**"Channel used"** — the file is already open. `CLOSE *` and try again.

**"No pages free"** — `COPY`, `BACKUP` or a RAM disc format found no free memory. `PRINT FPAGES` to confirm,
and erase a RAM disc or `CLEAR` to release some.

**"TRK-nnn,SCT-nn,Error"** — the sector named could not be transferred after the retry limit. The rest of the
disk is fine; `COPY "*" TO "d2:*"` will rescue everything except the affected file.

**A file deleted by mistake** — the data and the file's complete sector map are still in the directory entry
that was zeroed; `ERASE` writes one byte. Restoring the type byte with `READ AT` / `WRITE AT` recovers the
file, provided nothing has been written to the disk since. See
[the SAMDOS guide](https://github.com/stefandrissen/samdos) for the procedure, which is identical.

**The current directory suddenly changed to the root** — that is deliberate. Swapping the disk resets it,
because the directory tag you were in belonged to a different disk's tree.
