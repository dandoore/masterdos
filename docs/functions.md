# MasterDOS 2.3 — Functions

The BASIC functions MasterDOS provides: the seven it adds to the language, and the five ROM functions whose
work it does through hooks.

## Contents

| Function | Result | |
|---|---|---|
| [`TIME$`](#time-and-date) | String | The time as `hh:mm:ss` |
| [`DATE$`](#time-and-date) | String | The date as `dd/mm/yy` |
| [`INP$`](#inp) | String | Characters read from a stream |
| [`DIR$`](#dir) | String | The catalogue, ten characters per name |
| [`FSTAT`](#fstat) | Number | A file's number, length, type or flags |
| [`DSTAT`](#dstat) | Number | A drive's free space, free slots, file count, readiness |
| [`FPAGES`](#fpages) | Number | Free 16K pages in the machine |
| [`PATH$`](#path) | String | The current path — see [which drive](#which-drive-path-reports-on) |
| [`DVAR`](#dvar) | Number | The address of a DOS variable |
| [`EOF`](#eof-and-ptr) | Number | End of file on a stream |
| [`PTR`](#eof-and-ptr) | Number | Position within an open file |
| [`LENGTH`](#length) | Number | A ROM function MasterDOS patches |

## New keywords

SAMDOS could only claim tokens the ROM already had. MasterDOS adds real keywords by repointing four ROM
vectors at boot (`INIP3`):

| ROM vector | Address | What MasterDOS does with it |
|---|---|---|
| `PRTOKV` | &5ADE | Hook 169 — print one of MasterDOS's tokens |
| `MTOKV` | &5AFA | Hook 171 — match one of MasterDOS's keywords while tokenising |
| `EVALUV` | &5AF6 | Hook 172 — evaluate one of MasterDOS's functions |
| `CMDV` | &5AF4 | Hook 173 — dispatch a command |

The keyword list is `KWMT`. Its functions are **two-byte tokens** with an &FF prefix, like the ROM's own
functions; its commands are single bytes.

| Token | Keyword | Type |
|---|---|---|
| `FF 30` | `TIME$` | String function |
| `FF 31` | `DATE$` | String function |
| `FF 32` | `INP$` | String function |
| `FF 33` | `DIR$` | String function |
| `FF 34` | `FSTAT` | Numeric function |
| `FF 35` | `DSTAT` | Numeric function |
| `FF 36` | `FPAGES` | Numeric function |
| 247 | `BACKUP` | Command |
| 248 | `TIME` | Command |
| 249 | `DATE` | Command |

`HGTTK` gets the &FF prefix into the line by copying a few instructions into a buffer and re-entering the
ROM's tokeniser seventeen bytes further on — the ROM's tokeniser has no provision for a two-byte token from
outside its own table.

The source shows keywords that were planned and dropped, commented out: `SCRAD` (`FF 37`), `ALTER` (250) and
`SORT` (251).

---

## `TIME$` and `DATE$`

```basic
PRINT TIME$        : REM 14:32:07
PRINT DATE$        : REM 09/08/26
```

No arguments. Both return an eight-character string, read fresh from the clock chip each time (`RDCLK`) and
formatted from the buffers in `DVAR` — `DVAR 81–88` for the date, `DVAR 96–103` for the time.

With no clock fitted (`DVAR 150` set to zero) both return `00/00/00` and `00:00:00`.

The strings are the `DVAR` buffers themselves, so a program can read the fields directly:

```basic
PRINT VAL TIME$(1 TO 2)     : REM the hour
```

---

## `INP$`

```basic
INP$(#stream, count)
```

Reads `count` characters from a stream and returns them as a string. Implemented at `INPST`.

```basic
OPEN #4, "data" IN
LET header$ = INP$(#4, 16)
CLOSE *4
```

| Limit | |
|---|---|
| stream | 0 to 16, else *Invalid stream* |
| count | 1 to 16384, else *out of range* |

This is the fixed-length counterpart to `INPUT #`, which stops at a delimiter. It is the natural way to read a
record from a file opened `RND`, paired with [`POINT`](commands.md#point).

---

## `DIR$`

```basic
DIR$
DIR$("pattern")
```

Returns the catalogue as one long string, **ten characters to a name**, with no separators. Implemented at
`FNDIRS`.

```basic
LET c$ = DIR$("*.bas")
FOR i = 1 TO LEN c$ STEP 10
  PRINT c$(i TO i+9)
NEXT i
```

It is built by the same routine that produces the short `DIR` listing (`DITOB`), collected in the screen page,
so it is affected by the same `DVAR` settings — in particular `DVAR 9` (`SRTFG`) decides whether the names come
back sorted.

Because every name is exactly ten characters, $`\text{LEN DIR\$} / 10`$ is the number of files, and the
$`n`$th name is `DIR$(n*10-9 TO n*10)`. Trailing spaces are part of each name.

Like `DIR`, it lists only the current directory unless a pattern says otherwise.

---

## FSTAT

```basic
FSTAT("name", n)
```

Information about one file, without loading it. Implemented at `FSTAT`.

| `n` | Returns |
|---|---|
| 1 | The file's **directory number**, as `DIR` prints it and `LOAD` accepts it |
| 2 | The file's **length** in bytes |
| 3 | The file's **type**, 0–31, with the protect and hidden bits masked off |
| 4 | The type byte **including** the protect and hidden bits |

| Special result | Meaning |
|---|---|
| 0 | The file does not exist |
| −1 | There is no disk in the drive, or the RAM disc is not formatted |

`n` outside 1–4 gives *out of range*.

```basic
IF FSTAT("save.dat", 1) = 0 THEN PRINT "no save file"
IF FSTAT("x", 4) >= 64 THEN PRINT "protected"
```

**The length** comes from the page-form field at directory offset &EF (239), which is the ROM header's length —
so it is the file's true content length, not its size on disk. A 48K snapshot is a special case and always
reports 49152. The result is stacked as a floating-point number, so lengths above 65535 are returned correctly.

**Type 3 versus 4.** Bit 6 is protect and bit 7 is hidden, so `FSTAT(f$,4) - FSTAT(f$,3)` is 0, 64 or 192.

---

## DSTAT

```basic
DSTAT(drive, n)
```

The state of a drive. Implemented at `DSTAT`.

| `n` | Returns |
|---|---|
| 1 | Free space in bytes, **or 0 if the disk is write protected or has no free directory slots** |
| 2 | 1 if write protected, 0 if not |
| 3 | Free space in bytes, regardless of protection |
| 4 | Free directory slots |
| 5 | Total files on the disk |
| 6 | Files in the current directory |
| 7 | The number of directory tracks (`DTKS`) |
| 8 | The current drive number |

**Every enquiry returns −1 if the drive is not ready** — no disk, or an unformatted RAM disc. That is what
makes `DSTAT` the right way to test for readiness: it does not raise an error.

```basic
IF DSTAT(1,1) = -1 THEN PRINT "no disk in drive 1": STOP
IF DSTAT(1,1) < 10000 THEN PRINT "not enough room"
```

Free space is $`\text{sectors} \times 510 - 9`$ — the nine bytes being the file header a new file will
need. `HOCHK` decides readiness: for a real drive it looks for the index hole, which is only present if a disk
is in and turning; `DSTAT(2,n)` short-circuits to −1 if `DVAR 2` says no second drive is fitted.

**Write protection is discovered by trying to write.** `WPCHK` issues a write to an impossible sector: the
command fails at once, but the controller's status still reports the protect line. A RAM disc is never
protected.

`n = 8` ignores the drive argument and returns the current drive. `n` above 7, other than 8, gives
*out of range*.

---

## FPAGES

```basic
PRINT FPAGES
```

The number of free 16K pages in the machine — internal RAM plus any MegaRAM. No arguments. Implemented at
`FPAGES` / `CNTFP`, which counts the MegaRAM pages from the bitmap in `DVAR 118–149` and the internal ones
from the ROM's allocation table at &5100.

This is what a program checks before creating a RAM disc, and what `COPY` and `BACKUP` use to size their
passes. See [ram-discs.md](ram-discs.md).

---

## `PATH$`

```basic
PRINT PATH$
```

Returns the current path as a string, for example `1:\GAMES\CHESS`. **It takes no argument.**

`PATH$` is a **ROM** function, token &4F, not one of MasterDOS's. The ROM implements it as a single hook call
(`IMPATHS` in the ROM's `rom1fns.asm`):

```asm
IMPATHS:   CALL SABORTER
           RST &08
           DB 142            ; PATHHK
```

`SABORTER` abandons the syntax pass, so there is nothing to parse and no argument is possible. Under SAMDOS 2
hook 142 is a bare `RET` and `PATH$` returns nothing useful; MasterDOS implements it at `HPATH`.

### Which drive `PATH$` reports on

Since there is no argument, the drive is decided entirely by the DOS. `HPATH` is two calls:

```asm
HPATH:         CALL GTDEF      ; decide the drive
               CALL GPATD      ; fetch that drive's path
```

**Step 1 — `GTDEF` picks the drive** from the ROM's `DEVICE` setting, which lives in the two bytes `PSLD` at
&5A06–&5A07 (letter, then number):

| `DEVN` (&5A07) | What `GTDD` does |
|---|---|
| 0 | Use `DVAR 15` (`ODEF`), the DOS's own default drive |
| 1 to 8 | Use it as the drive number |
| 9 or more | **Assume it is a tape speed, not a drive**, and use `ODEF` |

That last rule exists because `DEVICE` shares `PSLD` with the tape system: `DEVICE T45` puts 45 there as a
tape speed, and the standard tape speed is 112. Anything above the drive limit (`RDLIM`, 8) is therefore taken
as "not a drive at all".

The device **letter** at &5A06 is read into `LSTR1` but never tested by `PATH$`. So after `DEVICE T:`,
`PATH$` still returns a disk path — the letter is ignored on this route.

**Step 2 — the choice is committed.** `GTDF3` writes the chosen number back into `ODEF`, so *evaluating
`PATH$` changes the DOS's default drive* as a side effect. It then passes the number through:

| Routine | Effect |
|---|---|
| `CODN` | Maps the drive through `DRPT` (`DVAR 111–117`), the alias table. If drive 2 has been pointed at RAM disc 3, `PATH$` reports RAM disc 3's path |
| `DRSET` | Sets `DRIVE`, and loads `CDIRT` from that drive's entry in `CDIT` — each drive remembers its own current directory |

**Step 3 — `GPATD` fetches the path** for whatever `DRIVE` now holds:

| Drive | Where the path comes from |
|---|---|
| 1 | `PTH1` in the DOS's page |
| 2 | `PTH2` |
| 3–7 | Fetched by `MRDPN` out of the RAM disc's own first page into the buffer `PTHRD`, because a RAM disc keeps its path with itself rather than in the DOS |

The length in every case is that drive's byte in `PLT` (`DVAR 56–62`), which defaults to 2 — the length of
`"1:"`.

### In short

> `PATH$` reports on the drive named by the last `DEVICE` command, unless that number is 0 or looks like a
> tape speed, in which case it reports on the DOS's own default drive (`DVAR 15`). Either way the number is
> then run through the drive alias table, and evaluating `PATH$` sets the default drive to whatever it chose.

To read another drive's path, set the device first:

```basic
DEVICE D2: PRINT PATH$
```

There is no way to ask for a drive's path without also making it the default.

### Setting the path

`DIR = "path"` changes it — see [subdirectories.md](subdirectories.md#changing-directory). The path string is
built as the tree is walked, using the first of the two root symbols in `DVAR 12–13` (`RTSYM`, normally `\`
and `/`); both are accepted on input, and the first is always what gets written.

---

## DVAR

```basic
PEEK DVAR n
POKE DVAR n, x
```

**`DVAR n` returns the *address* of variable $`n`$, not its value.** The address is the DOS's page ×
16384 plus the offset, which needs more than sixteen bits, so it comes back as a floating-point number. See
[dos-variables.md](dos-variables.md).

---

## EOF and PTR

```basic
EOF(#stream)
PTR(#stream)
```

Both are ROM functions serviced by hooks — 140 and 141. Both are bare `RET`s in SAMDOS 2 and real in
MasterDOS.

| Function | Returns |
|---|---|
| `EOF` | 1 if the pointer has reached the end of the file, 0 if not |
| `PTR` | The current position in bytes, counted from 0 |

Both work on a stream opened with [`OPEN`](commands.md#open). `PTR` can exceed 65535 and is stacked as a
floating-point number.

Internally the pointer and the length are both stored **divided by 510**, as a sector count plus an offset,
which makes end-of-file a four-byte comparison rather than arithmetic. `D510` and `M510` convert to and from a
plain byte count for these two functions. See [open-files.md](open-files.md).

---

## LENGTH

`LENGTH` is a ROM function, but the ROM's version has a bug when the string it is measuring crosses a page
boundary. MasterDOS does not reimplement it: `FNLENG` finds the ROM's routine through the ROM's own function
table, copies 120 bytes of it into a buffer at &8D80 with ROM 1 paged in, patches fifteen bytes over the
faulty part (`PDATA`, which steps the page on and winds the address back when it passes &C000), and calls the
copy.

`LENGTH #stream` has no ROM equivalent and is handled separately, returning the length of the open file.

This is worth knowing for two reasons: `LENGTH` behaves differently with MasterDOS loaded than without, and
the patched copy lives at a fixed address that a program must not disturb.
