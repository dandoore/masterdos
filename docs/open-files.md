# MasterDOS 2.3 — Open Files, Streams and Random Access

The part of MasterDOS that SAMDOS has no equivalent of at all. A file can be opened as a stream and read or
written a byte at a time, sequentially or at any position, and BASIC's ordinary `PRINT #`, `INPUT #` and
`INKEY$` then work on it exactly as they do on the screen or the printer.

From part `SER2` of [masterdos23.asm](../annotated-src/masterdos23.asm): `OPEN`, `OPEND`, `CRMCH`, `CLOSE`,
`CLEAR`, `CLSRM`, `MCHRD`, `MCHWR`, `SBYT`, `LBYT`, `POINTC`, `FITS`, `CPPTR`, `SETLEN`, `GLEN`, `D510`,
`M510`.

## Contents

| Section | |
|---|---|
| [Using an open file](#using-an-open-file) | From BASIC |
| [The `D` channel](#the-d-channel) | What `OPEN` actually creates |
| [Modes](#modes) | `IN`, `OUT`, `RND` |
| [Position and length](#position-and-length) | Why both are divided by 510 |
| [Random access](#random-access) | How a seek costs no disk reads |
| [Delimiters](#delimiters) | `POINT ... OVER` and text files |
| [Closing](#closing) | And why it is not optional |
| [Errors](#errors) | |

## Using an open file

```basic
OPEN #4, "d1:data" RND
PRINT #4; "first record"
PRINT #4; "second record"
POINT #4, 0
INPUT #4; a$
PRINT a$
PRINT PTR(#4); " of "; LENGTH #4
CLOSE *4
```

Once open, the stream is an ordinary BASIC stream. Everything that works on stream 2 works on it:

| BASIC | Effect on an open file |
|---|---|
| `PRINT #s; ...` | Write, at the current position |
| `INPUT #s; a$` | Read up to the next carriage return |
| `INKEY$ #s` | Read one character |
| `INP$(#s, n)` | Read exactly $`n`$ characters — see [functions.md](functions.md#inp) |
| `MOVE #s TO ...` | Copy the rest of the file somewhere |
| `EOF(#s)` | 1 at the end |
| `PTR(#s)` | Current position in bytes |
| `LENGTH #s` | The file's length |
| `POINT #s, n` | Move the position |
| `CLOSE *s` | Finish, and write the directory entry back |

Streams 4 to 15 are the ones to use. Streams 0–3 are the ROM's own and are put back to their default channels
when closed.

## The `D` channel

`OPEND` claims **787 bytes** in the ROM's channel area for each open file:

| Part | Size | Contents |
|---|---|---|
| Channel header | 5 | The output routine address, the input routine address, and the channel letter — `D` |
| DOS state | ~14 | Mode flags, drive, buffer pointer, buffer address, next-sector link |
| Directory entry image | 256 | A copy of the file's whole directory entry |
| Sector buffer | 512 | The sector currently being worked on |

Everything the DOS needs to service that file is in one block, so **several files can be open at once** —
limited only by free memory in the channel area.

Key offsets from `IX`, which points at the channel:

| Offset | Field |
|---|---|
| 11 | `MDRV` — the drive |
| 12 | `MFLG` — the flag byte |
| 13–14 | `RPT` — pointer into the sector buffer |
| 15–16 | `BUF` — the sector buffer's address |
| 17–18 | `NSR` — the link to the next sector |
| 19 | `FFSA` — the first byte of the directory entry image, so entry offset $`n`$ is at $`\text{IX} + n + 19`$ |
| 34 | `FSAM` — the file's sector map within that image |

### The flag byte

`MFLG` at offset 12:

| Bits | Meaning |
|---|---|
| 0–1 | Mode: 00 `IN`, 01 `OUT`, 10 `RND` |
| 2 | The file already existed |
| 3 | The sector in the buffer has been written to and must be flushed |
| 5 | The file has been altered at all |

Bit 3 is what makes writing efficient: a sector is only written back when it is about to be replaced, so
sequential writing costs one disk write per 510 bytes rather than one per byte.

### The channel letter marks temporary channels

A channel created by `MOVE` for a named file has bit 7 set in its channel letter (`D`+&80), which is how
`CLTEMP` finds and reclaims them afterwards. `OPDST` clears bit 7 for a channel `OPEN` created, marking it
permanent.

> [!NOTE]
> The source defines a second set of channel-record offsets — `CHBTLO`, `CHBTHI`, `CHREC`, `CHNAME`,
> `CHFLAG`, `CHDRIV`, `RECFLG`, `RECNUM`, `RCLNLO`, `RCLNHI` — describing a record-structured channel with a
> record length and record number. **None of them is referenced anywhere in the released source.** They look
> like a design that was planned and not built. MasterDOS's record handling is done entirely through the byte
> position, with `POINT` doing the arithmetic; there is no fixed record length stored in the channel.

## Modes

| Mode | Spelled | File must exist | Effect |
|---|---|---|---|
| `IN` | `FF 60` | Yes | Read only |
| `OUT` | &E0 | **No** | Write only, from the beginning |
| `RND` | `FF 3C` | No | Read and write at any position; created if absent |
| *(none)* | | | `OUT` |

`IN` and `OUT` are ordinary tokens; `RND` borrows the ROM's `RND` function token, there being no better one.

**Opening an existing file for `OUT` is refused** — *Writing a read file*. To rewrite a file, erase it first
or open it `RND`.

**A protected file is forced to read-only** whatever was asked for.

When the file exists, its directory entry is copied wholesale into the channel — name, type, sector map,
length — and its first sector read. When it does not, the file is created empty, as type 10 (open-type).

`OPEND` walks the existing channels first: **the same file open twice on the same drive is refused**
(*Channel used*), because two channels would each keep their own idea of the file's length and the second
close would undo the first.

## Position and length

Both the pointer and the length are held **divided by 510** — as a sector count plus an offset within the
sector — rather than as plain byte counts.

The length occupies four bytes in the channel's entry image at `IX+LENL` (233), which is entry offset 214:

| Bytes | Field |
|---|---|
| 214–215 | Length modulo 510 |
| 216–217 | Length divided by 510 |

`GLEN` reads them and calls `M510` to produce a plain byte count; `D510` converts the other way.

The reason is that **end of file becomes a four-byte comparison rather than arithmetic**. `CPPTR` compares the
pointer against the length as two 16-bit pairs, which is a handful of instructions; converting both to byte
counts first would need a multiply on every character read.

`PTR` and `LENGTH` pay the conversion cost once, when asked.

Both can exceed 65535 and are stacked as floating-point numbers, so a file larger than 64K is handled
correctly throughout. `POINT` evaluates its argument in two halves (`EVBNUM`) for the same reason.

## Random access

**Seeking costs no disk reads at all.**

A file's own sector address map is already in its directory entry, and the entry is already in the channel.
So finding the file's $`n`$th sector is a matter of counting set bits:

1. `POINT #s, x` divides $`x`$ by 510 to give a sector number $`n`$ and an offset within it.
2. `FITS` walks the 195-byte map at `IX+FSAM`, counting set bits, until it has counted $`n`$ of them.
3. The bit's position converts directly back into a track and sector.
4. That sector is read, and `RPT` set to the offset.

A byte a megabyte into a file is reached in one disk read. Compare the alternative — following the sector
chain from the beginning — which would need one read per 510 bytes.

`FITS` skips whole map bytes that are zero, so the scan is fast even on a file scattered across the disk.

*End of file* if the map runs out of bits before $`n`$ is reached.

### Extending a file

Writing past the end allocates a new sector from the free map, links it into the chain, and sets its bit in
both the global map and the file's own. The length is updated in the channel; it does not reach the disk until
the file is closed.

## Delimiters

```basic
POINT #4, OVER n
```

skips forward over $`n`$ occurrences of the delimiter character in `DVAR 10` (`DELIM`), normally carriage
return. This is how a program steps through a text file record by record without knowing where the records
are.

Two implementations share the work:

| Where | How |
|---|---|
| Within the current sector | A `CPIR` |
| Beyond it | `SRSAD` counts delimiters **during the sector read itself**, so scanning costs no more than reading |

There is one trap, and `PTRC3` corrects for it: the two link bytes at the end of each sector might themselves
happen to equal the delimiter, and `SRSAD` would count them. The check is for exactly that case.

Setting `DVAR 10` to another character makes `OVER` step by that instead — useful for fixed-format files that
use a different separator.

## Closing

```text
CLOSE *4        close one stream
CLOSE *         close all
CLEAR #         discard all, without writing
```

`CLOSE` calls `SDCM`, which writes the file's directory entry back: the new length, the new sector map, the
new sector count, and the [date stamp](disk-format.md#the-date-stamp). `CLEAR` skips all of that — flag bit 1
tells `CLSRM` to abandon rather than write.

> [!IMPORTANT]
> **A file written but not closed does not exist.** Its sectors are allocated only in the in-memory copy of
> the free map; the directory entry has never been written. Resetting, or `CLEAR #`, loses the work — and,
> because free space is derived from the directory, leaves the disk consistent rather than corrupt. That is
> the trade: an abandoned write is safely lost rather than dangerously half-done.

`CLEAR #` also zeroes `SAMCNT`, the count of open files, and the DOS flag byte.

## Errors

| Error | When |
|---|---|
| *Channel used* | The same file is already open on the same drive |
| *Stream used* | The stream is already attached to a non-standard channel. Streams 0–3 are exempt |
| *Writing a read file* | `OUT` on a file that already exists, or a write to a stream opened `IN` |
| *Reading a write file* | A read from a stream opened `OUT` |
| *File not found* | `IN` on a file that does not exist |
| *End of file* | `POINT` past the end, or a read at the end |
| *Nonsense* | The device is not `D` |

## Open files and disk swaps

`SDTKS` notices when the disk in a drive has been changed, by comparing its
[random word](disk-format.md#disk-change-detection). If a file is open on that drive when it happens, the DOS
**beeps and prints `OPEN file`** as a warning.

It cannot refuse — a file may legitimately span a swap, as during a single-drive copy — but writing to a file
whose disk has been changed underneath it will corrupt both disks, so the warning is worth heeding.
