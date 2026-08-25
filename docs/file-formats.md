# MasterDOS 2.3 — File Types and File Formats

What MasterDOS stores at the front of a file, what each type's data contains, and how the ROM's header relates
to the DOS's. Derived from `SVHD`, `LDHD`, `HCONR`, `GTFLE`, `GTFSR`, `OFSM`, `CFSM`, `PNTYP` and `DRTAB` in
[masterdos23.asm](../annotated-src/masterdos23.asm).

The 48-byte header itself belongs to the ROM; it is documented in full in the ROM repository's
`docs/file-formats.md`.

## File types

The type is bits 0–4 of the first byte of the directory entry, and again byte 0 of the nine-byte header. Bits
6 and 7 are the protect and hidden flags and are not part of the type.

| Type | `DIR` shows | Meaning |
|---|---|---|
| 0 | — | Entry is free |
| 1 | `ZX BASIC` | ZX Spectrum BASIC |
| 2 | `ZX D.ARRAY` | ZX numeric array |
| 3 | `ZX $.ARRAY` | ZX string array |
| 4 | `ZX` | ZX code |
| 5 | `ZX SNP 48K` | ZX 48K snapshot |
| 6 | `MD.FILE` | Microdrive file |
| 7 | `ZX SCREEN$` | ZX screen |
| 8 | `SPECIAL` | Special |
| 9 | `ZX SNP 128K` | ZX 128K snapshot |
| 10 | `OPENTYPE` | Open-ended file — what [`OPEN`](commands.md#open) creates |
| 11 | `N/A EXECUTE` | Execute |
| 12–15 | `WHAT?` | Not used by MasterDOS |
| 16 | `BASIC` | SAM BASIC program |
| 17 | `D.ARRAY` | SAM numeric array |
| 18 | `$.ARRAY` | SAM string array |
| 19 | `C` | SAM code |
| 20 | `SCREEN$` | SAM screen |
| **21** | `DIR` | **Subdirectory** — MasterDOS's own, `DFT` |

`DRTAB` builds these from shared substrings — `ZXS`, `ARRAY`, `SCREENS`, `WHAT` are indices into the ROM's
dictionary, which is why the table is so compact.

**[external]** The MGT filesystem documentation lists further types used by other DOSes on the same format —
22–23 SAM Driver, 24–26 EDOS, 28–31 HDOS. MasterDOS shows all of them as `WHAT?`.

### Type conversion on the way in

`GTFSR` normalises the type byte as a file is opened. A Spectrum `SCREEN$` (7) is treated as Spectrum code
(4), and any type below 16 is moved into the SAM range so the ROM sees a type it understands, with a flag bit
remembering that it was really a Spectrum file. Code and `SCREEN$` are interchangeable for the type check, so
`LOAD "x" CODE` will load a `SCREEN$`.

Type 21 is checked separately by `POIDFT`, which returns Z if the entry is a directory — every command that
must not treat a directory as a file uses it.

## Two headers

A file carries its identity twice:

| | Nine-byte header | 48-byte header |
|---|---|---|
| Whose | The DOS's, inherited from GDOS | The ROM's |
| Where | The first 9 bytes of the file's data, **and** directory entry bytes 211–219 | Directory entry bytes 220–252, holding ROM header bytes 15–47 |
| Numbers | Plain 16-bit plus separate page bytes | Page form |
| Read by | The DOS, and Spectrum-era tools | The ROM |

`SVHD` writes both from the same buffer, so they agree.

## The nine-byte header

| Offset | Size | Field | Source | Contents |
|---|---|---|---|---|
| 0 | 1 | Type | `HD001` | One of the types above |
| 1–2 | 2 | Length | `HD0B1` | Bytes within the last page, low first, masked to 14 bits |
| 3–4 | 2 | Start | `HD0D1` | Start address, low first, in the &8000–&BFFF window |
| 5–6 | 2 | Execute | `HD0F1` | **Always &FFFF — see below.** Named for an execution address, but never written |
| 7 | 1 | Length pages | `PGES1` | Whole 16K pages |
| 8 | 1 | Start page | `PAGE1` | Page the start address is in |

$`\text{length} = \text{pages} \times 16384 + \text{length}_{16}`$

> [!IMPORTANT]
> The nine bytes are part of the file's data. A file of $`n`$ bytes occupies $`n + 9`$ on disk, and
> loading starts at byte 9. This is also why `DSTAT(d,3)` subtracts 9 from the free space it reports — the
> next file will need them.

### There is no execution address here

`HD0F1` and `HD0F2` appear **only in their own `DEFW` lines** — nothing in MasterDOS ever writes them and
nothing ever reads them, exactly as in SAMDOS. `HCONR` takes the type, start, start page, length and length
pages out of the ROM's 48-byte header and skips the exec field; `RESREG` has already filled the parameter
block with &FF, so `SVHD` always copies &FFFF out.

The reason is inherited. These nine bytes are the DISCiPLE/+D header, which is a **ZX Spectrum tape header** —
and a Spectrum is a 64K machine, so nothing in it needed a page byte:

| Byte | GDOS meaning | What the SAM DOSes do with it |
|---|---|---|
| 0 | Type | Type |
| 1–2 | Length | Length, low 14 bits |
| 3–4 | Start address | Start address |
| 5–6 | Type-specific — for BASIC, the length without variables | *Nothing* |
| 7–8 | Autostart line or address | **Length pages**, then **start page** |

SAM needs 20 bits to reach 512K, and SAMDOS found them by spending the **last word** — the Spectrum's
autostart field — on two page bytes. That word is exactly where an execution page byte would have had to go,
and there was no tenth byte to extend into without breaking the format. MasterDOS inherited the arrangement
unchanged, which is part of why its disks stay readable by SAMDOS.

Nothing is lost by it. **A SAM file's real execution address is in the ROM's 48-byte header at directory
offsets 242–244**, in full page form — that is what the ROM reads back from `HDL` and what runs the code.

> [!IMPORTANT]
> A tool reading the nine-byte header will find &FFFF in bytes 5–6 for every file either DOS has written,
> whatever the file's real execution address. Take it from directory offsets 242–244 instead.

## The 48-byte header

Built by the ROM at `HDR` (&4B00) before a save, expected back at `HDL` (&4B50) after a load.

| Offset | Size | Field |
|---|---|---|
| 0 | 1 | Type |
| 1–10 | 10 | Name |
| 11–14 | 4 | Name extension |
| 15 | 1 | Flags: bit 0 invisible, bit 1 protected |
| 16–26 | 11 | Type-specific |
| 27 | 1 | Directory entry number |
| 28–30 | 3 | Spare |
| 31–33 | 3 | Start, page form |
| 34–36 | 3 | Length, page form |
| 37–39 | 3 | Execute, page form |
| 40–79 | 40 | Comment |

Only bytes 0–47 reach the disk. **Bytes 48–79 of the comment area are not stored.** MasterDOS uses bytes
40–44 of the comment — directory offsets 245–249 — for its [date stamp](disk-format.md#the-date-stamp), so a
program that writes a comment there will find it overwritten when the file is closed.

## Page form

$`\text{value} = \text{pages} \times 16384 + \left( (\text{high} \times 256 + \text{low}) \bmod 16384 \right)`$

Written **pages, low, high**. The high byte usually has bit 7 set, being an address in the &8000 window, so a
reader must mask bits 6 and 7 before taking the remainder. A first byte of &FF means "not given".

## What the data contains

After the nine-byte header:

| Type | Data |
|---|---|
| 16, BASIC | One contiguous block from `PROG` to `ELINE-1`: the tokenised program, then the numeric variables, the gap, and the strings and arrays. The three boundaries are **not** stored — ROM header bytes 16–24 hold the distances from `PROG` to each. See [below](#basic-programs-what-travels-and-what-does-not) |
| 17 / 18, array | The array's contents; the 11-byte record header is in ROM header bytes 16–26 |
| 19, code | The bytes, verbatim |
| 20, `SCREEN$` | Screen memory, verbatim; the mode is in ROM header byte 16 |
| 10, open-type | Arbitrary bytes. The length is tracked in the directory entry and updated as the file is written |
| **21, directory** | **Nothing.** A subdirectory has no data sectors at all — see [subdirectories.md](subdirectories.md) |

Nothing is compressed or checksummed.

[Every type in detail](#every-type-in-detail) below gives the byte offsets for each of these, and for the
types this table leaves out.

### BASIC programs: what travels, and what does not

None of this is the DOS's doing — the ROM builds the block and MasterDOS stores it like any other. But a tool
reading a `.mgt` image has to understand it. The full account is in the ROM repository's `docs/memory-map.md`
and `docs/file-formats.md`.

BASIC's variable area is a chain of contiguous regions, each boundary a system-variable pointer. `SAVE` writes
the first four and stops:

| Region | In the file? | Notes |
|---|---|---|
| Program (`PROG`) | Yes | Tokenised lines, ending with the &FF program terminator |
| Numeric variables (`NVARS`) | Yes | 26 letter-chain roots, 52 bytes, then the records — so **`SAVE` after `RUN` carries the variables with it** |
| The gap (`NUMEND`) | Yes, as it stood | Free slack between numbers and strings |
| Strings and arrays (`SAVARS`) | Yes, **except the final &FF** | Re-planted on load, which is why the length stops one byte short of `ELINE` |
| Edit line (`ELINE`) | No | |
| Workspace (`WORKSP`) | No | |

**The three boundaries are not stored.** ROM header bytes 16–18, 19–21 and 22–24 hold the distances from
`PROG` to `NVARS`, `NUMEND` and `SAVARS` in page form; the ROM adds each to `PROG` after loading, so a program
image is freely relocatable.

**Some state is discarded rather than restored:** the BASIC stack is emptied, the `DATA` pointer reset, and
the FN/PROC calling buffers inside the program — which hold the addresses it had when saved — are recomputed
by the ROM's compile pass.

MasterDOS adds one wrinkle. Its [hook 173](hook-interface.md#the-basic-extension) patches the ROM's own
`SAVE`/`LOAD`/`MERGE`/`VERIFY` in a buffer before running them: the name fetch is redirected so a path longer
than fifteen characters survives, and `MERGE` is given a check that opens more room between `NUMEND` and
`SAVARS` when a program with many numeric variables would not otherwise fit. The file format is unchanged;
only the ROM's handling of it is corrected.

## Every type in detail

The nine-byte header's fields do not mean the same thing for every type, and the data block's layout varies.
This section gives both, per type, with byte offsets.

Two conventions throughout:

* **Entry offset** means a byte offset into the file's 256-byte directory entry.
* **Data offset** means an offset into the file's data as read by following the sector chain, *including* the
  nine-byte header where one is present.

### Types 1–4, 6, 8, 11 — Spectrum files

MasterDOS stores these mostly so a SAM can hold material for a Spectrum emulator. It does not interpret them.

Their numbers live in the **nine-byte header**, not in the ROM's 48-byte one. `GTFSR` normalises the type as
the file is opened — a Spectrum `SCREEN$` (7) becomes Spectrum code (4), and anything below 16 is moved into
the SAM range with a flag bit remembering what it really was — so the ROM sees a type it understands.

| Type | Data block |
|---|---|
| 1 ZX BASIC | Nine-byte header, then the Spectrum BASIC program and its variables |
| 2 ZX numeric array | Nine-byte header, then the array as the Spectrum stored it |
| 3 ZX string array | As above |
| 4 ZX code | Nine-byte header, then the bytes |
| 6 Microdrive file | Nine-byte header, then the bytes. Never produced by MasterDOS |
| 8 Special | Nine-byte header, then the bytes. No defined meaning |
| 11 Execute | Nine-byte header, then the bytes |

### Type 5 — ZX Spectrum 48K snapshot

The only type whose directory entry carries **processor registers**, and the only one written **without a
nine-byte header**.

#### The data block

| Data offset | Size | Contents |
|---|---|---|
| 0 | 49152 | Spectrum RAM &4000–&FFFF, verbatim |

**There is no nine-byte header.** `SNAP6` branches around the `SVHD` call for this type, and `SVHD` is what
writes those nine bytes both into the entry and into the file. So a 48K snapshot's data begins immediately
with the byte that was at Spectrum &4000, and entry bytes 211–219 are left at the zero `OFSM` cleared them to.

The image is 49152 bytes, so a snapshot occupies $`\lceil 49152 / 510 \rceil = 97`$ sectors.

#### The register dump

The 22 bytes at **entry offsets 220–241** are the machine's registers, in the order `NMI` pushed them. Every
pair is little-endian — low byte first — because each was written by a `PUSH`.

| Entry offset | Size | Register |
|---|---|---|
| 220–221 | 2 | `IY` |
| 222–223 | 2 | `IX` |
| 224–225 | 2 | `DE'` |
| 226–227 | 2 | `BC'` |
| 228–229 | 2 | `HL'` |
| 230–231 | 2 | `AF'` |
| 232–233 | 2 | `DE` |
| 234–235 | 2 | `BC` |
| 236–237 | 2 | `HL` |
| 238 | 1 | Flags — **not the program's `F`**; see below |
| 239 | 1 | `I` |
| 240–241 | 2 | `SP` |

`SNAP7`, which resumes a snapshot, pops them back in exactly that order, which is the definitive confirmation
of the layout. This is identical to SAMDOS — MasterDOS inherited the code unchanged.

> [!IMPORTANT]
> **`A` is not in the block, and byte 238 is not the program's flags.** `NMI` executes `LD A,I` before its
> first `PUSH AF`, which destroys `A`. So the byte in the `A` position holds **`I`**, and byte 238 holds
> whatever `LD A,I` left in `F`.
>
> That is deliberate rather than a bug: `LD A,I` copies **`IFF2` into the P/V flag (bit 2)**, so byte 238's
> bit 2 is the interrupt-enable state at the moment the button was pressed. The instruction also sets S and Z
> from `I`, clears H and N, and leaves C untouched.

**The interrupt mode is not stored either.** `SNAP7` infers it from `I`: a value of 0 or &3F means `IM 1`,
anything else means `IM 2`.

**[external]** The MGT filesystem documentation gives the same order for this block — *"IY, IX, DE', BC',
HL', AF', DE, BC, HL, junk, I, SP"* — which is an independent confirmation, and calls byte 238 "junk" for the
reason above. It adds that the real `AF`, `PC` and `R` live on the **interrupted program's own stack**, at the
address in the saved `SP`, in the order *"F indicating IFF, R, AF, PC"*.

That is consistent with how a SAM arrives here — the CPU pushes `PC` on NMI and the ROM's handler at &0066
pushes `AF` and then `HL` before any DOS code runs — but the ROM then switches to its own stack, and this
documentation has **not traced the SAM invocation path far enough to say exactly what the saved `SP` points
at**. Treat the stack layout as the DISCiPLE/+D convention, not as a verified SAM fact.

#### A consequence for tools

Entry offsets 236–241 are `HL`, the flags byte, `I` and `SP` — which is exactly where a normal file keeps its
**start address and length**. A 48K snapshot therefore has no usable length field. `FSTAT` special-cases the
type for precisely this reason: `FST3` tests for type 5 and returns a fixed 49152 rather than reading the
length field, which would otherwise return part of `SP`.

### Type 7 — ZX Spectrum SCREEN$

6912 bytes: 6144 of pixel data followed by 768 of attributes, the Spectrum's &4000–&5AFF.

| Data offset | Size | Contents |
|---|---|---|
| 0 | 9 | Nine-byte header |
| 9 | 6144 | Pixel data |
| 6153 | 768 | Attributes |

**MasterDOS never writes this type.** Answering the NMI prompt with `3` saves the screen as a `SCREEN$` using
the ordinary save path, not as type 7. Type 7 files on a SAM disk came from somewhere else.

### Type 9 — ZX Spectrum 128K snapshot

In the type table and shown by `DIR` as `ZX SNP 128K`, but **MasterDOS never writes one and has no code that
reads one**. The NMI handler offers only a 48K snapshot. Nothing in the source describes the layout, so
nothing is claimed here.

### Type 10 — OPENTYPE

An open-ended file with no declared length. The sector chain runs until a `00 00` link.

| Data offset | Size | Contents |
|---|---|---|
| 0 | 9 | Nine-byte header |
| 9 | … | Arbitrary bytes |

This is the type [`OPEN`](commands.md#open) creates, and the only one whose length changes after it is
written — see [Open-type files](#open-type-files) below and [open-files.md](open-files.md).

### Type 16 — SAM BASIC program

See [BASIC programs: what travels, and what does not](#basic-programs-what-travels-and-what-does-not) above
for the region layout and the three lengths. In byte terms:

| Data offset | Size | Contents |
|---|---|---|
| 0 | 9 | Nine-byte header |
| 9 | *(hdr16)* | Tokenised program, ending with the &FF program terminator |
| 9 + *(hdr16)* | … | Numeric variables: 26 chain roots (52 bytes) then the records |
| 9 + *(hdr19)* | … | The numbers-to-strings gap |
| 9 + *(hdr22)* | … | Strings and arrays — **without** the trailing &FF |

where *(hdr16)*, *(hdr19)* and *(hdr22)* are the three page-form lengths at entry offsets 221–223, 224–226
and 227–229 (ROM header offsets 16, 19 and 22).

The auto-run line is at entry offsets 242–244: a first byte of 0 means bytes 243–244 are the line number, low
byte first; a first byte of &FF means no auto-run.

### Types 17 and 18 — SAM numeric and string arrays

The data block is the complete **variables-area record**, copied from its type/length byte onward:

| Data offset | Size | Contents |
|---|---|---|
| 0 | 9 | Nine-byte header |
| 9 | 1 | Type/length byte: bit 7 hidden, bit 6 string array, bit 5 numeric array, bits 4–0 the true name length less one |
| 10 | 10 | Name, padded to ten characters |
| 20 | 1 | Data length in whole 16K pages |
| 21 | 2 | Data length modulo 16K, low byte first |
| 23 | … | Dimension count, then one word per dimension, then the elements — five bytes per numeric element; fixed-width rows for a string array |

The same eleven bytes — the type/length byte and the ten-character name — are also in the ROM header's
type-specific area, at **entry offsets 221–231**.

### Type 19 — SAM code

The simplest type. Nothing is compressed or checksummed; a code file on disk is the same bytes as in memory.

| Data offset | Size | Contents |
|---|---|---|
| 0 | 9 | Nine-byte header |
| 9 | *length* | The bytes |

| Field | Where |
|---|---|
| Start address | Entry offsets 236–238, page form |
| Length | Entry offsets 239–241, page form |
| Execution address | Entry offsets 242–244, page form; &FF in the first byte means none |

### Type 20 — SAM SCREEN$

The screen bitmap, then the palette, then the line-interrupt table if there is one. The bitmap's size depends
on the mode, which is in the ROM header's type-specific area at **entry offset 221**:

| Mode | Bitmap size |
|---|---|
| 0 | &1B00 (6912) |
| 1 | &3800 (14336) |
| 2 and 3 | &6000 (24576) |

| Data offset | Size | Contents |
|---|---|---|
| 0 | 9 | Nine-byte header |
| 9 | *(per mode)* | Screen memory |
| 9 + bitmap | 40 | `PALTAB` — the working palette: two sets of 16 palette memories for the flash pair, plus the mode-2 spares |
| … | … | The line-interrupt colour table, if one was in use |

### Type 21 — MasterDOS subdirectory

MasterDOS's own, and the only type with **no data at all**.

| Entry offset | Contents |
|---|---|
| 0 | Type 21 (`DFT`), plus the protect and hidden flags as usual |
| 1–10 | The directory's name |
| 11–12 | Sector count — **zero** |
| 13–14 | First track and sector — unused |
| 15–209 | Sector map — all zero |
| 250 | **The tag this directory gives to the files inside it** |
| 254 | The tag of the directory it is itself in |

`OPNDIR` sets flag bit 4, which tells `OFSM` not to allocate a first sector, so the entry costs one directory
slot and no disk space. Deleting one frees nothing. See [subdirectories.md](subdirectories.md).

Under SAMDOS this entry shows as a zero-length `WHAT?` file; erasing it there orphans everything inside.

### The snapshot filler block

When the NMI handler saves anything **other** than a 48K snapshot — in practice, a screen — the entry is
filled in differently from a 48K snapshot's, and differently again from SAMDOS's:

| Entry offset | Bytes | Written by |
|---|---|---|
| 211–219 | The nine-byte header | `SVHD` |
| 220 | `00` | The flags byte, explicitly zeroed |
| 221 | `00` | The screen mode, explicitly zeroed |
| 222–235 | `00` | Left as `OFSM` cleared them |
| 236–238 | `6E 00 80` | `SNPTAB` — start address, page form |
| 239–241 | `00 00 1B` | `SNPTAB` — length, page form: 0 pages plus &1B00, which is 6912 |
| 242 | `FF` | `SNPTAB` — first byte of the execution address, meaning none |

The length decodes correctly. **The start does not**: read as page form, a page byte of &6E is 110 pages,
which is past the top of even a 512K machine. The author's own comment on that line reads
`;START (IF 256K MACHINE)`, so the value is a fixed assumption about the machine rather than a computed
address. Do not rely on it.

SAMDOS writes a longer version of the same table — 33 bytes over entry offsets 220–252, with eleven spaces
where MasterDOS writes two zeros — but the start and length bytes at 236–241 are identical in both.

## Open-type files

Type 10 is what [`OPEN`](commands.md#open) creates, and it is the only type whose length changes after it is
written. `SDCM` writes the entry back as the file is closed, with the new length, the new sector map and the
date stamp. A file opened `RND` and extended grows one sector at a time, each allocated from the free map as
the pointer runs past the end of the last.

`MOVE` copies an open-type file byte for byte, since it has no structure to interpret.

## A verified example

**[image]** The one file in `res/master_dos_v2-3.mgt` is `MDOS23`, and its two headers agree:

```text
first nine bytes of the file data:   13 86 3D 00 80 FF FF 00 01
directory bytes 236-244:             01 00 80  00 86 3D  FF FF FF
```

| Field | Nine-byte header | Directory, page form | Value |
|---|---|---|---|
| Type | &13 | *(entry byte 0)* | 19, code |
| Length | &3D86 in 0 pages | `00 86 3D` | 15750 |
| Start | &8000 in page 1 | `01 00 80` | Page 1, &8000 |
| Execute | &FFFF | `FF FF FF` | None |

The sector count is 31, and $`31 \times 510 - 9 = 15801`$, which comfortably holds 15750 — so the figures
are self-consistent.

`MDOS23` genuinely has no execution address — it is loaded and called by the ROM's `BOOT`, not run from its
header — so the two &FFFF fields agree here by coincidence rather than by proving anything. The nine-byte
header's exec field would read &FFFF either way; see
[There is no execution address here](#there-is-no-execution-address-here).

### Where the image does *not* match

Three fields of that entry are not what the code would have written:

| Offset | In the image | What `SVHD` / `OFSM` would write |
|---|---|---|
| 211–219 | All zero | The nine-byte header, the same bytes as at the front of the file |
| 220 | &20 | The ROM header's flags byte |
| 221–229 | A second start/length/exec triple | ROM header bytes 16–24, unused for a code file |

The likeliest reading is that this image's directory entry was **synthesised by a PC-side build tool** from
the binary rather than written by MasterDOS. It is good corroboration for the fields it agrees on — type,
name, sector count, first sector, sector map, and the whole 236–244 range — and should not be read as
evidence about the ones it does not.

## Reading a file without the DOS

1. Read entry 0 (track 0, sector 1, first half). Byte 255 plus 4 is the number of directory tracks; bytes
   210–219 are the disk name.
2. Read the directory: tracks 0 to `DTKS`−1 of side 1, two 256-byte entries per sector.
3. Skip entries whose byte 0 is zero. Type is `byte0 & 0x1F`; bit 6 protect, bit 7 hidden.
4. For a tree, read byte 254 as the parent tag — 0 is the root — and, for a type-21 entry, byte 250 as the tag
   it gives its contents.
5. The first sector is byte 13 (track, bit 7 = side 2) and byte 14 (sector).
6. Follow the chain: 510 bytes of data, then byte 510 = next track, byte 511 = next sector. `00 00` ends it.
7. The first nine bytes of the stream are the header; content starts at byte 9.
8. The length is header bytes 7 × 16384 + bytes 1–2, or directory bytes 239–241 in page form. The start
   address is header byte 8 × 16384 plus bytes 3–4 masked to 14 bits, or directory bytes 236–238.
9. **The execution address is directory bytes 242–244 only.** Header bytes 5–6 are always &FFFF and must not
   be used — see [above](#there-is-no-execution-address-here).
