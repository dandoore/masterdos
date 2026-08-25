# MasterDOS 2.3 — On-Disk Format

The physical layout of a SAM Coupé disk and everything MasterDOS stores on it. Derived from
[masterdos23.asm](../annotated-src/masterdos23.asm) — `FDHR`, `NRSAD`, `OFSM`, `CFSM`, `SDCM`, `FESET`,
`SDTKS`, `DFMT`, `WFOD` — and the structural equates in part `A1`.

This is the **MGT format**, shared with SAMDOS and, before it, with GDOS on the DISCiPLE and +D.
**[external]** MasterDOS changes nothing about it: every addition lives in bytes SAMDOS left at zero.

## Contents

| Section | |
|---|---|
| [Geometry](#geometry) | Tracks, sides, sectors |
| [Layout of the disk](#layout-of-the-disk) | What lives where, and why it is variable |
| [Sector chaining](#sector-chaining) | How a file's sectors are linked |
| [The directory](#the-directory) | Variable size, and how an entry is addressed |
| [The directory entry](#the-directory-entry) | All 256 bytes, including MasterDOS's own |
| [Entry 0: the disk's own record](#entry-0-the-disks-own-record) | Name, identity, directory size |
| [The sector address map](#the-sector-address-map) | Bit ordering, and its three uses |
| [Free space](#free-space) | Why it is not stored anywhere |
| [Allocation](#allocation) | How a new sector is chosen |
| [Disk change detection](#disk-change-detection) | The random word |
| [Disk images](#disk-images) | MGT file ordering |

## Geometry

| Property | Value |
|---|---|
| Tracks per side | 80 (`DVAR 1` / `DVAR 2` say what the formatter writes) |
| Sides | 2 |
| Sectors per track | 10, numbered **1 to 10** |
| Bytes per sector | 512 |
| Usable bytes per sector | 510 — the last two link to the next sector |
| Encoding | MFM, double density |
| Total capacity | 819200 bytes |

**The side is carried in bit 7 of the track number.** Side 1 is tracks 0–79, side 2 is tracks 128–207. The
DOS fills one whole side before starting the other.

## Layout of the disk

| Tracks | Contents |
|---|---|
| 0 to `DTKS`−1, side 1 | The directory, 20 entries per track |
| `DTKS` to 79, side 1 | File data |
| 128–207, side 2 | File data |

`DTKS` is **variable in MasterDOS**, from 4 to 39. It is not stored directly: byte 255 of the first directory
entry holds the number of tracks **beyond** the standard four, and `SDTKS` adds 4 to it after reading track 0
sector 1.

That encoding is the compatibility trick. A disk with the standard four tracks stores zero there, which is
what SAMDOS leaves and what an unformatted byte reads as — so a SAMDOS disk is correctly understood as a
four-track disk without SAMDOS ever having known about the field.

| Directory tracks | Entries | Data sectors on an 80-track double-sided disk |
|---|---|---|
| 4 | 80 | 1560 |
| 10 | 200 | 1500 |
| 39 | 780 | 1210 |

> [!WARNING]
> **A disk with more than four directory tracks must not be written by SAMDOS.** SAMDOS's `disk.dirtrks` is a
> constant 4, so it will treat tracks 4 upward as free data space and allocate over the extra directory.

## Sector chaining

Every sector holds **510 bytes of data followed by a two-byte link**:

| Offset in sector | Contents |
|---|---|
| 0–509 | Data |
| 510 | Track of the next sector |
| 511 | Sector of the next sector |

`00 00` marks the last sector. The order is **track first**, the opposite of the directory entry's
first-sector field.

**[image]** Verified against `res/master_dos_v2-3.mgt`: the `MDOS23` file starts at track 4 sector 1, whose
bytes 510–511 are `04 02`.

## The directory

Tracks 0 to `DTKS`−1 of side 1, **two 256-byte entries per sector**. Entry $`n`$, counting from zero:

$`\text{track} = \left\lfloor \frac{n}{20} \right\rfloor, \quad \text{sector} = \left\lfloor \frac{n \bmod 20}{2} \right\rfloor + 1, \quad \text{half} = n \bmod 2`$

The half is the `RPTH` field of the channel record. The number `DIR` prints and `LOAD n` takes is
$`n + 1`$.

**An entry whose first byte is zero is free.**

`FSLSR` remembers the first free slot met during a scan, so that closing a file does not have to search again
— which matters more here than in SAMDOS, since a 39-track directory is 390 sectors to read.

## The directory entry

All 256 bytes. Fields marked **[MD]** are MasterDOS's own additions.

| Offset | Size | Field | Contents |
|---|---|---|---|
| 0 | 1 | Type and flags | Bits 0–4 file type, bit 6 protected, bit 7 hidden. **Zero means free** |
| 1–10 | 10 | Name | Ten characters, space-padded |
| 11–12 | 2 | Sector count | **High byte first** |
| 13 | 1 | First track | Bit 7 selects side 2 |
| 14 | 1 | First sector | 1–10 |
| 15–209 | 195 | Sector address map | See [below](#the-sector-address-map) |
| 210–219 | 10 | Nine-byte header, or the disk name | In entry 0, bytes 210–219 are the **disk name**. In any other entry, 211–219 are the file's nine-byte header |
| 220–252 | 33 | ROM header tail | Bytes 15–47 of the ROM's 48-byte header |
| 250 (&FA) | 1 | **[MD]** Directory tag | For an entry of type 21, the tag it gives to files inside it. `DIRT` |
| 252–253 (&FC–&FD) | 2 | **[MD]** Disk identity | *In entry 0 only.* The random word, **high byte at 252** |
| 254 (&FE) | 1 | **[MD]** Parent tag | The tag of the directory this file belongs to; 0 is the root |
| 255 (&FF) | 1 | **[MD]** Extra directory tracks | *In entry 0 only.* `DTKS` − 4, so 0–35 |

Note that 250 and 252–253 fall inside the 220–252 range nominally occupied by the ROM header tail. They are
bytes of the ROM header that are never used for the file types MasterDOS stores, which is why they were
available.

**[external]** The MGT filesystem wiki gives 250 as "MasterDOS subdirectory ID", 252–253 as "MasterDOS disk
ID", 254 as "parent directory ID" and 255 as "extra directory tracks (0–35)" — agreeing exactly with the
source on all four.

### Where the ROM's header fields land

Bytes 220 onwards are the ROM's 48-byte header from its offset 15, so:

| Directory offset | ROM header offset | Field |
|---|---|---|
| 220 (&DC) | 15 | Flags: bit 0 invisible, bit 1 protected |
| 236–238 (&EC–&EE) | 31–33 | Start address, page form |
| 239–241 (&EF–&F1) | 34–36 | Length, page form — this is what `FSTAT(f$,2)` reads |
| 242–244 (&F2–&F4) | 37–39 | Execution address, or for BASIC the auto-run line |
| 245–249 (&F5–&F9) | 40–44 | **[MD]** The date stamp, in the ROM's comment area |

### The date stamp

`DATSET` writes five bytes at offset 245 as a file is closed, taken from the clock:

| Offset | Field |
|---|---|
| 245 | Day, 1–31 |
| 246 | Month, 1–12 |
| 247 | Year |
| 248 | Hour, 0–23 |
| 249 | Minute, 0–59 |

`PNDAT` prints them in a `DIR DATE` listing. They occupy the first five bytes of the ROM's 40-byte comment
area, which the ROM never writes to disk — so nothing is displaced.

**[image]** The `MDOS23` entry carries `07 09 68 0C 02` there: 7 September, year &68, 12:02. **[external]** The
MGT filesystem wiki gives 245–249 as "Timestamp (day, month, year, hour, minute)", which matches. Note the
year is a plain byte, not BCD — &68 is 104, presumably 2004.

## Entry 0: the disk's own record

The first directory entry is an ordinary entry — the first file on a fresh disk uses it — but four of its
fields describe the disk rather than the file:

| Offset | Field | Written by | Read by |
|---|---|---|---|
| 210–219 | Disk name, ten characters | `FESET` at format, `RENAM` at `RENAME TO` | `SDTKS`, into `DNAME` for the `DIR` heading |
| 252–253 | Random identity word | `FESET`, and `BACKUP` on the target | `SDTKS`, compared against `CRWT` |
| 255 | `DTKS` − 4 | `FESET` | `SDTKS` |

`SDTKS` runs after every read of track 0 sector 1 and picks up all three at once.

## The sector address map

195 bytes at offset 15, **one bit per data sector**, set if the sector belongs to this file.
$`195 \times 8 = 1560`$, exactly the data sectors of an 80-track double-sided disk with a four-track
directory.

**Bit ordering is little-endian within each byte, and the sequence starts at the first data sector:**

| Byte | Bit | Track | Sector |
|---|---|---|---|
| 0 | 0 | `DTKS` | 1 |
| 0 | 1 | `DTKS` | 2 |
| 0 | 7 | `DTKS` | 8 |
| 1 | 0 | `DTKS` | 9 |
| 1 | 2 | `DTKS`+1 | 1 |
| … | | | |
| 94 | 7 | 79 | 10 |
| 95 | 0 | 128 | 1 |

On a standard disk `DTKS` is 4, and the map is exactly the one SAMDOS produces. On a disk with a larger
directory the map still starts at the first data track, so the bit positions shift and the top of the map goes
unused — which is why a bigger directory costs data capacity twice over.

**The map is used three ways:**

1. **In a file's own entry**, to say which sectors it owns.
2. **Globally**, OR-ed together across every entry as the directory is scanned (`NRSAD`), giving the free-space
   picture. That combined map is the only record of what is free.
3. **For random access.** `FITS` counts set bits through a file's own map to find its $`n`$th sector,
   which is how [`POINT`](open-files.md#random-access) seeks without reading a single sector from disk.

The third use is MasterDOS's, and is why the map — redundant with the sector chain — earns its 195 bytes.

## Free space

**Free space is not stored anywhere on the disk.** There is no allocation table and no free count. It is
recomputed by reading every directory sector and OR-ing the maps of the entries in use.

Consequences:

* Deleting a file frees its sectors instantly and implicitly — zeroing the type byte removes its map from the
  union, which is why `ERASE` writes one byte.
* An interrupted save cannot corrupt the free space: if the entry was never written, the sectors were never
  claimed.
* A deleted file's data and its complete sector map are both still there.
* Every allocation has to read the whole directory first, which on a 39-track directory is 390 sectors.
* `DSTAT(d,3)` and the figure `DIR` prints both come from this scan, and are therefore accurate rather than
  assumed — unlike SAMDOS's, which subtracts from a capacity taken from `DVAR 1`.

## Allocation

First-fit from the start of the data area, as SAMDOS. A map byte of &FF skips eight sectors at once; otherwise
bits are tested from bit 0 upward. The chosen bit is set in both the global map and the file's own, and the
file's sector count is incremented.

`DVAR 14` (`SKEW`) selects the sector interleave: &FF for skew 1, &FE for skew 2.

There is no attempt at contiguity. A file written to a fragmented disk gets whatever holes exist, in order.

## Disk change detection

Every formatted disk carries a **random word** at entry 0 bytes 252–253, made at format time from the Z80's
refresh register and the frame counter — cheap entropy, but enough that two disks are unlikely to collide.

`SDTKS` compares it against `CRWT` (`DVAR 63–76`), which holds the last word seen in each drive:

| Result | Action |
|---|---|
| Same word | Nothing; the same disk is still there |
| Different word | The current directory for that drive is **reset to the root**, because the tag it held belonged to a different disk's tree |
| Different word **and** a file is open on that drive | The DOS beeps and prints `OPEN file` as a warning |

It cannot refuse the swap — a file may legitimately span one, as during a single-drive copy — but it can say
so. `SAMRN` holds the word that was current when the free-space map was built, which is the other half of the
same check.

This is why `BACKUP` gives the target a **fresh** random word rather than copying the source's: two disks
sharing an identity would defeat the whole mechanism.

## Disk images

A `.mgt` image is 819200 bytes, every sector in physical order with the two sides interleaved by track:

$`\text{offset} = \bigl( (\text{track} \times 2 + \text{side}) \times 10 + \text{sector} - 1 \bigr) \times 512`$

where side is bit 7 of the DOS's track number and track the low seven bits.

```python
def offset(track, sector):
    side = 1 if track & 0x80 else 0
    return (((track & 0x7f) * 2 + side) * 10 + sector - 1) * 512
```

**[image]** Parsing `res/master_dos_v2-3.mgt` with this gives one file: `MDOS23`, type 19 (code), 31 sectors,
first sector track 4 sector 1, start page 1 &8000, length 15750, no execution address — and a sector map with
exactly 31 bits set. Every field this document describes for a code file's entry was checked against it; see
[file-formats.md](file-formats.md#a-verified-example) for the parts that did not match and why.
