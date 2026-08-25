# MasterDOS 2.3 — RAM Discs and MegaRAM

Drives 3 to 7 are RAM discs: sets of 16K pages pretending to be floppies. From part `RAMD` of
[masterdos23.asm](../annotated-src/masterdos23.asm): `FORMRD`, `RDRSCT`, `RDWSCT`, `RDADR`, `CPFTS`, `FRDRD2`,
`CNTFP`, `MRINIT`.

## The idea

Every disk operation in the DOS begins with `TIRD`, "test if RAM disc". For drives 3 to `RDLIM`−1 it diverts
to the equivalent routine here — `RDRSCT` for a read, `RDWSCT` for a write, `RDSB` for a block save — so
**nothing above that level knows the difference**. A RAM disc has a directory, a sector map, subdirectories, a
name and a path exactly as a floppy does, and every command works on it unchanged.

`RDLIM` is 8, so the RAM disc drives are **3, 4, 5, 6 and 7** — five of them, each independent.

## Creating one

```text
FORMAT "d3:name", dirtracks, tracks
FORMAT "d3:", 0
```

| Argument | Meaning |
|---|---|
| `dirtracks` | Directory tracks, 1 to 39. **A RAM disc may have as few as one** — 20 entries — where a floppy needs four |
| `tracks` | Total tracks, which sets the size. Must exceed the directory and leave at least one data track |

The limits at `WFOD02`: with one directory track the total may be 2 to 157; with four, 5 to 160.

Each track is 10 sectors of 512 bytes, so a RAM disc of $`t`$ tracks needs

$`\text{pages} = \left\lceil \frac{t \times 10}{31} \right\rceil`$

— **31** sectors per page rather than 32, because the first 512-byte block of every page is reserved (see
[below](#the-mover-in-every-page)).

```basic
FORMAT "d3:scratch", 1, 20     : REM a 20-track RAM disc, about 95K
PRINT FPAGES                   : REM check there is room first
FORMAT "d3:", 0                : REM erase it, returning the pages
```

`FORMAT "d3:", 0` deletes the RAM disc and returns its pages to the system. Anything on it is lost.

| Error | When |
|---|---|
| *Nonsense* | A drive above `RDLIM` |
| *No pages free* | Not enough free memory |

## Where the sectors go

A RAM disc owns a list of pages, kept in the first of them. `CPFTS` turns a track and sector into a linear
sector number and then adds one plus a thirty-first of itself — which has the effect of **skipping the first
512-byte block of every page**.

$`\text{block} = s + 1 + \left\lfloor \frac{s}{31} \right\rfloor \quad\text{where } s = (\text{track} \times 10) + \text{sector} - 1`$

The block number then splits into a page index and an offset within it, and `RDADR` maps that page at &8000.

Those reserved first blocks are not wasted. Each holds:

* a copy of the [block mover](#the-mover-in-every-page);
* the page list, so any page can find the others;
* in the **first** page only, the disc's name, its random word, and its current path.

That last point is why `GPATD` fetches a RAM disc's path with `MRDPN` into the buffer `PTHRD` rather than
reading it from `PTH1` or `PTH2`: a RAM disc carries its path with itself, so the path survives the disc being
left alone while other drives are used. See [functions.md](functions.md#which-drive-path-reports-on).

## Internal RAM and MegaRAM

| Page numbers | Memory |
|---|---|
| 0 to &1F | Ordinary internal RAM, 512K |
| &20 and above | **MegaRAM** — external memory, selected through port `MRPRT` (128) with the paging register's top bit set |

`MRTAB` (`DVAR 118–149`) is a bitmap of which MegaRAM pages are in use: 32 bytes, one bit per page, so up to
256 external pages — 4M — can be tracked. `MRINIT` finds out at boot how much is actually fitted, by writing
and reading back.

Internal pages are claimed from the ROM's own allocation table at &5100, so BASIC will not use them; MegaRAM
pages are claimed in `MRTAB`, which nothing but MasterDOS knows about.

`FPAGES` returns the total of both (`CNTFP`).

## The mover in every page

Copying a sector *out of* a RAM disc means having the source page and the destination page mapped at the same
time — which leaves nowhere for the code doing the copying to live. It would be paged out along with whatever
it was reading.

The answer is to put a copy of the mover **in every RAM disc page**: whichever page is mapped, the code is
there. `FORMRD` writes it at &8002 as each page is claimed, followed by 128 unrolled `LDI` instructions at
&8020 and a small loop around them — so a 512-byte move is four passes of an unrolled block copy.

`FRDRD2` copies one sector by jumping into the copy that lives in the source page.

This is the whole reason a page yields 31 sectors and not 32.

## Practical notes

**Speed.** A RAM disc read is a page switch and an `LDIR`; there is no seek, no rotational latency and no
retry. Every DOS operation that is slow on a floppy — a directory scan, a file open, an allocation — is
effectively instant.

**Volatility.** A RAM disc lives in RAM and does not survive a reset or power off. It survives `NEW` and
`CLEAR`, because its pages are claimed in the ROM's allocation table.

**Drive aliasing.** `DRPT` (`DVAR 111–117`) lets a drive pretend to be another. `POKE DVAR 112, 3` points
every reference to drive 2 at RAM disc 3, so a program written for two floppies runs against a RAM disc
without being changed:

```basic
FORMAT "d3:work", 1, 40
POKE DVAR 112, 3               : REM drive 2 is now RAM disc 3
COPY "d1:*" TO "d2:*"          : REM ...so this fills the RAM disc
```

**Write protection.** A RAM disc is never write protected; `WPCHK` returns 0 for any drive of 3 or above
without touching hardware.

**Readiness.** `DSTAT(3,n)` returns −1 until the RAM disc has been formatted, which is the test a program
should use:

```basic
IF DSTAT(3,1) = -1 THEN FORMAT "d3:temp", 1, 20
```

`HOCHK` decides this by asking `RTSTD` for the disc's track count: zero means not formatted.

**Backing it up.** `BACKUP "d3:x" TO "d1:x"` works, and only transfers the sectors in use — so a lightly used
RAM disc copies to floppy quickly. This is the intended way to make a RAM disc's contents permanent before
switching off.
