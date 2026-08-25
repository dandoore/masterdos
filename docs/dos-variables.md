# MasterDOS 2.3 — DOS Variables (`DVAR`)

The block at `DVAR` (&4220 in the DOS page) in [masterdos23.asm](../annotated-src/masterdos23.asm), reached
from BASIC through the `DVAR` function, which is hook code 139 (`HVAR`).

This is **MasterDOS's principal configuration interface**. Nearly every default the DOS has is here, and the
block is far larger than SAMDOS's — 154 bytes against 32.

## How `DVAR` works

> [!IMPORTANT]
> **`DVAR n` returns the *address* of variable $`n`$, not its value.** Use `PEEK DVAR n` to read and
> `POKE DVAR n, x` to write.

The address is the DOS's page × 16384 plus the offset, which needs more than sixteen bits, so it is stacked as
a floating-point number. `PEEK` and `POKE` accept that form, so nothing special is needed:

```basic
PRINT (PEEK DVAR 7 - 20) / 10      : REM the version: 2.3
POKE DVAR 0, 0                     : REM stop the border flashing
POKE DVAR 9, 0                     : REM unsorted directory listings
```

There is **no range check**. A large $`n`$ returns an address inside the DOS's own code.

## The variables

Multi-byte fields are marked with their length.

### Drives and disks

| `DVAR` | Name | Default | Purpose |
|---|---|---|---|
| 0 | `RBCC` | 7 | Border colour flashed during disk access. **Zero disables it** |
| 1 | `TRAKS1` | 128+80 | Drive 1: track count, bit 7 set for double sided |
| 2 | `TRAKS2` | 0 | Drive 2, same encoding. **Zero means no second drive** |
| 3 | `STPRAT` | 0 | Drive 1 head step rate |
| 4 | `STPRT2` | 0 | Drive 2 step rate |
| 14 | `SKEW` | &FF | Sector interleave: &FF for skew 1, &FE for skew 2 |
| 15 | `ODEF` | 1 | **The DOS's own default drive.** Used when `DEVICE` gives nothing usable — see [`PATH$`](functions.md#which-drive-path-reports-on) |
| 16 | `DTKS` | 4 | Directory tracks on the current disk. Set by `SDTKS` from the disk itself; also the default `FORMAT` uses |
| 111–117 | `DRPT` (7) | 1,2,3,4,5,6,7 | **The drive alias table.** Drive $`n`$ actually means `DRPT[n]` |

### The catalogue

| `DVAR` | Name | Default | Purpose |
|---|---|---|---|
| 5 | `CHDIR` | `" "` | The character `DIR` prints in place of a space inside a file name |
| 8 | `DCOLS` | 0 | Column count for a short listing. **Set automatically**; not an input |
| 9 | `SRTFG` | 1 | Sort the short listing. Zero gives directory order |
| 11 | `FNSEP` | `"."` | The separator `PFNAME` uses inside file names |
| 19 | `DTFLG` | 0 | Non-zero to show dates. Set by `DIR DATE`, cleared at the start of every `DIR` |
| 21 | `MAXT` | 0 | Highest subdirectory tag in use. Maintained by `FDHR`; `OPNDIR` allocates `MAXT`+1 |
| 24 | `MSFLG` | 0 | Invert characters above 127 when `MOVE` writes to the screen. 1 prints everything except &FF literally |
| 25 | `MSUPC` | `"."` | Character `MOVE` substitutes for codes below 32 or equal to 255 |

### Subdirectories and paths

| `DVAR` | Name | Default | Purpose |
|---|---|---|---|
| 12–13 | `RTSYM` (2) | `\` `/` | The two root symbols. **Both accepted on input; the first is always written** |
| 17–18 | `CDIRT` (2) | 0 | The current directory tag, and its alternate-register copy |
| 49–55 | `CDIT` (7) | 0 | Current directory tag **for each drive** — `DRSET` loads the right one |
| 56–62 | `PLT` (7) | 2 | Path length for each drive. 2 is the length of `"1:"` |

See [subdirectories.md](subdirectories.md).

### Open files

| `DVAR` | Name | Default | Purpose |
|---|---|---|---|
| 10 | `DELIM` | &0D | **The delimiter `POINT ... OVER` counts.** Carriage return by default |
| 20 | `SAMCNT` | 0 | Number of open files, used to decide whether a disk-swap warning is needed |
| 29 | `DWAI` | 0 | Quarter-seconds to wait before a write, so a drive that has just started has come up to speed |

### RAM discs

| `DVAR` | Name | Default | Purpose |
|---|---|---|---|
| 39–43 | `RDDT` (5) | 0 | Tracks per RAM disc, for drives 3–7. **Zero means not formatted** |
| 44–48 | `FIPT` (5) | 0 | First page of each RAM disc |
| 118–149 | `MRTAB` (32) | 0 | MegaRAM page bitmap, one bit per page — up to 256 external pages |

See [ram-discs.md](ram-discs.md).

### Disk identity

| `DVAR` | Name | Default | Purpose |
|---|---|---|---|
| 63–76 | `CRWT` (14) | 0 | The random word last seen in each drive — two bytes per drive |
| 77–78 | `SAMRN` (2) | 0 | The disk's random word at the moment the free-sector map was built |

See [disk change detection](disk-format.md#disk-change-detection).

### Clock

| `DVAR` | Name | Default | Purpose |
|---|---|---|---|
| 79–80 | `TDVAR` (2) | 0 | Which buffer `TIME` / `DATE` is working on |
| 81–88 | `DATDT` (8) | `00/00/00` | **The date, as a printable string.** This is what `DATE$` returns |
| 89 | | &0D | Terminator |
| 90–95 | (6) | 31,1,12,1,99,0 | Date limits: day high/low, month high/low, year high/low |
| 96–103 | `TIMDT` (8) | `00:00:00` | **The time, as a printable string.** What `TIME$` returns |
| 104 | | &0D | Terminator |
| 105–110 | (6) | 23,0,59,0,59,0 | Time limits: hour, minute, second, each high then low |
| 150 | `CKPT` | &EF | **The clock chip's port. Zero means no clock**, and all clock code returns at once |

The limits are checked by `DTVCK` when a value is set, which is why `TIME "99:00:00"` is refused without a
separate validation table.

### Extending the DOS

| `DVAR` | Name | Default | Purpose |
|---|---|---|---|
| 30–32 | `EXTADD` | `CALL` | The three bytes of a `CALL` instruction — do not poke these |
| 33–34 | `ONERR` (2) | 0 | **The external command vector.** See below |
| 35 | | `RET` | The `RET` that ends the external call |
| 36 | `EAPG` | 0 | Page to map at &8000 if `ONERR` is above &8000 |
| 22–23 | | `SAMHK` | The address of the hook table — see [hook-interface.md](hook-interface.md) |
| 37–38 | `MSINC` (2) | &0200 | Multi-sector address increment |

### Other

| `DVAR` | Name | Default | Purpose |
|---|---|---|---|
| 6 | `NSTAT` | 1 | Network station number |
| 7 | `VERS` | 43 | **Version × 10 + 20.** 43 is 2.3, 42 is 2.2, and SAMDOS 2 reads 20 |
| 26 | `NMIKP` | 4 | Page mapped at &8000 when NMI is answered with `1` or `5` |
| 27–28 | `NMIKA` (2) | &0004 | **The NMI user vector.** Default is a bare `RET` |
| 151–152 | `BEEPT` (2) | &0085 | Beep duration |
| 153–154 | `XXPTR` (2) | 0 | `XPTR` store |
| 155 | | 19 | The number of entries in `CTAB` |

## The pokes that matter

**`POKE DVAR 2, 128+80`** — enable a second floppy drive. Zero by default, so `DIR 2` gives *No such drive*
until this is set. The single most commonly needed poke.

**`POKE DVAR 0, 0`** — stop the border flashing during disk access.

**`POKE DVAR 9, 0`** — list the directory in physical order rather than sorted. Useful when the directory
number matters, since `LOAD n` takes the physical number and a sorted listing does not show it in order.

**`POKE DVAR 10, CODE "*"`** — change the delimiter that `POINT #s, OVER n` counts, for a file that uses
something other than carriage return between records.

**`POKE DVAR 112, 3`** — make drive 2 an alias for RAM disc 3, so a two-drive program runs against a RAM disc
unchanged.

**`POKE DVAR 150, 0`** — declare that no clock is fitted, so the clock code returns immediately rather than
retrying the chip.

**`POKE DVAR 26, page: POKE DVAR 27, lo: POKE DVAR 28, hi`** — install a routine on the NMI button, entered
when NMI is answered with `1` or `5`.

## The external command vector

`DVAR 33–34` is called by `SYNTAX` for any command token MasterDOS does not recognise:

```asm
EXTADD:        CALL             ; DVAR 30-32
ONERR:         DEFW 0           ; DVAR 33-34  -- the address to call
               RET              ; DVAR 35
EAPG:          DEFB 0           ; DVAR 36
```

| On entry | |
|---|---|
| A | The ROM error code that would have been reported |
| `CHADD` | Restored to where the ROM left it |

**MasterDOS improves on SAMDOS here.** If the address has bit 15 set, the page in `EAPG` is mapped at &8000
before the jump, so a handler can live in its own 16K page rather than having to fit in the system area.
Returning without acting lets the ROM's error stand; a vector of zero is skipped entirely.

There is still no chaining convention, so two utilities installing themselves here will conflict.

## Version detection

```basic
IF PEEK DVAR 7 = 20 THEN LET dos$ = "SAMDOS 2"
IF PEEK DVAR 7 >= 42 THEN LET dos$ = "MasterDOS " + STR$ ((PEEK DVAR 7 - 20) / 10)
```

Worth doing before using anything above hook 142, or any of the new keywords — under SAMDOS the hooks are
bare `RET`s and the keywords do not tokenise.
