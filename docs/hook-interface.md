# MasterDOS 2.3 — The Hook Interface

The `RST &08` codes a machine-code program can call the DOS with. From `HOOK` and `SAMHK` in
[masterdos23.asm](../annotated-src/masterdos23.asm).

## Calling a hook

```asm
           LD IX,&4B00      ; the ROM's header buffer, HDR
           RST &08
           DB 129           ; open the file described there
```

The ROM's `RST &08` handler notices that the code is 128 or more and that a DOS is loaded, pages the DOS in at
&8000 and calls it at page offset &0200. Codes below 128 are ordinary errors.

`HOOK` doubles the code to make a word offset into `SAMHK`, **which discards bit 7 in the process** — so codes
128 to 174 map onto entries 0 to 46 without a subtraction. It saves both register sets, since the ROM's save
and load code passes arguments in the alternate set and expects results back the same way.

> [!IMPORTANT]
> **Arguments are read from the alternate register set.** `HKA` is `A'`, `HKHL` is `HL'`, `HKDE` is `DE'`,
> `HKBC` is `BC'`. Set them with `EXX` / `EX AF,AF'` before the `RST`.

> [!WARNING]
> **There is no upper bound check.** A code above 174 indexes past the end of `SAMHK` and jumps to whatever
> follows. Unlike SAMDOS there is no lower check either.

The table's own address is in `DVAR 22–23`, so a program can find and, if it must, patch it.

## The table

Codes 128–142 are the set the ROM publishes and match SAMDOS's. 143 upward are MasterDOS's own. `HDUMMY` is a
stub for codes that are reserved or unimplemented.

| Code | Routine | Purpose |
|---|---|---|
| 128 | `INIT` | Boot the DOS |
| 129 | `HGTHD` | [Open a file and return its header](#open-a-file) |
| 130 | `HLOAD` | [Load the file body](#load-verify-and-save) |
| 131 | `HVERY` | Verify the file body against memory |
| 132 | `HSAVE` | Save header and body |
| 133 | `SKSAFE` | Park the head somewhere harmless |
| 134 | `HOPEN` | [Open a stream onto a file](#open-files) |
| 135 | `HCLOS` | Close a stream |
| 136 | `HAUTO` | Load and run the `AUTO*` file |
| 137 | `HSKTD` | Seek to track D |
| 138 | `HDUMMY` | *(reserved: format a track)* |
| 139 | `HVAR` | [The `DVAR` function](#the-dvar-hook) |
| 140 | `HEOF` | The `EOF` function |
| 141 | `HPTR` | The `PTR` function |
| 142 | `HPATH` | [The `PATH$` function](functions.md#path) |
| 143 | `HLDPG` | Like 130, but `A` gives the destination page |
| 144 | `HVEPG` | Like 131, but `A` gives the page |
| 145 | `HSDIR` | [Set the current directory](#set-directory): `A` = page, `DE` = start, `BC` = length of the name |
| 146 | `ROFSM` | Open a file's sector map |
| 147 | `HOFLE` | Open a file for writing |
| 148 | `SBYT` | Save one byte, in `A` |
| 149 | `HWSAD` | [Write a sector](#raw-sector-access) |
| 150 | `HKSB` | Save a block: `A:DE` bytes from `HL` |
| 151 | `HDBOP` | Output `BC` bytes from `DE` to the open file at `IX` |
| 152 | `SCFSM` | Close the file being written |
| 153 | `HORDER` | [Sort a list](#sort): `A` = characters to compare, `BC` = item length, `DE` = count, `HL` = start |
| 154–157 | `HDUMMY` | *(reserved)* |
| 158 | `HGFLE` | Open a file for reading |
| 159 | `LBYT` | Load one byte into `A` |
| 160 | `HRSAD` | Read a sector |
| 161 | `HLDBK` | Load a block |
| 162 | `HFRSAD` | [Far read](#far-multi-sector-transfers): `IX` sectors from drive `A`, track `D`, sector `E`, to page `C` offset `HL` |
| 163 | `HFWSAD` | Far write, same arguments |
| 164 | `REST` | Move the head to track 0 |
| 165 | `PCAT` | Print the catalogue |
| 166 | `HERAZ` | Erase the file named at `(IX+1)`…`(IX+10)` |
| 167 | `MCHWR` | The `D` channel's write routine |
| 168 | `MCHRD` | The `D` channel's read routine |
| 169 | `HPTV` | [Print token `A`](#the-basic-extension) |
| 170 | `HPFF` | Post-&FF print |
| 171 | `HGTTK` | Get token — match a MasterDOS keyword |
| 172 | `HKLEN` | Evaluator patch, including `LENGTH` |
| 173 | `HSLMV` | `SAVE` / `LOAD` / `MERGE` / `VERIFY` patch |
| 174 | `RCPTCH` | `RUN` / `CLEAR` patch |

### Comparison with SAMDOS

| Codes | SAMDOS 2 | MasterDOS 2.3 |
|---|---|---|
| 133–135, 138, 140–142 | Bare `RET` | Implemented |
| 143–146, 151, 153, 162–163, 167–174 | Bare `RET` or absent | Implemented |
| 154–157 | `RET` | `HDUMMY` — the same thing |

A program that needs anything above 142 should check `DVAR 7` first: 20 is SAMDOS 2, 42 or 43 MasterDOS.

**[external]** The [Sam Coupé Scrapbook hook code table](http://www.mono.org/~unc/Coupe/Tech/mdhook.html)
documents the same set independently and agrees with the source throughout, including the register contracts
quoted below. It names 146 `HOFSM` and 152 `HCFSM`, where the source's labels are `ROFSM` and `SCFSM`; the
functions match.

---

## Open a file

| Entry | |
|---|---|
| `IX` | The 48-byte header buffer, normally `HDR` at &4B00 |
| `(IX+0)` | The type wanted |
| `(IX+1)`…`(IX+10)` | The name wanted; &FF in the first byte matches anything |

| Exit | |
|---|---|
| `HDL` (&4B50) | The header found |
| The file | Open, positioned at its first sector |

## Load, verify and save

| Register | Meaning |
|---|---|
| `HL'` | Destination address, in the &8000–&BFFF window |
| `C'` | Whole 16K pages |
| `DE'` | Length modulo 16K |

Hooks **143** and **144** are the same as 130 and 131 but take a destination **page** in `A`, which removes the
need for the caller to have the page mapped.

Hook **132** takes a complete 48-byte header at `IX`: type, name, start, length and execution address.

Hook **131** raises *Verify failed* (93) at the first difference.

## Open files

The two sets are different things and easy to confuse:

| Hooks | What they do |
|---|---|
| 134, 135 | **Streams.** Attach a `D` channel to a BASIC stream, as [`OPEN`](commands.md#open) does. `HKHL` points at the stream's entry in `STRMS`, `HKDE` at the name, `HKC` gives its length |
| 147, 148, 150, 152 | **A file being written by the DOS itself**, as `SAVE` uses. Open, byte, block, close |

For 147–152 the sequence is:

| Step | Hook | Effect |
|---|---|---|
| 1 | 147 `HOFLE` | Read the header at `IX`, scan the directory, allocate the first sector, write the nine-byte header |
| 2 | 148 `SBYT` | Write one byte from `A` |
| 2′ | 150 `HKSB` | Write a block: `A` pages plus `DE` bytes, from `HL` |
| 2″ | 151 `HDBOP` | Write `BC` bytes from `DE` to an open-type file |
| 3 | 152 `SCFSM` | Flush the buffer and **write the directory entry** |

> [!IMPORTANT]
> Step 3 is not optional. Until `SCFSM` runs the entry exists only in memory and the file does not exist.

Reading is the mirror: 158 `HGFLE` to open, 159 `LBYT` for a byte, 161 `HLDBK` for a block. There is no close.

## The DVAR hook

| Entry | Calculator stack: the variable number |
|---|---|
| Exit | Calculator stack: its **address**, as a floating-point number |

See [dos-variables.md](dos-variables.md).

## Set directory

| Entry | |
|---|---|
| `A` | Page the name is in |
| `DE` | Offset of the name |
| `BC` | Length of the name |

The hook form of `DIR = "path"`. The name is evaluated as a path by `EVNM2`, avoiding the string fetch that
the BASIC form does — so the caller supplies raw bytes rather than a BASIC string. See
[subdirectories.md](subdirectories.md#changing-directory).

## Raw sector access

| Entry | |
|---|---|
| `A'` | Drive, 1–7 |
| `D'` | Track; bit 7 selects side 2 |
| `E'` | Sector, 1–10 |
| `HL'` | The caller's address |

512 bytes. Neither consults the directory or the free map. **RAM disc drives work identically**, since `TIRD`
diverts them below this level.

## Far multi-sector transfers

MasterDOS's own, and the fastest way to move bulk data:

| Entry | |
|---|---|
| `A` | Drive, 1–7 |
| `D` | Track |
| `E` | Sector |
| `C` | Destination (or source) **page** |
| `HL` | Offset within that page, &8000–&BFFF |
| `IX` | **Number of sectors** |

The page is given explicitly, so the caller does not have to map it — which is what makes these usable for
filling a page other than the one the caller is running in. `BACKUP` and `COPY` are built on them.

The address increments by `DVAR 37–38` (`MSINC`, &0200) per sector.

## Sort

| Entry | |
|---|---|
| `HL` | Start of the list |
| `BC` | Length of one item |
| `DE` | Number of items |
| `A` | Number of characters to compare |

Sorts in place into ASCII order. This is what `DIR` uses on the names it has collected, and it is exposed
because a program listing a catalogue itself wants the same thing.

## 165, 166

| Code | Notes |
|---|---|
| 165 `PCAT` | `A` = 2 for a simple listing, 4 for a detailed one |
| 166 `HERAZ` | The name is at `(IX+1)`…`(IX+10)`. **Does not check the protect flag** — unlike the `ERASE` command, this deletes a protected file without complaint |

## The BASIC extension

These are not called by user programs. They are the routines the ROM calls, having had four of its vectors
repointed at boot by `INIP3`:

| Hook | ROM vector | Called to |
|---|---|---|
| 169 `HPTV` | `PRTOKV` &5ADE | Print token `A`, if it is one of MasterDOS's |
| 170 `HPFF` | | Handle the byte after an &FF prefix. Returns carry if it is not one of MasterDOS's |
| 171 `HGTTK` | `MTOKV` &5AFA | Match one of MasterDOS's keywords while tokenising |
| 172 `HKLEN` | `EVALUV` &5AF6 | Evaluate one of MasterDOS's functions, and `LENGTH` |
| 173 `HSLMV` | `CMDV` &5AF4 | `SAVE`, `LOAD`, `MERGE`, `VERIFY` — the ROM's own code, copied to a buffer and patched |
| 174 `RCPTCH` | | `RUN` and `CLEAR` |

Two of them are worth knowing about even so:

**173** patches the ROM's `SAVE`/`LOAD`/`MERGE`/`VERIFY` rather than reimplementing them. It copies the ROM's
code into a buffer, redirects the name fetch to `SVDT` (the ROM copies only fifteen characters, which is not
enough for a path), fixes `MERGE`'s handling of a program with many numeric variables, and rewrites the ROM's
stack so returning from the hook enters the patched copy.

**172** handles `LENGTH` by copying the ROM's routine into a buffer at &8D80 and patching fifteen bytes over
the part that mishandles a string crossing a page boundary. See [functions.md](functions.md#length).

`INIP3` finds two of the addresses it needs by **searching the ROM** for the three-byte instruction sequences
it expects (`FTHREE`) rather than hardcoding them, because they are not in the published jump table — so a
different ROM revision would still be patched correctly.
