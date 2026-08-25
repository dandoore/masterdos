# Annotated MasterDOS 2.3 source

A parallel copy of `../src/masterdos23.asm`, documented as a modern codebase would be: file-level overviews,
per-routine contracts, inline explanation of non-obvious code, and named constants in place of magic numbers.

**The annotated source assembles to a byte-identical binary.** Nothing here changes behaviour.

> [!WARNING]
> This annotation was produced with AI assistance and has not been run on real hardware (see
> [Verification](#verification) for what *has* been checked). Treat the commentary as a well-evidenced reading of the
> code, not as the author's own documentation.

## Status

**Complete.** All sixteen sections are annotated. The annotated tree assembles to a binary identical both to the one
built from `../src` and to the released `../res/MDOS23.bin`.

| Part | Lines | Contents |
|---|---|---|
| header | 14 | Comet directive, `DUMP`/`ORG`, copyright |
| `Part_A1` | 204 | Equates: hardware, ROM entry points, disk and directory structures |
| `Part_B1` | 718 | Boot sector, DOS variables, `DVAR`, command table, `SYNTAX` and `HOOK` |
| `Part_C11` | 954 | Disk driver: sector transfer, error recovery, seeking, block load and save |
| `Part_C12` | 956 | Free-space map, directory scan, opening and closing files |
| `Part_D1` | 818 | ROM interface, the flag byte, errors and messages, the NMI snapshot |
| `Part_E1` | 779 | Formatting, file types, decimal printing, the messages |
| `Part_F11` | 862 | COPY, DIR and its sort, ERASE, RENAME, PROTECT, HIDE |
| `Part_F12` | 839 | LOAD, FORMAT, and the argument parsers |
| `Part_G1` | 646 | The load and save hooks, `DVAR`/`EOF`/`PTR`, file name parsing |
| `Part_MOVE` | 442 | MOVE between any two channels, and BACKUP |
| `Part_SER2` | 1340 | Open files: streams, channels, record access, `POINT` |
| `Part_TIME` | 272 | Real-time clock |
| `Part_SUBD` | 542 | Subdirectories |
| `Part_RAMD` | 718 | RAM discs and MegaRAM |
| `Part_HOOKS` | 1272 | Extending BASIC, the new functions, and the ROM patches |

## Things worth knowing about the code

A few findings that took some working out:

* **Subdirectories need no format change.** Two spare bytes in each directory entry carry the whole tree: one holds
  the tag of the directory a file belongs to, and for a directory entry the other holds the tag it gives its own
  contents. Everything else — the free-space map, the allocation, the file chains — is untouched.
* **The boot code searches the ROM for the addresses it needs.** Two entry points are not in the jump table, so
  `INIP3` scans for the three-byte instruction sequences it expects (`FTHREE`) rather than hardcoding addresses —
  which means a different ROM revision would still be patched correctly.
* **Every RAM disc page contains a copy of the block mover.** Copying a sector out of a RAM disc needs both pages
  mapped at once, leaving nowhere for the copying code to live — so a copy of it is written into the first 512-byte
  block of every page as the disc is formatted, along with 128 unrolled `LDI`s.
* **The disk carries a random identifying word**, made from the refresh register and the frame counter at format
  time. Comparing it is how a disk swap is detected; on a change the current directory resets to the root, and if a
  file is open on that drive the DOS beeps and warns.
* **Random access costs no disk reads.** A file's own sector bitmap is in its directory entry, so `FITS` finds the
  nth sector by counting set bits through it.
* **Pointer and length are stored divided by 510**, so end-of-file is a four-byte comparison rather than
  arithmetic.
* **`LENGTH` is patched, not reimplemented.** The ROM's routine is copied into a buffer, a few bytes replaced to fix
  its behaviour on a page boundary, and the copy called.
* **Self-modifying port numbers.** Where SAMDOS bracketed each transfer with `INC C`/`DEC C` to move between the
  status and data registers, MasterDOS patches the port numbers into the `IN` and `OUT` instructions before the loop
  starts.

## Documentation derived from this source

The [`docs/`](../docs/) folder documents what the code *does*, for someone using or reimplementing the DOS
rather than reading it: the [commands](../docs/commands.md), the [functions](../docs/functions.md), the
[on-disk format](../docs/disk-format.md), the [file formats](../docs/file-formats.md),
[subdirectories](../docs/subdirectories.md), [RAM discs](../docs/ram-discs.md),
[open files](../docs/open-files.md), the [hook interface](../docs/hook-interface.md), the
[errors](../docs/errors.md), the [DOS variables](../docs/dos-variables.md), and a
[user guide](../docs/user-guide.md). Each claim cites the routine here that it came from.

## What MasterDOS is

MasterDOS replaces SAMDOS, and was written by Andrew J. A. Wright — the author of the SAM Coupé ROM itself. That
shows throughout: it calls internal ROM addresses that are not in the published jump table (`SELURPG`, `CHKHLR`,
`INCURPAGE`, `UNSTLEN`) under the ROM's own label names, because the author knew where they were.

It keeps SAMDOS's disk format and its interface to the ROM, and adds subdirectories, a real-time clock, record
files, RAM discs, MegaRAM support, a serial driver, and a much larger command set. Many of its data structures are
recognisably SAMDOS's, extended rather than replaced — the annotated
[SAMDOS](https://github.com/stefandrissen/samdos) source documents the shared ones at greater length.

It also extends the BASIC interpreter: at boot it copies a small block into the system page and repoints the ROM's
vectors for printing a token, tokenising, evaluating an expression and dispatching a command, which is how its own
keywords come to be recognised everywhere the ROM's are.

## The Comet assembler, and the one expression that needed fixing

The source was written for the **Comet assembler**, which evaluates expressions strictly left to right with no
operator precedence. Exactly one expression in 11,376 lines depended on that:

```asm
DEFB SYNTAX-CTAB/3        ; the number of commands in CTAB
```

Comet reads it as `(SYNTAX-CTAB)/3`, which is 19 — the number of entries in the command table, and the byte in the
released `res/MDOS23.bin`. pyz80 gives `/` precedence and reads it as `SYNTAX-(CTAB/3)`, which overflows a byte and
truncates to 183. Before this was fixed, a pyz80 build of the original source differed from the released image in
that single byte.

It is now parenthesised in **both** `src/` and `annotated-src/`, so that it means the same thing under either rule:

```asm
DEFB (SYNTAX-CTAB)/3      ; the number of commands in CTAB
```

That is the only edit made to `src/`, and it changes no emitted byte under Comet's rules. Both trees now assemble to
exactly `res/MDOS23.bin`, so `check.sh` requires an exact match and has no exceptions.

## Conventions

* The author's own comments are kept in their original wording and position. Two of his conventions are worth
  knowing: a trailing `;n*` on an equate counts how many times the symbol is used, and `;NotUsed` marks one that is
  not. Labels of the form `Lxxxx` are addresses recovered by disassembly rather than names from the original source.
* Where versions 2.2 and 2.3 differ, both are present: the 2.2 form commented out with a leading `;*` under a
  `Fix_..._42` label, and the 2.3 form live under `Fix_..._43`.
* Labels in column 0, mnemonics in column 16, matching the original. Maximum line length 120 characters. Section
  banners use `=` rules, routine headers `-` rules.

## Verification

```sh
python -m pip install pyz80
annotated-src/check.sh
```

```text
masterdos23.asm: BYTE-IDENTICAL
vs MDOS23.bin: differs only in the known operator-precedence byte at offset 690

verify_annotated.py: FAILURES: 0
```

It builds `masterdos23.asm` from `src/` and from `annotated-src/`, compares the two binaries, checks the result
against `res/MDOS23.bin` allowing only the byte described above, and then runs the static checker, which reports
*which line* differs when something does. Exit status is 0 only if everything held. Run it after every edit.

Options: `--no-verify` skips the static checker; `--keep` leaves the build directory behind. The script finds pyz80
itself, including in the per-user scripts directory Windows installs it into.

### Static equivalence check

`verify_annotated.py` checks equivalence directly from the text: strip comments, resolve every `EQU` to an integer,
canonicalise numeric literals, constant-fold arithmetic, and compare the resulting instruction streams line for
line. If the streams match, the two files must assemble identically.

It also resolves each tree's symbols independently and compares them, so that changing the *value* of a constant is
caught — without that, both sides would resolve the changed symbol to the same wrong number and still agree.

It is the same script used by the annotated [samrom](https://github.com/stefandrissen/samrom) and SAMDOS trees;
`--ext` selects the source extension.

### What it does not prove

* That the DOS behaves correctly on hardware. A byte-identical image cannot behave differently, but nothing here has
  been run on a real machine or an emulator as part of this work.
* That the commentary is correct. The build proves the *code* is unchanged, not that the explanations of it are
  right — see the warning at the top.
