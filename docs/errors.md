# MasterDOS 2.3 — Error Codes and Messages

From `ERRTBL` and `DERR` in [masterdos23.asm](../annotated-src/masterdos23.asm).

## How a DOS error is reported

The ROM handles codes 0 to 80 from its own message table. **Code 81 and above are the DOS's.** When the ROM
meets one it reads the word at the DOS page + &0210, which is the address of `ERRTBL`, indexes it by
$`\text{code} - 81`$ by stepping over that many messages, and prints from there until a character with bit
7 set.

An error is raised with `RST &08` followed by the code byte, exactly as the ROM raises its own.

## Messages are compressed

`ERRTBL` is not plain text. **Bytes below 32 are indices into the ROM's own dictionary of common substrings**,
which the ROM's message printer expands. The equates in the source name them, and they match the ROM's
`COMPLIST` table exactly:

Leading and trailing spaces are shown as `␣`.

| Value | Name in the source | Expands to |
|---|---|---|
| 0 | `INVALID` | `Invalid␣` |
| 7 | `ERROR` | `Error` |
| 8 | `TREAM` | `tream` |
| 11 | `NO` | `No␣` |
| 17 | `SNOTS` | `␣not␣` |
| 18 | `SNAME` | `␣name` |
| 20 | `TOOMANY` | `Too␣many␣` |
| 21 | `TATEMENT` | `tatement` |
| 23 | `FILE` | `file` |

So the entry for code 94 is `"Wrong "`, byte 23, `" type"` — which prints as `Wrong file type`. A tool
reading the table must expand these, not treat them as control characters.

## The codes

The table is indexed from 81, and its entries are numbered 0 upward in the source's own comments. An entry of
a single space is a code MasterDOS does not use, or leaves the ROM to report.

| Code | Index | Message |
|---|---|---|
| 81 | 0 | *(unused)* |
| 82 | 1 | *(unused)* |
| 83 | 2 | *(unused)* |
| 84 | 3 | `Escape requested` |
| 85 | 4 | `TRK-nnn,SCT-nn,Error` |
| 86 | 5 | `Format TRK-nnn lost` |
| 87 | 6 | `Check disk in drive` |
| 88 | 7 | *(unused)* |
| 89 | 8 | *(unused)* |
| 90 | 9 | *(unused)* |
| 91 | 10 | `Invalid device` |
| 92 | 11 | *(unused)* |
| 93 | 12 | `Verify failed` |
| 94 | 13 | `Wrong file type` |
| 95–98 | 14–17 | *(unused)* |
| 99 | 18 | `Reading a write file` |
| 100 | 19 | `Writing a read file` |
| 101 | 20 | `No AUTO* file` |
| 102 | 21 | *(unused)* |
| 103 | 22 | `No such drive` |
| 104 | 23 | `Disk is write protected` |
| 105 | 24 | `Disk full` |
| 106 | 25 | `Directory full` |
| 107 | 26 | `File not found` |
| 108 | 27 | *(unused)* |
| 109 | 28 | `File name used` |
| 110 | 29 | *(unused)* |
| 111 | 30 | `Stream used` |
| 112 | 31 | `Channel used` |
| **113** | 32 | **`Directory not found`** |
| **114** | 33 | **`Directory not empty`** |
| **115** | 34 | **`No pages free`** |
| **116** | 35 | **`PROTECTED file`** |

The last four are MasterDOS's own, for subdirectories and RAM discs.

## Why so many entries are blank

SAMDOS defined a DOS message for nearly every condition. **MasterDOS reuses the ROM's own error codes wherever
the ROM already has the right message**, and leaves the corresponding DOS entry blank. The `REP*` entry points
show it directly:

| Entry point | Reports | Message, and whose |
|---|---|---|
| `REP0` | 29 | `Not understood` — **the ROM's** |
| `REP8` | 18 | `Invalid file name` — **the ROM's** |
| `REP27` | 22 | `End of file` — **the ROM's** |
| `REP3` | 84 | `Escape requested` — the DOS's |
| `REP10` | 91 | `Invalid device` — the DOS's |
| `REP26` | 107 | `File not found` — the DOS's |

So a program that expects *End of file* as code 108, as it would be under SAMDOS, will not see it: MasterDOS
raises code **22**. The same applies to invalid file names (18, not 89) and to syntax errors in a DOS command
(29, not 81).

This is the single most likely incompatibility between the two DOSes for a program that traps errors by code.

> [!NOTE]
> The `REP*` label numbers are not the table indices and drift by one above `REP33` — there is no `REP34`, and
> `REP35` and `REP36` report codes 115 and 116, which are indices 34 and 35. Use the codes, not the labels.

## Differences from SAMDOS

MasterDOS's table is **not** message-for-message the same as SAMDOS's, even for shared codes:

| Code | SAMDOS 2 | MasterDOS 2.3 |
|---|---|---|
| 81 | `Nonsense in SAMDOS 1.1` | *(unused — the ROM reports it)* |
| 83 | `Statement end error` | *(unused)* |
| 88 | `No "BOOT" file` | *(unused)* |
| 89 | `Invalid file name` | *(unused)* |
| 105 | `Not enough space on disk` | `Disk full` |
| 108 | `End of file` | *(unused in the table)* |
| 113–116 | — | Four new messages |

Where MasterDOS's entry is a single space, the condition is either impossible or reported by the ROM from its
own table. A program that switches on the error *code* is safe; one that matches on the error *text* is not.

## What raises each one

| Message | Raised when |
|---|---|
| `Escape requested` | `ESC` during a long operation |
| `TRK-nnn,SCT-nn,Error` | A sector could not be transferred after the retry limit. The digits are patched into the message text itself by `DERR` — note the **three**-digit track field, since MasterDOS reaches track 207 |
| `Format TRK-nnn lost` | The controller failed while writing a track |
| `Check disk in drive` | No disk, or the drive is not ready. Use [`DSTAT`](functions.md#dstat) to test without provoking this |
| `Invalid device` | A device letter other than `D` |
| `Verify failed` | `VERIFY` found a difference |
| `Wrong file type` | The type found does not match the type asked for |
| `Reading a write file` | A read from a stream opened `OUT` |
| `Writing a read file` | A write to a stream opened `IN`, or `OPEN ... OUT` on a file that exists |
| `No AUTO* file` | The auto-load hook found none |
| `No such drive` | A drive outside 1–7, or drive 2 when `DVAR 2` says none is fitted |
| `Disk is write protected` | The protect tab is closed |
| `Disk full` | Allocation ran off the end of the disk |
| `Directory full` | Every entry is in use — 20 × `DTKS` of them |
| `File not found` | Nothing matched |
| `File name used` | `RENAME`'s target exists |
| `Stream used` | The stream is already attached to a non-standard channel |
| `Channel used` | The same file is already open on the same drive |
| `Directory not found` | A path component does not exist, or is not a directory |
| `Directory not empty` | `ERASE DIR` on a directory that still has contents |
| `No pages free` | `COPY`, `BACKUP` or `FORMAT` of a RAM disc found no free memory |
| `PROTECTED file` | `ERASE` matched only protected files and no `OVER` was given |

## Recursion, and errors inside the DOS

The DOS calls back into the ROM to evaluate expressions. If one of those evaluations fails with *Not
understood*, the ROM would call `SYNTAX` again — from inside the DOS. `NRFLG` prevents that: it is set on
entry to `SYNTAX` and makes a second, nested call return the error unchanged. It is cleared again on every
exit path.

## Errors and the stack

`HKSP` holds the stack pointer saved on entry to a hook, so an error inside a hook unwinds to the ROM routine
that called it rather than to the command loop. `SYNTAX` zeroes it, meaning "go back to BASIC". Every exit
restores `SP` to &7FFA, which is correct for both a command and a hook.

## Trapping from BASIC

`ON ERROR` works normally. For anything that can be tested in advance, prefer the functions:

```basic
IF DSTAT(1,1) = -1 THEN PRINT "no disk": STOP
IF DSTAT(1,3) < needed THEN PRINT "not enough room": STOP
IF DSTAT(1,4) = 0 THEN PRINT "directory full": STOP
IF FSTAT(f$,1) = 0 THEN PRINT "no such file": STOP
IF DSTAT(1,2) THEN PRINT "write protected": STOP
```

None of those raises an error, which is the point of them.
