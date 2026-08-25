# MasterDOS 2.3 — Subdirectories

MasterDOS's largest addition to the SAM disk format — and it changes the format not at all. From part `SUBD`
of [masterdos23.asm](../annotated-src/masterdos23.asm): `OPNDIR`, `STDIR`, `DTREE`, `FNDIR`, `GPATD`, `GPLA`.

## How a tree is stored in a flat directory

The catalogue is still the same flat list of entries in tracks 0 to `DTKS`−1. **Two spare bytes in each entry
carry the whole tree:**

| Offset | Field | Meaning |
|---|---|---|
| 254 (&FE) | Parent tag | The tag of the directory this file belongs to. **0 means the root** |
| 250 (&FA) | Own tag (`DIRT`) | For an entry of type 21 only: the tag it gives to the files inside it |

So a subdirectory is an ordinary directory entry that **owns a number**, and "being in a directory" means
"carrying that number in byte 254". Nothing else in the DOS has to know about it: the free-space map, the
allocation, the sector chains and the file headers are all untouched.

```text
entry  type  name        own tag (250)  parent tag (254)
  0     19   MDOS23           -               0          root
  1     21   GAMES            1               0          root, is a directory
  2     19   CHESS            -               1          inside GAMES
  3     21   SAVES            2               1          inside GAMES, is a directory
  4     10   GAME1            -               2          inside GAMES\SAVES
```

Listing a directory means showing only the entries whose byte 254 matches `CDIRT`, the current tag. Setting
`CDIRT` to &FF shows all of them, which is what `DIR ?` does.

### Tags are allocated, not derived

`FDHR` records the highest tag it sees in `MAXT` (`DVAR 21`) as it scans. `OPNDIR` gives a new directory
`MAXT` + 1. Tags are therefore **per disk**, dense, and never reused — deleting a directory leaves a hole in
the numbering, and a disk cannot hold more than 255 directories over its lifetime without the count wrapping.

`CDIRT` is `DVAR 17–18`, and each drive's current tag is remembered separately in `CDIT` (`DVAR 49–55`);
`DRSET` loads the right one whenever a drive is selected.

### A directory has no data

A directory entry has no sectors at all. `OPNDIR` sets flag bit 4, which tells `OFSM` not to allocate a first
sector, so the entry's sector count is zero and its sector map is empty. Creating a directory costs one
directory slot and nothing else; deleting one frees nothing.

## What this buys, and what it costs

| | |
|---|---|
| **No format change** | A MasterDOS disk with subdirectories is still a valid MGT disk |
| **No extra reads** | The tag is already in the entry the scan is reading |
| **Cheap to create** | One entry, no sectors |
| **SAMDOS sees the files** | Every file is still in the flat catalogue, wherever it lives in the tree |
| **SAMDOS sees them *all*** | There is no hiding: a SAMDOS `DIR` lists every file on the disk regardless of directory, and names may collide |
| **The directory limit is shared** | 80 entries on a standard disk, tree or no tree. Deep trees consume slots on directories themselves |
| **A directory entry looks broken to SAMDOS** | Type 21 is unknown, so it shows as a zero-length `WHAT?`. Erasing it under SAMDOS orphans everything inside |

## Paths

The current directory is remembered per drive in two forms:

| | Where | `DVAR` |
|---|---|---|
| As a tag | `CDIT`, one byte per drive | 49–55 |
| As a printable string | `PTH1` (drive 1), `PTH2` (drive 2), or the RAM disc's own page | — |
| Its length | `PLT`, one byte per drive | 56–62 |

The maximum path length is 38 characters (`MPL`). The default is 2 — the length of `"1:"`.

**Root symbols.** `DVAR 12–13` (`RTSYM`) holds two characters, normally `\` and `/`. Both are accepted on
input; the first is always the one written into the path. Changing `DVAR 12` changes what `PATH$` looks like.

**`^` means the parent.** `UPDIR` searches the path string backwards for the last root symbol and truncates
there.

### `DTREE`: walking a path

`DTREE` takes a string and breaks it into directory names, selecting each in turn and leaving the **final
component** in `NSTR1` for the caller. That is why a path works anywhere a file name is accepted, not only in
`DIR =`:

```basic
LOAD "\GAMES\CHESS"
ERASE "d2:\OLD\*.bak"
OPEN #4, "^\DATA\log" RND
```

| Leading character | Effect |
|---|---|
| `\` or `/` | Start from the root, replacing the current path |
| `^` | Start from the parent |
| anything else | Start from the current directory, appending |

A path that names a directory which does not exist gives *Directory not found*.

## Changing directory

```text
DIR = "path"
DIR = "\"
DIR = "^"
```

`STDIR` recognises three cases without touching the disk: a bare root symbol resets to the root, a bare `^`
moves up one level, and anything else is a path to descend.

**Whether the new name replaces or extends the path depends on its first character**, and this is the one
place the distinction bites:

```basic
DIR = "GAMES"      : REM append: 1:\  becomes  1:\GAMES
DIR = "\GAMES"     : REM replace: 1:\ANYTHING  becomes  1:\GAMES
```

`STDIR` sets the path length to 2 — just `"1:"` — before appending when the name starts with a root symbol,
so `DIR = "\GAMES\SCRABBLE"` gives `1:\GAMES\SCRABBLE` whatever the path was before.

`TEMPW1` keeps the previous path length, which is what lets `^` back out again.

## Creating a directory

```text
OPEN DIR "name"
```

`OPEN` falls through to `OPNDIR` when what follows is not `#`. The new directory is created **in the current
directory**, carrying the current `CDIRT` as its parent tag, and is given `MAXT` + 1 as its own tag.

## Deleting and renaming

```text
ERASE DIR "name"
RENAME DIR "old" TO "new"
```

`ERASE DIR` refuses unless the directory is **empty**: it puts the directory's own tag into `CDIRT` and
searches the catalogue for any entry carrying it. *Directory not empty* if one is found.

There is no recursive delete. Emptying a tree means walking it from the leaves up.

`RENAME DIR` changes the name only; the tags are untouched, so nothing inside moves.

## What resets the current directory

| Event | Effect |
|---|---|
| A disk with a different [random word](disk-format.md#disk-change-detection) is read | That drive's directory resets to the **root**, because the tag belonged to another disk's tree |
| `DIR = "\"` | Root |
| Selecting a different drive | Nothing — each drive keeps its own current directory in `CDIT` |

The disk-swap reset is important: without it, a tag from the old disk would silently select an unrelated
directory on the new one.

## Enumerating a tree from a program

Everything needed is in the entries themselves, so a tree can be walked without any DOS support at all:

1. Read every directory entry.
2. Build a map from tag (byte 250) to entry, for entries of type 21.
3. For each entry, byte 254 gives its parent tag; 0 is the root.
4. A directory's children are the entries whose byte 254 equals its byte 250.

From BASIC, `DIR ?` lists every file on the disk regardless of directory, and `DSTAT(d,5)` and `DSTAT(d,6)`
give the total file count and the count in the current directory — the difference being what is elsewhere in
the tree.
