#!/bin/bash
#
# check.sh -- prove that the annotated source assembles to the same binary as the original.
#
# Builds masterdos23.asm from src/ and from annotated-src/, compares the two binaries byte for byte, and checks both
# against res/MDOS23.bin, the released MasterDOS 2.3 image. Finally the static equivalence checker is run, which
# reports *which line* differs when something does.
#
# Usage:  annotated-src/check.sh [options]
#
#   --no-verify    skip verify_annotated.py (the binary comparison alone is the authoritative check)
#   --keep         leave the build directory in place and print its path
#
# Exit status is 0 only if every comparison matched.
#
# ---------------------------------------------------------------------------------------------------------------------
# A note on the assembler
# ---------------------------------------------------------------------------------------------------------------------
#
# The source was written for the Comet assembler, which evaluates expressions strictly left to right with no operator
# precedence. pyz80 gives * and / precedence over + and -, as C does. Exactly one expression in the file depended on
# the difference -- "DEFB SYNTAX-CTAB/3", the number of commands in CTAB -- and it has been parenthesised in both
# trees so that it means the same thing under either rule. Before that change a pyz80 build differed from the
# released image in that one byte.

set -u

verify=1
keep=0

for arg in "$@"; do
    case "$arg" in
        --no-verify) verify=0 ;;
        --keep)      keep=1 ;;
        -h|--help)   sed -n '3,17p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
        *)           echo "check.sh: unknown option '$arg'" >&2; exit 2 ;;
    esac
done

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/.." && pwd)

# ---------------------------------------------------------------------------------------------------------------------
# Locate pyz80. The pip package installs a console script called "pyz80", not "pyz80.py". On Windows it lands in the
# per-user scripts directory, which is often not on PATH.
# ---------------------------------------------------------------------------------------------------------------------

if ! command -v pyz80 >/dev/null 2>&1; then
    for finder in "sysconfig.get_path('scripts','nt_user')" "sysconfig.get_path('scripts')"; do
        dir=$(python -c "import sysconfig;print($finder)" 2>/dev/null) || continue
        case "$dir" in
            [A-Za-z]:\\*) dir="/$(echo "${dir:0:1}" | tr 'A-Z' 'a-z')/$(echo "${dir:3}" | tr '\\' '/')" ;;
        esac
        if [ -x "$dir/pyz80" ] || [ -f "$dir/pyz80" ]; then
            PATH="$PATH:$dir"
            export PATH
            break
        fi
    done
fi

if ! command -v pyz80 >/dev/null 2>&1; then
    echo "check.sh: pyz80 not found. Install it with:  python -m pip install pyz80" >&2
    exit 2
fi

# ---------------------------------------------------------------------------------------------------------------------

work=$(mktemp -d) || exit 2
if [ "$keep" -eq 0 ]; then
    trap 'rm -rf "$work"' EXIT
fi

status=0

# build <source-dir> <top-level-source> <output-binary> <label>
build() {
    local dir=$1 src=$2 out=$3 label=$4
    local log="$work/$(basename "$out").log"
    if ( cd "$dir" && pyz80 --obj="$out" -o "$work/$(basename "$out").dsk" "$src" ) >"$log" 2>&1; then
        return 0
    fi
    echo "*** $label BUILD FAILED ***"
    tail -30 "$log"
    return 1
}

# compare <binary-a> <binary-b> <label>
compare() {
    local a=$1 b=$2 label=$3
    if cmp -s "$a" "$b"; then
        echo "$label: BYTE-IDENTICAL"
        return 0
    fi
    echo "*** $label DIFFERS ***"
    cmp -l "$a" "$b" | head -20
    echo "differing bytes: $(cmp -l "$a" "$b" | wc -l)"
    return 1
}

# --- The DOS image ------------------------------------------------------------------------------------------------

build "$root/src" masterdos23.asm "$work/orig.bin" "ORIGINAL" || exit 1
build "$here"     masterdos23.asm "$work/anno.bin" "ANNOTATED" || exit 1

compare "$work/orig.bin" "$work/anno.bin" "masterdos23.asm" || status=1

# --- Against the released image -------------------------------------------------------------------------------------

reference="$root/res/MDOS23.bin"
if [ -f "$reference" ]; then
    compare "$reference" "$work/anno.bin" "vs MDOS23.bin" || status=1
fi

# --- Static equivalence check ---------------------------------------------------------------------------------------

if [ "$verify" -eq 1 ] && [ -f "$here/verify_annotated.py" ]; then
    echo
    if out=$(cd "$root" && python "$here/verify_annotated.py" --ext=.asm "$root/src" "$here" 2>&1); then
        echo "verify_annotated.py: $(echo "$out" | tail -1)"
    else
        echo "$out" | awk '/\*\* MISMATCH/ {show=1} /^  (OK|--) / {show=0} show'
        echo "verify_annotated.py: $(echo "$out" | tail -1)"
        status=1
    fi
fi

if [ "$keep" -eq 1 ]; then
    echo
    echo "build directory kept at $work"
fi

exit $status
