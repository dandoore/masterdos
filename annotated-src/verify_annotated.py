#!/usr/bin/env python3
"""Verify that an annotated assembly file emits the same code as the original.

The check is textual but semantic: comments and blank lines are discarded,
every symbol that resolves to a number through an EQU is replaced by that
number, all numeric literals are canonicalised to decimal, and the resulting
instruction streams are compared line for line.

If the streams match, the two files must assemble identically -- annotation
and constant-naming cannot have changed the emitted bytes.

Usage:
    verify_annotated.py [--ext=.s] ORIGINAL_DIR ANNOTATED_DIR [file ...]

--ext selects the source extension to scan for. It defaults to .asm, which is
what the SAM ROM tree uses; the SAMDOS tree uses .s.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

# --------------------------------------------------------------------------
# Lexing helpers
# --------------------------------------------------------------------------

# A quoted string or character constant; kept intact while stripping comments.
#
# The apostrophe form is only recognised when it does not directly follow an identifier character, so that the
# alternate-register syntax EX AF,AF' is not mistaken for the start of a string -- which would otherwise swallow
# the ";" of any trailing comment and make two identical instructions compare unequal.
QUOTED = re.compile(r'"[^"]*"|(?<![A-Za-z_0-9])\'[^\']*\'')

# Numeric literals. The guards on either side keep the pattern from matching part of an identifier: without the
# lookbehind SKIP1LDH is read as SKIP1L followed by the suffix-hex literal "DH", and without the lookahead the
# SAMDOS symbol fdh.number is read as the suffix-hex literal "fdh" followed by ".number".
NUMBER = re.compile(
    r"""
    (?<![A-Za-z_0-9])
    (
      &[0-9A-Fa-f]+          # &FF   pyz80 hex
    | \$[0-9A-Fa-f]+         # $FF   alternative hex
    | 0[xX][0-9A-Fa-f]+      # 0xFF
    | %[01]+                 # %1010 binary
    | [0-9A-Fa-f]+[Hh]       # 0FFh  suffix hex
    | \d+                    # 123   decimal
    )
    (?![A-Za-z_0-9.])
    """,
    re.VERBOSE,
)

# Identifiers may contain dots: the SAMDOS source uses them as a namespace separator (gnd.bank, org.adjust,
# comm.port.1), and this annotation follows that convention for the constants it adds.
IDENT = re.compile(r"[A-Za-z_][A-Za-z_0-9.]*")

EQU_LINE = re.compile(
    r"^\s*([A-Za-z_][A-Za-z_0-9.]*)\s*:?\s+EQU\s+(.+?)\s*$",
    re.IGNORECASE,
)


def strip_comment(line: str) -> str:
    """Remove a trailing ; comment, respecting quoted strings."""
    out: list[str] = []
    i = 0
    while i < len(line):
        ch = line[i]
        if ch in "\"'":
            m = QUOTED.match(line, i)
            if m:
                out.append(m.group(0))
                i = m.end()
                continue
        if ch == ";":
            break
        out.append(ch)
        i += 1
    return "".join(out).rstrip()


def parse_number(tok: str) -> int:
    if tok.startswith("&") or tok.startswith("$"):
        return int(tok[1:], 16)
    if tok.lower().startswith("0x"):
        return int(tok[2:], 16)
    if tok.startswith("%"):
        return int(tok[1:], 2)
    if tok[-1] in "Hh":
        return int(tok[:-1], 16)
    return int(tok, 10)


# --------------------------------------------------------------------------
# Symbol table
# --------------------------------------------------------------------------


def collect_equs(paths: list[Path]) -> dict[str, str]:
    """Map SYMBOL -> raw right-hand side for every EQU in the given files."""
    raw: dict[str, str] = {}
    for path in paths:
        if not path.exists():
            continue
        for line in path.read_text(errors="replace").splitlines():
            body = strip_comment(line)
            m = EQU_LINE.match(body)
            if m:
                raw[m.group(1).upper()] = m.group(2)
    return raw


def resolve(raw: dict[str, str]) -> dict[str, int]:
    """Evaluate EQU right-hand sides to integers, iterating for dependencies."""
    values: dict[str, int] = {}
    for _ in range(12):  # depth of EQU chaining is tiny in practice
        progressed = False
        for name, expr in raw.items():
            if name in values:
                continue
            val = try_eval(expr, values)
            if val is not None:
                values[name] = val
                progressed = True
        if not progressed:
            break
    return values


def try_eval(expr: str, values: dict[str, int]) -> int | None:
    """Evaluate a simple assembler expression, or None if it can't be done."""
    # Character constants: "A" / 'A'
    def char_sub(m: re.Match[str]) -> str:
        text = m.group(0)[1:-1]
        return str(ord(text)) if len(text) == 1 else "\x00UNRESOLVED"

    work = QUOTED.sub(char_sub, expr)
    if "UNRESOLVED" in work:
        return None

    work = NUMBER.sub(lambda m: str(parse_number(m.group(0))), work)

    def ident_sub(m: re.Match[str]) -> str:
        name = m.group(0).upper()
        if name in values:
            return str(values[name])
        return "\x00UNRESOLVED"

    work = IDENT.sub(ident_sub, work)
    if "UNRESOLVED" in work:
        return None

    # pyz80 uses \ for integer division in the sources we care about.
    work = work.replace("\\", "//")
    if not re.fullmatch(r"[-+*/()<>| 0-9]+", work.strip()):
        return None
    try:
        return int(eval(work, {"__builtins__": {}}, {}))  # noqa: S307 - arithmetic only
    except Exception:
        return None


# --------------------------------------------------------------------------
# Normalisation
# --------------------------------------------------------------------------


def normalise(line: str, values: dict[str, int]) -> str | None:
    """Reduce a source line to a canonical code form, or None if it emits nothing."""
    body = strip_comment(line)
    if not body.strip():
        return None

    # Drop a standalone label definition line -- labels are position markers and
    # the annotated file may put them on their own line.
    stripped = body.strip()

    # EQU lines emit nothing.
    if EQU_LINE.match(body):
        return None

    # Split off a leading label so "LBL: OP" and "LBL:\n OP" compare equal.
    label = ""
    m = re.match(r"^\s*([A-Za-z_][A-Za-z_0-9]*)\s*:(.*)$", body)
    if m:
        label = m.group(1).upper()
        stripped = m.group(2).strip()
    elif not body[:1].isspace():
        # Label without a colon (pyz80 allows it) followed by an opcode.
        parts = body.split(None, 1)
        label = parts[0].upper()
        stripped = parts[1].strip() if len(parts) > 1 else ""

    if not stripped:
        return f"LABEL {label}" if label else None

    # Canonicalise the instruction text.
    text = stripped

    def char_sub(m: re.Match[str]) -> str:
        s = m.group(0)[1:-1]
        if len(s) == 1:
            return str(ord(s))
        return '"' + s + '"'

    text = QUOTED.sub(char_sub, text)
    text = NUMBER.sub(lambda mm: str(parse_number(mm.group(0))), text)

    def ident_sub(mm: re.Match[str]) -> str:
        name = mm.group(0).upper()
        return str(values[name]) if name in values else name

    text = IDENT.sub(ident_sub, text)
    text = re.sub(r"\s+", " ", text).strip().upper()
    text = text.replace(", ", ",").replace(" ,", ",")
    text = fold_operands(text)

    return (f"{label}: " if label else "") + text


# Operators the folder understands. The shift and or operators are included because a bit mask is most clearly
# written as "1<<bit | 1<<bit"; & is deliberately left out, since it is pyz80's hex prefix rather than an operator.
ARITH_ONLY = re.compile(r"^[-+*/()<>| 0-9\\]+$")


def split_top_level(s: str) -> list[str]:
    """Split on commas that are not inside parentheses."""
    parts, depth, cur = [], 0, []
    for ch in s:
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
        if ch == "," and depth == 0:
            parts.append("".join(cur))
            cur = []
        else:
            cur.append(ch)
    parts.append("".join(cur))
    return parts


ADD_SPLIT = re.compile(r"([+-])")


def fold_numeric_tail(op: str) -> str:
    """Fold the numeric terms of an expression whose first term is an unresolvable label.

    Labels have no value until the file is assembled, so an operand like "uifa+hdr.start+1" cannot be reduced to a
    number -- but the constants in it can still be added up, giving "uifa+32", which is what the original source
    spells directly. Only chains of + and - are handled, so there is no precedence to get wrong.
    """
    if any(c in op for c in "()*/\\<>|"):
        return op
    parts = ADD_SPLIT.split(op)
    if len(parts) < 3:
        return op
    head = parts[0].strip()
    if not head:
        return op
    total = 0
    rest = parts[1:]
    for i in range(0, len(rest), 2):
        term = rest[i + 1].strip()
        if not term.isdigit():
            return op
        total += int(term) if rest[i] == "+" else -int(term)
    if total == 0:
        return head
    return f"{head}{'+' if total > 0 else '-'}{abs(total)}"


def fold_operands(text: str) -> str:
    """Constant-fold arithmetic in operands so &0400+34 and (4*256)+34 compare equal.

    Folded values are reduced modulo 65536, because that is the widest operand a Z80
    instruction has: -128 and &FF80 name the same word, so LD HL,-SCANBYTESM23 and
    LD HL,&FF80 must compare equal.

    An operand that is entirely wrapped in parentheses is memory indirection, not
    arithmetic, and is left alone."""
    if " " not in text:
        return text
    mnem, _, rest = text.partition(" ")
    out = []
    for operand in split_top_level(rest):
        op = operand.strip()
        wrapped = op.startswith("(") and op.endswith(")") and split_top_level(op[1:-1]) == [op[1:-1]] \
            and balanced(op[1:-1])
        # An indirection keeps its parentheses -- that is what distinguishes LD HL,(CHAD) from arithmetic -- but the
        # address inside one is still an expression and is folded, so (uifa+31+1) and (uifa+32) compare equal.
        body = op[1:-1] if wrapped else op
        if ARITH_ONLY.match(body) and re.search(r"[-+*/\\<>|]", body):
            try:
                val = int(eval(body.replace("\\", "//"), {"__builtins__": {}}, {}))  # noqa: S307
                body = str(val & 0xFFFF)
            except Exception:
                pass
        else:
            body = fold_numeric_tail(body)
        op = "(" + body + ")" if wrapped else body
        out.append(op)
    return mnem + " " + ",".join(out)


def balanced(s: str) -> bool:
    depth = 0
    for ch in s:
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth < 0:
                return False
    return depth == 0


INCLUDE_LINE = re.compile(r'^\s*INCLUDE\s+"([^"]+)"', re.IGNORECASE)


def code_stream(path: Path, values: dict[str, int], inert: set[str] | None = None) -> list[tuple[int, str]]:
    """Normalised code lines. Includes of files that emit no bytes are dropped, so
    moving a pure-EQU include (as the annotated build does) is not flagged."""
    out: list[tuple[int, str]] = []
    for n, line in enumerate(path.read_text(errors="replace").splitlines(), 1):
        m = INCLUDE_LINE.match(strip_comment(line))
        if m and inert and m.group(1).lower() in inert:
            continue
        norm = normalise(line, values)
        if norm is not None:
            out.append((n, norm))
    return out


def find_inert(dirs: list[Path], values: dict[str, int], ext: str) -> set[str]:
    """Names of source files that emit no bytes at all (pure EQU/comment files)."""
    inert: set[str] = set()
    for d in dirs:
        for p in d.glob("*" + ext):
            if not code_stream(p, values):
                inert.add(p.name.lower())
    return inert


# --------------------------------------------------------------------------
# Driver
# --------------------------------------------------------------------------


def main(argv: list[str]) -> int:
    args = list(argv[1:])
    ext = ".asm"
    for i, a in enumerate(args):
        if a.startswith("--ext="):
            ext = a.split("=", 1)[1]
            if not ext.startswith("."):
                ext = "." + ext
            del args[i]
            break

    if len(args) < 2:
        print(__doc__)
        return 2

    orig_dir = Path(args[0])
    anno_dir = Path(args[1])
    names = args[2:] or sorted(p.name for p in anno_dir.glob("*" + ext))

    # Symbols come from both trees so either naming scheme resolves.
    equ_sources = sorted(orig_dir.glob("*" + ext)) + sorted(anno_dir.glob("*" + ext))
    values = resolve(collect_equs(equ_sources))
    inert = find_inert([orig_dir, anno_dir], values, ext)

    failures = 0

    # Because the two trees share one symbol table, a symbol whose *value* was changed would resolve to the same
    # (wrong) number on both sides and the streams would still match. Resolve each tree on its own and compare, so
    # that redefining a constant is caught. Symbols added by the annotation appear only in the second table and are
    # not a difference.
    orig_values = resolve(collect_equs(sorted(orig_dir.glob("*" + ext))))
    anno_values = resolve(collect_equs(sorted(anno_dir.glob("*" + ext))))
    changed = sorted(
        (name, orig_values[name], anno_values[name])
        for name in orig_values.keys() & anno_values.keys()
        if orig_values[name] != anno_values[name]
    )
    for name, was, now in changed:
        print(f"  ** SYMBOL {name}: {was} in the original, {now} in the annotated tree")
    if changed:
        failures += 1
    for name in names:
        orig = orig_dir / name
        anno = anno_dir / name
        if not orig.exists():
            print(f"  --  {name}: no original (new file, skipped)")
            continue

        a = code_stream(orig, values, inert)
        b = code_stream(anno, values, inert)

        if [x[1] for x in a] == [x[1] for x in b]:
            print(f"  OK  {name}  ({len(a)} code lines)")
            continue

        failures += 1
        print(f"  ** MISMATCH {name}: {len(a)} original vs {len(b)} annotated code lines")
        shown = 0
        for i in range(max(len(a), len(b))):
            ea = a[i] if i < len(a) else (0, "<missing>")
            eb = b[i] if i < len(b) else (0, "<missing>")
            if ea[1] != eb[1]:
                print(f"       original:{ea[0]:>5}  {ea[1]}")
                print(f"      annotated:{eb[0]:>5}  {eb[1]}")
                shown += 1
                if shown >= 12:
                    print("       ... (further differences suppressed)")
                    break

    print()
    print("FAILURES:", failures)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
