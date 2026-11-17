#!/usr/bin/env python3
"""T07.2: verificação somente de leitura dos bytes originais em scripts/."""
from __future__ import annotations

import argparse
import csv
import hashlib
import re
import subprocess
import sys
from pathlib import Path, PurePosixPath

BASE_SHA = "c0fab905ed1bd3ca61926885ca1eed32f8470269"
MANIFEST = "reports/t07_2/manifesto_codigos_originais_sha256.csv"
HEADER = ["path", "sha256", "bytes", "git_blob_sha1"]
EXTENSIONS = {".R", ".py", ".ps1", ".stan"}
HEX64 = re.compile(r"[0-9a-f]{64}\Z")
HEX40 = re.compile(r"[0-9a-f]{40}\Z")


class IntegrityError(ValueError):
    pass


def is_code_path(name: str) -> bool:
    p = PurePosixPath(name)
    return name.startswith("scripts/") and p.suffix in EXTENSIONS


def git_blob_sha1(data: bytes) -> str:
    return hashlib.sha1(b"blob " + str(len(data)).encode("ascii") + b"\0" + data).hexdigest()


def load_manifest(path: Path) -> dict[str, tuple[str, int, str]]:
    try:
        with path.open("r", encoding="utf-8", newline="") as stream:
            reader = csv.DictReader(stream)
            if reader.fieldnames != HEADER:
                raise IntegrityError("cabecalho do manifesto invalido")
            entries: dict[str, tuple[str, int, str]] = {}
            order: list[str] = []
            for row in reader:
                if None in row or set(row) != set(HEADER):
                    raise IntegrityError("registro CSV invalido")
                name = row["path"]
                p = PurePosixPath(name)
                if (not is_code_path(name) or str(p) != name or
                    any(part in (".", "..") for part in name.split("/")) or
                    "\\" in name or "\x00" in name):
                    raise IntegrityError(f"caminho invalido: {name!r}")
                if name in entries:
                    raise IntegrityError(f"caminho duplicado: {name}")
                sha256, oid, size = row["sha256"], row["git_blob_sha1"], row["bytes"]
                if not HEX64.fullmatch(sha256) or not HEX40.fullmatch(oid):
                    raise IntegrityError(f"hash invalido: {name}")
                if not size.isdecimal() or (size.startswith("0") and len(size) > 1):
                    raise IntegrityError(f"tamanho invalido: {name}")
                entries[name] = (sha256, int(size), oid)
                order.append(name)
            if not entries or order != sorted(order):
                raise IntegrityError("manifesto vazio ou fora da ordem canonica")
            return entries
    except (OSError, UnicodeError, csv.Error) as exc:
        raise IntegrityError(f"manifesto inacessivel ou malformado: {exc}") from exc


def observed_files(root: Path) -> dict[str, Path]:
    base = root / "scripts"
    if not base.is_dir() or base.is_symlink():
        raise IntegrityError("diretorio scripts/ ausente ou simbolico")
    observed: dict[str, Path] = {}
    for p in base.rglob("*"):
        relative = p.relative_to(root).as_posix()
        if p.is_symlink():
            raise IntegrityError(f"link simbolico nao permitido: {relative}")
        if p.is_file() and is_code_path(relative):
            observed[relative] = p
    return observed


def verify_tree(root: Path, manifest: dict[str, tuple[str, int, str]]) -> None:
    actual = observed_files(root)
    missing, added = sorted(manifest.keys() - actual.keys()), sorted(actual.keys() - manifest.keys())
    errors = [f"original removido: {p}" for p in missing]
    errors += [f"codigo novo fora do manifesto: {p}" for p in added]
    for name in sorted(manifest.keys() & actual.keys()):
        data = actual[name].read_bytes()
        digest, size, oid = manifest[name]
        if len(data) != size:
            errors.append(f"tamanho alterado: {name}")
        if hashlib.sha256(data).hexdigest() != digest or git_blob_sha1(data) != oid:
            errors.append(f"conteudo/hash alterado: {name}")
    if errors:
        raise IntegrityError("\n".join(errors))


def git(root: Path, *args: str) -> bytes:
    try:
        return subprocess.run(
            ["git", "-C", str(root), *args], check=True,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        ).stdout
    except (OSError, subprocess.CalledProcessError) as exc:
        raise IntegrityError(f"objeto Git-base indisponivel: {exc}") from exc


def verify_baseline(root: Path, manifest: dict[str, tuple[str, int, str]]) -> None:
    """Âncora a lista e o SHA-256 aos bytes Git no commit fixado, sem executar código."""
    raw = git(root, "ls-tree", "-r", "-z", BASE_SHA, "--", "scripts")
    tree: dict[str, str] = {}
    for entry in raw.split(b"\0"):
        if not entry:
            continue
        prefix, path = entry.split(b"\t", 1)
        mode, kind, oid = prefix.split(b" ")
        name = path.decode("utf-8")
        if is_code_path(name):
            if kind != b"blob" or mode not in (b"100644", b"100755"):
                raise IntegrityError(f"tipo Git original nao regular: {name}")
            tree[name] = oid.decode("ascii")
    if set(tree) != set(manifest):
        missing = sorted(tree.keys() - manifest.keys())
        extra = sorted(manifest.keys() - tree.keys())
        raise IntegrityError(f"escopo difere do commit-base: ausentes={missing}; extras={extra}")
    for name, oid in sorted(tree.items()):
        data = git(root, "cat-file", "blob", oid)
        digest, size, saved_oid = manifest[name]
        if (oid != saved_oid or len(data) != size or
            git_blob_sha1(data) != oid or hashlib.sha256(data).hexdigest() != digest):
            raise IntegrityError(f"baseline divergente do commit {BASE_SHA}: {name}")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--manifest", type=Path, default=None)
    parser.add_argument("--check-baseline", action="store_true")
    args = parser.parse_args(argv)
    root = args.root.resolve()
    file = args.manifest or root / MANIFEST
    try:
        entries = load_manifest(file)
        verify_tree(root, entries)
        if args.check_baseline:
            verify_baseline(root, entries)
    except (IntegrityError, OSError) as exc:
        print(f"T07.2 FALHOU: {exc}", file=sys.stderr)
        return 1
    print(f"T07.2 OK: {len(entries)} codigos imutaveis; SHA-256 + git blob; "
          f"baseline={'verificado' if args.check_baseline else 'nao consultado'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
