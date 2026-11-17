#!/usr/bin/env python3
"""T07.3: interface controlada para auditorias históricas previamente revisadas.

Nenhum caminho sob scripts/ é alterado. Somente PNAD E10 N2 é executável;
os demais casos são explicitamente bloqueados até controle específico de escrita.
"""
from __future__ import annotations

import argparse
import csv
import hashlib
import io
import json
import os
from pathlib import Path
import re
import subprocess
import sys
from datetime import datetime, timezone

ROOT = Path(__file__).resolve().parents[2]
MANIFEST = ROOT / "reports/t07_2/manifesto_codigos_originais_sha256.csv"
MANIFEST_REL = "reports/t07_2/manifesto_codigos_originais_sha256.csv"
RUN_ID = re.compile(r"[a-z0-9][a-z0-9_-]{0,63}\Z")
# Categorias de outputs/README.md; diretórios de execução são sempre exclusivos.
OUTPUT_KINDS = frozenset({"figures", "tables", "models", "logs", "cmdstan_csv", "data_stan"})
STUDIES = {
    "pnadc_estudo_i": {
        "script": "scripts/pnadc_estudo_i/auditar_e10.py",
        "input": "data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv",
        "kind": "python_read_only",
        "allowed": True,
        "reason": "Auditoria descritiva E10 N2 pública; não executa E04-E08, Stan ou coleta",
    },
    "mortalidade_sp_0_74": {
        "script": "scripts/mortalidade_sp_0_74/reproduzir_E10.R",
        "input": "data/raw/mortalidade_sp_0_74/resultados_auditados/",
        "kind": "r_writes_fixed_outputs",
        "allowed": False,
        "reason": "O original cria/grava outputs fixos; isolamento de escritas requer gate T07.4",
    },
    "caes_raca_f5": {
        "script": "scripts/caes_raca_f5/caes_raca_f5_cmdstanr.R",
        "input": "data/raw/caes_raca_f5/resultados_auditados/",
        "kind": "r_writes_fixed_outputs",
        "allowed": False,
        "reason": "Original grava tabelas/logs fixos e contém rota Stan proibida neste adaptador",
    },
}


class GateError(Exception):
    """Bloqueio explícito de contrato, integridade ou execução."""


def _is_regular_file(path: Path) -> bool:
    return path.is_file() and not path.is_symlink()


def checked_regular(root: Path, rel: str, label: str) -> Path:
    """Aceita arquivo regular sob a raiz sem links simbólicos nos ancestrais.

    A verificação é prévia à abertura, não constitui sandbox nem elimina TOCTOU.
    """
    target = root / rel
    if not _is_regular_file(target) or root.is_symlink():
        raise GateError(f"{label} ausente ou symlink: {rel}")
    parent = target.parent
    while parent != root:
        if parent == parent.parent:
            raise GateError(f"{label} fora da raiz: {rel}")
        if parent.is_symlink() or not parent.is_dir():
            raise GateError(f"{label} com ancestral inválido: {rel}")
        parent = parent.parent
    return target


def hash_checked_input(root: Path, rel: str) -> str:
    path = checked_regular(root, rel, "Entrada")
    return hashlib.sha256(path.read_bytes()).hexdigest()


def checked_source_with_manifest(root: Path, rel: str) -> tuple[Path, str, tuple[int, int, int, int]]:
    """Valida o código com bytes de manifesto capturados em uma única leitura.

    Devolve o hash dos mesmos bytes do manifesto usados na autorização.
    Isso detecta alterações persistentes; não constitui isolamento anti-TOCTOU.
    """
    source = checked_regular(root, rel, "Código histórico")
    manifest = checked_regular(root, MANIFEST_REL, "Manifesto T07.2")
    before = manifest.stat()
    manifest_bytes = manifest.read_bytes()
    after = checked_regular(root, MANIFEST_REL, "Manifesto T07.2").stat()
    identity = lambda stat: (stat.st_dev, stat.st_ino, stat.st_size, stat.st_mtime_ns)
    if identity(before) != identity(after):
        raise GateError("Manifesto T07.2 mudou durante a validação")
    manifest_sha256 = hashlib.sha256(manifest_bytes).hexdigest()
    with io.StringIO(manifest_bytes.decode("utf-8"), newline="") as handle:
        rows = list(csv.DictReader(handle))
    matches = [r for r in rows if r.get("path") == rel]
    if len(matches) != 1:
        raise GateError(f"Caminho ausente/duplicado no manifesto T07.2: {rel}")
    expected = matches[0].get("sha256", "")
    if not re.fullmatch(r"[0-9a-f]{64}", expected):
        raise GateError("Hash SHA-256 do manifesto inválido")
    if hashlib.sha256(source.read_bytes()).hexdigest() != expected:
        raise GateError("Código original divergiu do manifesto T07.2")
    return source, manifest_sha256, identity(after)


def checked_source(root: Path, rel: str) -> Path:
    """Mantém o contrato da T07.3 para validação da fonte isoladamente."""
    return checked_source_with_manifest(root, rel)[0]


def canonical_output(root: Path, study: str, kind: str, run_id: str) -> Path:
    """Normaliza saídas do adaptador para outputs/<estudo>/<tipo>/<id>."""
    if study not in STUDIES:
        raise GateError(f"Estudo não admitido: {study}")
    if kind not in OUTPUT_KINDS:
        raise GateError(f"Tipo de saída não canônico: {kind}")
    if not RUN_ID.fullmatch(run_id):
        raise GateError("--id deve conter apenas minúsculas, números, _ ou - (1–64)")
    root = Path(root)
    parent = root / "outputs" / study / kind
    for folder in (root, root / "outputs", root / "outputs" / study, parent):
        if folder.is_symlink():
            raise GateError(f"Diretório de saída é symlink: {folder}")
        if folder.exists() and not folder.is_dir():
            raise GateError(f"Componente da saída não é diretório: {folder}")
    return parent / run_id


def safe_output(root: Path, study: str, run_id: str) -> Path:
    """Compatibilidade T07.3: toda execução permitida usa logs/ por ID."""
    return canonical_output(root, study, "logs", run_id)


def reserve_output(root: Path, study: str, kind: str, run_id: str) -> Path:
    """Reserva novo diretório: nunca reutiliza uma execução existente."""
    target = canonical_output(root, study, kind, run_id)
    target.parent.mkdir(parents=True, exist_ok=True)
    # Verificar novamente após criação dos ancestrais (mitiga, não elimina TOCTOU).
    canonical_output(root, study, kind, run_id)
    target.mkdir(exist_ok=False)
    return target


def write_new_artifact(target: Path, filename: str, content: str) -> None:
    """Cria arquivo exclusivo, inclusive contra arquivo/link já existente."""
    if filename not in {"stdout.txt", "stderr.txt", "report.json"}:
        raise GateError(f"Artefato inesperado: {filename}")
    if target.is_symlink() or not target.is_dir():
        raise GateError(f"Diretório de execução inválido: {target}")
    flags = os.O_WRONLY | os.O_CREAT | os.O_EXCL
    if hasattr(os, "O_NOFOLLOW"):
        flags |= os.O_NOFOLLOW
    fd = os.open(target / filename, flags, 0o600)
    with os.fdopen(fd, "w", encoding="utf-8") as f:
        f.write(content)


def plan(study: str, run_id: str, root: Path = ROOT) -> dict:
    spec = STUDIES[study]
    output = safe_output(root, study, run_id)
    return {
        "contract": "T07.3/v1", "study": study, "mode": "auditoria_rapida_N2",
        "script": spec["script"], "input": spec["input"],
        "output": output.relative_to(root).as_posix(),
        "allowed_execute": spec["allowed"], "reason": spec["reason"],
        "execution": "python -I <script>" if spec["allowed"] else "BLOCKED",
        "scientific_level": "N2 somente quando houver evidência individual; não N3/N4",
    }


def execute(study: str, run_id: str, root: Path = ROOT) -> dict:
    spec = STUDIES[study]
    if not spec["allowed"]:
        raise GateError(spec["reason"])
    descriptor = plan(study, run_id, root)
    script, manifest_sha256, manifest_identity = checked_source_with_manifest(root, spec["script"])
    checked_regular(root, spec["input"], "Entrada pública")
    # Somente subprocesso Python read-only revisado; nada de R, shell ou Stan.
    command = [sys.executable, "-I", str(script)]
    target = reserve_output(root, study, "logs", run_id)
    # Inicializar o registro antes de qualquer operação pós-mkdir que possa falhar.
    record = {**descriptor, "timestamp_utc": datetime.now(timezone.utc).isoformat(),
              "command": ["python", "-I", spec["script"]],
              "state": "ERROR", "returncode": None,
              "manifest_sha256": manifest_sha256}
    postcheck = False
    try:
        record["source_sha256"] = hashlib.sha256(script.read_bytes()).hexdigest()
        record["input_sha256"] = hash_checked_input(root, spec["input"])
        env = {"PATH": os.environ.get("PATH", ""), "PYTHONDONTWRITEBYTECODE": "1",
               "PYTHONHASHSEED": "0"}
        result = subprocess.run(command, cwd=root, env=env, text=True,
                                capture_output=True, timeout=30, check=False)
        record["returncode"] = result.returncode
        write_new_artifact(target, "stdout.txt", result.stdout)
        write_new_artifact(target, "stderr.txt", result.stderr)
        # Não declarar sucesso antes de concluir TODAS as verificações.
        postcheck = True
        final_input_sha256 = hash_checked_input(root, spec["input"])
        final_source_sha256 = hashlib.sha256(
            checked_regular(root, spec["script"], "Código histórico").read_bytes()
        ).hexdigest()
        manifest_after = checked_regular(root, MANIFEST_REL, "Manifesto T07.2")
        final_manifest_sha256 = hashlib.sha256(manifest_after.read_bytes()).hexdigest()
        final_stat = manifest_after.stat()
        final_identity = (final_stat.st_dev, final_stat.st_ino,
                          final_stat.st_size, final_stat.st_mtime_ns)
        if (final_manifest_sha256 != record["manifest_sha256"]
                or final_identity != manifest_identity):
            record["state"] = "FAILED_INTEGRITY"
            record["error"] = "MANIFEST_T07_2_CHANGED"
        elif (final_input_sha256 != record["input_sha256"]
              or final_source_sha256 != record["source_sha256"]):
            record["state"] = "FAILED_INTEGRITY"
            record["error"] = "INPUT_OR_SOURCE_CHANGED"
        else:
            record["state"] = "EXECUTED_N2_CHECK" if result.returncode == 0 else "FAILED"
    except (OSError, subprocess.TimeoutExpired, GateError) as exc:
        record["state"] = "FAILED_INTEGRITY" if postcheck else "ERROR"
        record["error"] = type(exc).__name__
    finally:
        try:
            write_new_artifact(
                target, "report.json",
                json.dumps(record, ensure_ascii=False, indent=2, sort_keys=True) + "\n")
        except OSError as exc:
            raise GateError(
                f"Não foi possível preservar report.json em {target}: {type(exc).__name__}"
            ) from exc
    if record["state"] != "EXECUTED_N2_CHECK":
        raise GateError(f"Execução falhou; evidências preservadas em {target}")
    return record


def main(argv: list[str] | None = None, root: Path = ROOT) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--estudo", choices=sorted(STUDIES), required=True)
    parser.add_argument("--modo", choices=["auditoria_rapida_N2"], required=True)
    parser.add_argument("--acao", choices=["planejar", "executar"], required=True)
    parser.add_argument("--id", required=True, help="ID determinístico; impede sobrescrita")
    ns = parser.parse_args(argv)
    try:
        result = (plan(ns.estudo, ns.id, root) if ns.acao == "planejar" else
                  execute(ns.estudo, ns.id, root))
    except (GateError, FileExistsError, OSError, ValueError) as exc:
        print(f"BLOQUEADO T07.4/T07.3: {exc}", file=sys.stderr)
        return 2
    print(json.dumps(result, ensure_ascii=False, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
