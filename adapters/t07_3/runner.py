#!/usr/bin/env python3
"""T07.3: interface controlada para auditorias históricas previamente revisadas.

Nenhum caminho sob scripts/ é alterado. Somente PNAD E10 N2 é executável;
os demais casos são explicitamente bloqueados até controle específico de escrita.
"""
from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
from datetime import datetime, timezone

ROOT = Path(__file__).resolve().parents[2]
MANIFEST = ROOT / "reports/t07_2/manifesto_codigos_originais_sha256.csv"
RUN_ID = re.compile(r"[a-z0-9][a-z0-9_-]{0,63}\Z")
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


def checked_source(root: Path, rel: str) -> Path:
    """Exige bytes históricos iguais ao manifesto T07.2 e ausência de symlinks."""
    source = root / rel
    if not _is_regular_file(source) or any(p.is_symlink() for p in source.parents if p != root.parent):
        raise GateError(f"Código histórico ausente ou symlink: {rel}")
    if not _is_regular_file(root / "reports/t07_2/manifesto_codigos_originais_sha256.csv"):
        raise GateError("Manifesto T07.2 ausente ou symlink")
    with (root / "reports/t07_2/manifesto_codigos_originais_sha256.csv").open(
        encoding="utf-8", newline=""
    ) as handle:
        rows = list(csv.DictReader(handle))
    matches = [r for r in rows if r.get("path") == rel]
    if len(matches) != 1:
        raise GateError(f"Caminho ausente/duplicado no manifesto T07.2: {rel}")
    expected = matches[0].get("sha256", "")
    if not re.fullmatch(r"[0-9a-f]{64}", expected):
        raise GateError("Hash SHA-256 do manifesto inválido")
    if hashlib.sha256(source.read_bytes()).hexdigest() != expected:
        raise GateError("Código original divergiu do manifesto T07.2")
    return source


def safe_output(root: Path, study: str, run_id: str) -> Path:
    if not RUN_ID.fullmatch(run_id):
        raise GateError("--id deve conter apenas minúsculas, números, _ ou - (1–64)")
    dest_parent = root / "outputs" / study / "logs"
    # Não aceitar redirecionamento de diretórios de outputs por links simbólicos.
    for folder in (root, root / "outputs", root / "outputs" / study, dest_parent):
        if folder.is_symlink():
            raise GateError(f"Diretório de saída é symlink: {folder}")
        if folder.exists() and not folder.is_dir():
            raise GateError(f"Componente da saída não é diretório: {folder}")
    return dest_parent / run_id


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
    script = checked_source(root, spec["script"])
    inp = root / spec["input"]
    if not _is_regular_file(inp) or inp.parent.is_symlink():
        raise GateError(f"Entrada pública ausente ou symlink: {spec['input']}")
    # Somente subprocesso Python read-only revisado; nada de R, shell ou Stan.
    command = [sys.executable, "-I", str(script)]
    target = safe_output(root, study, run_id)
    target.parent.mkdir(parents=True, exist_ok=True)
    # Sem exist_ok: nunca sobrescrever execução anterior, inclusive falha parcial.
    target.mkdir(exist_ok=False)
    record = {**descriptor, "timestamp_utc": datetime.now(timezone.utc).isoformat(),
              "command": ["python", "-I", spec["script"]],
              "source_sha256": hashlib.sha256(script.read_bytes()).hexdigest(),
              "input_sha256": hashlib.sha256(inp.read_bytes()).hexdigest(),
              "state": "ERROR", "returncode": None}
    try:
        env = {"PATH": os.environ.get("PATH", ""), "PYTHONDONTWRITEBYTECODE": "1",
               "PYTHONHASHSEED": "0"}
        result = subprocess.run(command, cwd=root, env=env, text=True,
                                capture_output=True, timeout=30, check=False)
        record["returncode"] = result.returncode
        (target / "stdout.txt").write_text(result.stdout, encoding="utf-8")
        (target / "stderr.txt").write_text(result.stderr, encoding="utf-8")
        record["state"] = "EXECUTED_N2_CHECK" if result.returncode == 0 else "FAILED"
    except (OSError, subprocess.TimeoutExpired) as exc:
        record["error"] = type(exc).__name__
    finally:
        (target / "report.json").write_text(
            json.dumps(record, ensure_ascii=False, indent=2, sort_keys=True) + "\n",
            encoding="utf-8")
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
        print(f"BLOQUEADO T07.3: {exc}", file=sys.stderr)
        return 2
    print(json.dumps(result, ensure_ascii=False, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
