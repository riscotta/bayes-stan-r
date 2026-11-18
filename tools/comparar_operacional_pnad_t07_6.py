#!/usr/bin/env python3
"""T07.6: confronto operacional N2, sem baseline histórica independente.

Compara stdout/stderr do adaptador previamente executado com uma chamada
direta ao MESMO script público. Não gera evidência de N3/N4 ou equivalência
com saídas históricas externas.
"""
from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
STUDY = "pnadc_estudo_i"
FILES = ("stdout.txt", "stderr.txt", "report.json")


class ComparacaoBloqueada(ValueError):
    pass


def sha(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def carregar_runner(root: Path):
    spec = importlib.util.spec_from_file_location(
        "runner_t07_6_comparacao", root / "adapters/t07_3/runner.py")
    if spec is None or spec.loader is None:
        raise ComparacaoBloqueada("Runner canônico indisponível")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def comparar(root: Path, run_id: str, python: str = sys.executable) -> dict:
    # Sem criação de arquivos pela ferramenta; o adaptador executado antes
    # é a única origem dos artefatos canônicos.
    root = Path(root).absolute()
    if not root.is_dir() or root.is_symlink():
        raise ComparacaoBloqueada("Raiz ausente ou symlink")
    runner = carregar_runner(root)
    source_rel = runner.STUDIES[STUDY]["script"]
    input_rel = runner.STUDIES[STUDY]["input"]
    source, manifest_sha, _ = runner.checked_source_with_manifest(root, source_rel)
    source_sha = sha(source.read_bytes())
    input_sha = runner.hash_checked_input(root, input_rel)
    target = runner.safe_output(root, STUDY, run_id)
    rel_output = target.relative_to(root).as_posix()
    artifacts = {}
    for name in FILES:
        path = runner.checked_regular(root, f"{rel_output}/{name}", name)
        artifacts[name] = path.read_bytes()
    try:
        report = json.loads(artifacts["report.json"].decode("utf-8"))
    except (UnicodeError, json.JSONDecodeError) as exc:
        raise ComparacaoBloqueada("report.json inválido") from exc
    required = {
        "contract": "T07.3/v1",
        "study": STUDY, "mode": "auditoria_rapida_N2",
        "script": source_rel, "input": input_rel, "output": rel_output,
        "state": "EXECUTED_N2_CHECK", "returncode": 0,
        "source_sha256": source_sha, "input_sha256": input_sha,
        "manifest_sha256": manifest_sha,
        "command": ["python", "-I", source_rel],
    }
    for key, expected in required.items():
        if report.get(key) != expected:
            raise ComparacaoBloqueada(f"Metadados divergentes no report.json: {key}")

    env = {
        "PATH": os.environ.get("PATH", ""),
        "PYTHONDONTWRITEBYTECODE": "1",
        "PYTHONHASHSEED": "0",
    }
    try:
        direct = subprocess.run(
            [python, "-I", str(source)], cwd=root, env=env, text=True,
            capture_output=True, timeout=30, check=False)
    except (OSError, subprocess.TimeoutExpired) as exc:
        raise ComparacaoBloqueada("Execução direta de controle falhou") from exc
    if direct.returncode != 0:
        raise ComparacaoBloqueada(
            f"Código público recusou auditoria direta: rc={direct.returncode}")

    # Confronto byte a byte somente na MESMA versão/código/entrada/ambiente.
    # Não equivale a comparação contra resultado histórico autenticado.
    if direct.stdout.encode("utf-8") != artifacts["stdout.txt"]:
        raise ComparacaoBloqueada("DIVERGENCIA_OPERACIONAL_STDOUT")
    if direct.stderr.encode("utf-8") != artifacts["stderr.txt"]:
        raise ComparacaoBloqueada("DIVERGENCIA_OPERACIONAL_STDERR")
    if (runner.hash_checked_input(root, input_rel) != input_sha
            or sha(runner.checked_regular(root, source_rel, "Código").read_bytes()) != source_sha
            or sha(runner.checked_regular(root, runner.MANIFEST_REL, "Manifesto").read_bytes()) != manifest_sha):
        raise ComparacaoBloqueada("Arquivo original/entrada/manifesto mudou durante controle")
    for name in FILES:
        if runner.checked_regular(root, f"{rel_output}/{name}", name).read_bytes() != artifacts[name]:
            raise ComparacaoBloqueada(f"Artefato do adaptador modificado: {name}")
    return {
        "status": "IGUALDADE_OPERACIONAL_N2",
        "estudo": STUDY,
        "head_depende_do_checkout": True,
        "stdout_sha256": sha(artifacts["stdout.txt"]),
        "stderr_sha256": sha(artifacts["stderr.txt"]),
        "codigo_sha256": source_sha,
        "entrada_sha256": input_sha,
        "referencia_historica_independente": False,
        "comparacao_historica": False,
        "mcmc_reexecutado": False,
        "n3_n4_certificados": False,
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--id", required=True)
    args = parser.parse_args()
    try:
        result = comparar(ROOT, args.id)
    except (ComparacaoBloqueada, ValueError, OSError) as exc:
        print(f"T07.6 BLOQUEADO: {exc}", file=sys.stderr)
        return 2
    print(json.dumps(result, ensure_ascii=False, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
