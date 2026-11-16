#!/usr/bin/env python3
"""T06.6: executar auditoria pública F5 e comparar com cálculo independente N2.

Nunca executa Stan nem lê E04 individual. Requer Rscript para o wrapper histórico.
"""
import csv
import hashlib
import json
import os
from datetime import datetime, timezone
from decimal import Decimal, InvalidOperation
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]
STUDY = "caes_raca_f5"
SOURCES = {
    "metricas_full_E08_MP.csv": "2bf24f4ef3ff43a85bdb3149781581e20d377dbc",
    "metricas_por_fold_auditadas.csv": "0d337c8ecf3b47fdfefe3b16735da48cf0a72016",
    "contrastes_por_fold_auditados.csv": "fcaf2791d34663c7a38134463a9c3108c153bb9f",
    "bootstrap_independente_2000.csv": "757d7a47482c493695677e156b836eac81cfe3a0",
}
INPUT = ROOT / "data" / "raw" / STUDY / "resultados_auditados"
GENERATED = ROOT / "outputs" / STUDY / "tables" / "comparacao_F5_E08_MP.csv"
FIELDS = [
    "familia", "caes", "rmse_M1", "rmse_M2", "delta_rmse", "ganho_pct",
    "ci95_inf", "ci95_sup", "cobertura90_M2", "abaixo_q05_M2", "acima_q95_M2",
]
FAMILIES = ("t5", "normal", "skew")
# Tolerância numérica pré-especificada: serialização decimal R vs. Decimal.
ATOL = Decimal("5e-13")


def read_csv(path):
    with path.open(encoding="utf-8", newline="") as stream:
        reader = csv.DictReader(stream)
        if not reader.fieldnames or len(reader.fieldnames) != len(set(reader.fieldnames)):
            raise ValueError(f"CSV com cabeçalho ausente/duplicado: {path}")
        rows = list(reader)
    if not rows or any(None in row for row in rows):
        raise ValueError(f"CSV vazio ou linha malformada: {path}")
    return rows


def number(value):
    try:
        result = Decimal(str(value))
    except (InvalidOperation, TypeError):
        raise ValueError(f"Valor não numérico: {value!r}") from None
    if not result.is_finite():
        raise ValueError(f"Valor não finito: {value!r}")
    return result


def unique(rows, keys, expected_n):
    if len(rows) != expected_n:
        raise ValueError(f"Contagem incorreta para {keys}: {len(rows)}, esperado {expected_n}")
    found = {}
    for row in rows:
        key = tuple(row[k] for k in keys)
        if key in found:
            raise ValueError(f"Chave duplicada: {key}")
        found[key] = row
    return found


def expected_rows(directory=INPUT):
    metrics = unique(read_csv(directory / "metricas_full_E08_MP.csv"),
                     ("familia", "modelo"), 7)
    folds = unique(read_csv(directory / "metricas_por_fold_auditadas.csv"),
                   ("familia", "modelo", "fold"), 30)
    contrasts = unique(read_csv(directory / "contrastes_por_fold_auditados.csv"),
                       ("familia", "fold"), 15)
    bootstrap = read_csv(directory / "bootstrap_independente_2000.csv")
    if set(metrics) != {("baseline", "M0")} | {(f, m) for f in FAMILIES for m in ("M1", "M2")}:
        raise ValueError("Modelos/famílias de referência divergentes")
    if len(bootstrap) < 3:
        raise ValueError("Bootstrap de referência incompleto")

    expected = []
    for family in FAMILIES:
        one, two = metrics[(family, "M1")], metrics[(family, "M2")]
        if number(one["n_caes"]) != 2084 or number(two["n_caes"]) != 2084:
            raise ValueError("Amostra original divergente")
        r1, r2 = number(one["rmse"]), number(two["rmse"])
        if not r1 > r2 > 0:
            raise ValueError("Direção global RMSE diverge")
        for model in ("M1", "M2"):
            for fold in range(1, 6):
                if (family, model, str(fold)) not in folds:
                    raise ValueError(f"Fold ausente: {family}/{model}/{fold}")
        per_fold = [contrasts[(family, str(k))] for k in range(1, 6)]
        if sum(int(number(row["n"])) for row in per_fold) != 2084:
            raise ValueError("Denominador por fold divergente")
        if sum(number(row["delta_rmse"]) > 0 for row in per_fold) != 4:
            raise ValueError("Direção dos folds divergente")
        if number(per_fold[0]["delta_rmse"]) >= 0:
            raise ValueError("Fold 1 deveria ser desfavorável")
        ci = [x for x in bootstrap if x["familia"] == family
              and x["contraste"] == "delta_RMSE_M1_menos_M2"]
        if len(ci) != 1:
            raise ValueError("IC bootstrap ausente/duplicado")
        lo, hi = number(ci[0]["LI95"]), number(ci[0]["LS95"])
        if not (0 < lo < hi):
            raise ValueError("Intervalo bootstrap inválido")
        cov = number(two["cobertura90"])
        below, above = number(two["abaixo_q05"]), number(two["acima_q95"])
        if abs(cov + below + above - 1) > Decimal("1e-12"):
            raise ValueError("Caudas/cobertura não reconciliadas")
        delta = r1 - r2
        gain = 100 * delta / r1
        if not Decimal("0.8") < gain < Decimal("1.5"):
            raise ValueError("Ganho fora do escopo histórico")
        expected.append({
            "familia": family, "caes": Decimal(2084), "rmse_M1": r1,
            "rmse_M2": r2, "delta_rmse": delta, "ganho_pct": gain,
            "ci95_inf": lo, "ci95_sup": hi, "cobertura90_M2": cov,
            "abaixo_q05_M2": below, "acima_q95_M2": above,
        })
    return expected


def compare_generated(expected, path):
    observed = read_csv(path)
    if list(observed[0]) != FIELDS:
        raise ValueError(f"Esquema de saída inesperado: {list(observed[0])}")
    if len(observed) != 3 or [r["familia"] for r in observed] != list(FAMILIES):
        raise ValueError("Famílias/ordem/quantidade da saída divergem")
    comparisons = 0
    for reference, result in zip(expected, observed):
        for field in FIELDS:
            if field == "familia":
                if reference[field] != result[field]:
                    raise ValueError("Família diferente")
                continue
            err = abs(number(result[field]) - reference[field])
            if err > ATOL:
                raise ValueError(f"Divergência {reference['familia']}/{field}: {err} > {ATOL}")
            comparisons += 1
    return comparisons


def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def gate_inputs():
    hashes = []
    for name, expected_blob in SOURCES.items():
        path = INPUT / name
        if not path.is_file():
            raise FileNotFoundError(f"Entrada pública ausente: {path}")
        found = subprocess.check_output(["git", "hash-object", str(path)],
                                        cwd=ROOT, text=True).strip()
        if found != expected_blob:
            raise ValueError(f"Identidade Git divergente para {name}: {found}")
        hashes.append({"path": str(path.relative_to(ROOT)), "git_blob_sha": found,
                       "sha256": sha256(path), "bytes": path.stat().st_size})
    return hashes


def capture_generated(path, evidence, record):
    """Preserva bytes e hash mesmo se o cotejo R/Python falhar depois."""
    if not path.is_file():
        return False
    content = path.read_bytes()
    evidence.mkdir(parents=True, exist_ok=True)
    (evidence / path.name).write_bytes(content)
    record["output"] = {
        "path": str(path.relative_to(ROOT)) if path.is_relative_to(ROOT) else path.name,
        "sha256": hashlib.sha256(content).hexdigest(),
        "bytes": len(content),
        "captured_before_numeric_gate": True,
    }
    return True


def execute():
    run_id = os.environ.get("GITHUB_RUN_ID", datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ"))
    attempt = os.environ.get("GITHUB_RUN_ATTEMPT", "local")
    evidence = ROOT / "outputs" / "_reexecucoes_t06_4" / STUDY / f"t06_6_{run_id}_{attempt}"
    evidence.mkdir(parents=True, exist_ok=False)
    record = {
        "task": "T06.6", "study": STUDY, "mode": "auditoria_rapida_N2",
        "date_utc": datetime.now(timezone.utc).isoformat(),
        "source": "https://doi.org/10.5061/dryad.g4f4qrfr0",
        "source_vintage": "Dryad 2022-05-05; agregados derivados versionados no git",
        "source_license": "Dryad CC0 1.0; NÃO estende licença a insumos terceiros",
        "base_main": "77ec0e1cf4ef4bf3feb9721ede8221846c6b0bf6",
        "checkout_sha": os.environ.get("GITHUB_SHA", ""),
        "pr_head_sha": os.environ.get("PR_HEAD_SHA", ""),
        "seed": "NAO_APLICAVEL_sem_simulacao",
        "tolerance_abs": str(ATOL), "tolerance_rel": "0",
        "command": ["Rscript", "--vanilla",
                    "scripts/caes_raca_f5/caes_raca_f5_cmdstanr.R", "--run_stan=0"],
        "status": "FALHOU", "scientific_level": "N2_apenas_tabelas_agregadas",
        "limitations": ["sem dados individuais E04", "sem Stan/MCMC", "sem N3/N4",
                        "sem validacao externa", "sem homologacao geral P0/#14",
                        "QA independente pendente"],
    }
    try:
        if GENERATED.exists():
            raise ValueError("Saída preexistente: recusa sobrescrever")
        record["inputs"] = gate_inputs()
        expected = expected_rows()
        completed = subprocess.run(record["command"], cwd=ROOT, text=True,
                                   capture_output=True, check=False)
        (evidence / "r_stdout.txt").write_text(completed.stdout, encoding="utf-8")
        (evidence / "r_stderr.txt").write_text(completed.stderr, encoding="utf-8")
        record["r_return_code"] = completed.returncode
        # A tabela (mesmo parcial) é evidência indispensável quando o cotejo reprova.
        captured = capture_generated(GENERATED, evidence, record)
        if completed.returncode != 0:
            raise RuntimeError("Wrapper R retornou erro; confira r_stderr.txt e a tabela capturada")
        if not captured:
            raise RuntimeError("Wrapper R saiu com 0 mas não produziu a tabela esperada")
        count = compare_generated(expected, GENERATED)
        record["comparison_count"] = count
        record["r_version"] = subprocess.check_output(
            ["Rscript", "--version"], cwd=ROOT, stderr=subprocess.STDOUT, text=True).strip()
        record["status"] = "COMPARACAO_N2_CONFORME_PENDENTE_QA"
        print(f"T06.6: {count} verificações numéricas; tolerância {ATOL}; saída {record['output']['sha256']}")
    except Exception as exc:
        record["error"] = f"{type(exc).__name__}: {exc}"
        print("T06.6: " + record["error"], file=sys.stderr)
    finally:
        (evidence / "registro_execucao.json").write_text(
            json.dumps(record, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
        print(f"Registro T06.6: {evidence.relative_to(ROOT)}")
    return 0 if record["status"] == "COMPARACAO_N2_CONFORME_PENDENTE_QA" else 1


if __name__ == "__main__":
    raise SystemExit(execute())
