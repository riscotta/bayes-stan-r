#!/usr/bin/env python3
"""T05.4: compilacao nativa CmdStan e sampling somente com fixtures sinteticas.

Nao importa dados institucionais, nem altera os programas Stan historicos.
Testes unitarios do protocolo sao distintos das execucoes nativas registradas.
"""
import argparse
import csv
import hashlib
import json
import math
import random
import shutil
import subprocess
import time
from datetime import datetime, timezone
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CMDSTAN_VERSION = "2.40.0"
CMDSTAN_RELEASE_SHA256 = "26d21c9737b35728e926f9b7a5d7362b38dc23e2e7d5e166c890e1fa74dac90c"
MODEL_PATHS = {
    "taxa": "scripts/pnadc_estudo_i/estudo_I/e07/stan/taxa_marginal.stan",
    "multinomial": "scripts/mega_sena_dezenas/modelo_multinomial_global.stan",
}
NUM_CHAINS = 2
NUM_WARMUP = 250
NUM_SAMPLES = 250
SEEDS = {"taxa": (20261111, 20261112), "multinomial": (20261113, 20261114)}
TRUTH_TAXA = 0.68
TRUTH_MULTINOMIAL = (0.18, 0.23, 0.27, 0.32)
TOL_TAXA = 0.05
TOL_MULTINOMIAL = 0.08


def require(ok, message):
    if not ok:
        raise ValueError(message)


def sha256(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def synthetic_cases():
    """Geracao deterministica: nao ha acesso a dados reais."""
    rate_rng = random.Random(5401)
    observation = round(100 * TRUTH_TAXA + rate_rng.gauss(0, 0.55), 8)
    data_rate = {"y": observation, "s": 0.55}
    multi_rng = random.Random(5402)
    n_slots = [750, 900]
    counts = []
    for total in n_slots:
        row = [0] * len(TRUTH_MULTINOMIAL)
        for _ in range(total):
            u = multi_rng.random()
            cumulative = 0.0
            for index, probability in enumerate(TRUTH_MULTINOMIAL):
                cumulative += probability
                if u < cumulative or index == len(row) - 1:
                    row[index] += 1
                    break
        counts.append(row)
    data_multi = {"D": 4, "G": 2, "y": counts, "n_slots": n_slots}
    return {
        "taxa": {"data": data_rate, "truth": TRUTH_TAXA, "tolerance": TOL_TAXA},
        "multinomial": {
            "data": data_multi,
            "truth": list(TRUTH_MULTINOMIAL),
            "tolerance": TOL_MULTINOMIAL,
        },
    }


def write_fixtures(folder):
    folder = Path(folder)
    folder.mkdir(parents=True, exist_ok=True)
    fixtures = synthetic_cases()
    paths = {}
    for name, spec in fixtures.items():
        path = folder / (name + ".json")
        path.write_text(json.dumps(spec["data"], sort_keys=True, indent=2) + "\n", encoding="utf-8")
        paths[name] = path
    return fixtures, paths


def read_draws(path, expected=NUM_SAMPLES):
    lines = [
        line for line in Path(path).read_text(encoding="utf-8").splitlines()
        if line.strip() and not line.startswith("#")
    ]
    require(lines, f"CSV sem cabecalho: {path}")
    reader = csv.DictReader(lines)
    require(reader.fieldnames is not None, f"CSV sem campos: {path}")
    require(len(reader.fieldnames) == len(set(reader.fieldnames)), f"Campos duplicados: {path}")
    rows = list(reader)
    require(len(rows) == expected, f"Esperados {expected} draws em {path}, obtidos {len(rows)}")
    require(all(None not in row for row in rows), f"CSV irregular: {path}")
    return rows


def field_set(rows, prefix):
    return {name for name in rows[0] if name.startswith(prefix)}


def value(row, field):
    require(field in row, f"Coluna ausente: {field}")
    number = float(row[field])
    require(math.isfinite(number), f"Valor nao finito em {field}")
    return number


def check_rate(rows, truth=TRUTH_TAXA, tolerance=TOL_TAXA):
    require(bool(rows), "Sem draws para taxa")
    require(field_set(rows, "p.") == set(), "Dimensao inesperada para p escalar")
    for col in ("lp__", "p", "theta", "y_rep"):
        require(col in rows[0], f"Taxa: coluna faltante {col}")
    draws = []
    for row in rows:
        p = value(row, "p")
        theta = value(row, "theta")
        value(row, "y_rep")
        require(0 <= p <= 1, "p fora de [0,1]")
        require(abs(theta - 100 * p) <= 0.005, "theta != 100*p")
        draws.append(p)
    posterior_mean = sum(draws) / len(draws)
    error = abs(posterior_mean - truth)
    require(error <= tolerance, f"Recuperacao taxa: erro {error:.6f} > {tolerance}")
    return {"posterior_mean": posterior_mean, "truth": truth, "abs_error": error, "tolerance": tolerance}


def check_multinomial(rows, data, truth=TRUTH_MULTINOMIAL, tolerance=TOL_MULTINOMIAL):
    require(bool(rows), "Sem draws multinomiais")
    d, g = data["D"], data["G"]
    require(d == len(truth), "Dimensao de verdade multinomial incorreta")
    require(len(data["y"]) == g and all(len(row) == d for row in data["y"]), "Dimensoes de dados y incorretas")
    require(len(data["n_slots"]) == g, "Dimensoes de n_slots incorretas")
    require(all(sum(row) == total for row, total in zip(data["y"], data["n_slots"])), "Contagens incompatíveis com n_slots")
    p_cols = {f"p.{k}" for k in range(1, d + 1)}
    alpha_cols = {f"alpha.{k}" for k in range(1, d + 1)}
    rep_cols = {f"y_rep_multinomial.{i}.{k}" for i in range(1, g + 1) for k in range(1, d + 1)}
    ll_cols = {f"log_lik.{i}" for i in range(1, g + 1)}
    for prefix, expected in (("p.", p_cols), ("alpha.", alpha_cols), ("y_rep_multinomial.", rep_cols), ("log_lik.", ll_cols)):
        require(field_set(rows, prefix) == expected, f"Dimensao/campos invalidos de {prefix}")
    require("lp__" in rows[0], "lp__ ausente")
    totals = [0.0] * d
    for row in rows:
        value(row, "lp__")
        for c in alpha_cols | ll_cols:
            value(row, c)
        probs = [value(row, f"p.{k}") for k in range(1, d + 1)]
        require(all(0 <= p <= 1 for p in probs), "Probabilidade fora de [0,1]")
        require(abs(sum(probs) - 1.0) <= 0.00002, "Simplex nao soma 1")
        for k, p in enumerate(probs):
            totals[k] += p
        for i, n in enumerate(data["n_slots"], 1):
            rep = [value(row, f"y_rep_multinomial.{i}.{k}") for k in range(1, d + 1)]
            require(all(v >= 0 and v.is_integer() for v in rep), "y_rep nao inteiro/negativo")
            require(sum(rep) == n, f"Dimensao/invariante de y_rep no grupo {i}")
    means = [s / len(rows) for s in totals]
    errors = [abs(a - b) for a, b in zip(means, truth)]
    require(max(errors) <= tolerance, f"Recuperacao multinomial: erro maximo {max(errors):.6f} > {tolerance}")
    return {"posterior_means": means, "truth": list(truth), "abs_errors": errors, "max_abs_error": max(errors), "tolerance": tolerance}


def checked_run(command, logfile, timeout=1200):
    logfile = Path(logfile)
    start = time.monotonic()
    with logfile.open("w", encoding="utf-8") as output:
        result = subprocess.run([str(x) for x in command], stdout=output, stderr=subprocess.STDOUT, timeout=timeout, check=False)
    elapsed = round(time.monotonic() - start, 3)
    require(result.returncode == 0, f"Comando falhou ({result.returncode}): {command[0]}; log={logfile}")
    return elapsed


def version_check(cmdstan):
    exe = cmdstan / "bin" / "stanc"
    require(exe.is_file(), "CmdStan sem bin/stanc; executar make build")
    result = subprocess.run([str(exe), "--version"], capture_output=True, text=True, check=True, timeout=30)
    version = (result.stdout + result.stderr).strip()
    require(CMDSTAN_VERSION in version, f"Versao CmdStan/stanc inesperada: {version}")
    return version


def native_dimension_negative(executable, correct_data, folder):
    invalid = json.loads(json.dumps(correct_data))
    invalid["y"] = [row[:-1] for row in invalid["y"]]
    bad_path = folder / "multinomial_dimensao_invalida.json"
    bad_path.write_text(json.dumps(invalid, sort_keys=True, indent=2) + "\n", encoding="utf-8")
    output = folder / "negative_dimensions.log"
    command = [
        str(executable), "sample", "num_warmup=1", "num_samples=1",
        "random", "seed=20261115",
        "data", f"file={bad_path}",
        "output", f"file={folder / 'negative_should_not_exist.csv'}", "refresh=0",
    ]
    with output.open("w", encoding="utf-8") as log:
        result = subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, timeout=30, check=False)
    require(result.returncode != 0, "Stan aceitou JSON com dimensoes intencionalmente incorretas")
    return {"rejected": True, "exit_code": result.returncode, "log": output.name}


def diagnostic_logs(cmdstan, csv_paths, folder, model_name):
    logs = {}
    for tool in ("stansummary", "diagnose"):
        executable = cmdstan / "bin" / tool
        require(executable.is_file(), f"CmdStan sem {tool}")
        path = folder / f"{model_name}_{tool}.txt"
        elapsed = checked_run([executable, *csv_paths], path, timeout=120)
        logs[tool] = {"file": path.name, "seconds": elapsed}
    return logs


def run_native(cmdstan, output_dir):
    output_dir = Path(output_dir).resolve()
    output_dir.mkdir(parents=True, exist_ok=True)
    report = {
        "task": "T05.4", "status": "FAIL",
        "timestamp_utc": datetime.now(timezone.utc).isoformat(),
        "cmdstan_version_required": CMDSTAN_VERSION,
        "release_asset_sha256_required": CMDSTAN_RELEASE_SHA256,
        "num_chains": NUM_CHAINS, "num_warmup_per_chain": NUM_WARMUP,
        "num_samples_per_chain": NUM_SAMPLES,
        "limitations": "Dados inteiramente sinteticos; tolerancias smoke e nao homologacao inferencial; diagnosticos descritivos, sem gate T05.5.",
        "models": {},
    }
    try:
        cmdstan = Path(cmdstan).resolve()
        report["stanc_version_output"] = version_check(cmdstan)
        fixtures, files = write_fixtures(output_dir / "fixtures")
        report["fixtures"] = {name: {"sha256": sha256(path), "data": fixtures[name]["data"]} for name, path in files.items()}
        for name, source_relpath in MODEL_PATHS.items():
            source = ROOT / source_relpath
            require(source.is_file(), f"Modelo ausente: {source_relpath}")
            work = output_dir / name
            work.mkdir(parents=True, exist_ok=True)
            model = work / source.name
            shutil.copyfile(source, model)
            exe = model.with_suffix("")
            info = {
                "source": source_relpath, "source_sha256": sha256(source),
                "data_file": files[name].relative_to(output_dir).as_posix(),
                "seed_per_chain": list(SEEDS[name]),
            }
            report["models"][name] = info
            info["compile_seconds"] = checked_run(
                ["make", "-C", cmdstan, "-j2", exe], work / "native_compile.log", timeout=1500
            )
            require(exe.is_file(), f"Compilacao nativa sem executavel: {name}")
            csvs = []
            times = []
            for chain, seed in enumerate(SEEDS[name], 1):
                output = work / f"chain_{chain}.csv"
                command = [
                    str(exe), "sample", f"num_warmup={NUM_WARMUP}",
                    f"num_samples={NUM_SAMPLES}", "adapt", "delta=0.9",
                    "random", f"seed={seed}", f"id={chain}",
                    "data", f"file={files[name]}",
                    "output", f"file={output}", "refresh=0",
                ]
                times.append(checked_run(command, work / f"chain_{chain}.log", timeout=420))
                csvs.append(output)
            draws = []
            for csv_path in csvs:
                draws.extend(read_draws(csv_path))
            info["sampling_seconds_by_chain"] = times
            info["n_draws_inspected"] = len(draws)
            if name == "taxa":
                info["recovery"] = check_rate(draws, truth=fixtures[name]["truth"], tolerance=fixtures[name]["tolerance"])
            else:
                info["recovery"] = check_multinomial(
                    draws, fixtures[name]["data"], truth=fixtures[name]["truth"], tolerance=fixtures[name]["tolerance"]
                )
                info["negative_dimension_test"] = native_dimension_negative(exe, fixtures[name]["data"], work)
            info["diagnostic_files"] = diagnostic_logs(cmdstan, csvs, work, name)
            info["result"] = "PASS"
            print(f"T05.4 {name}: compilacao C++ nativa, sampling e dimensoes/recuperacao PASS", flush=True)
        report["status"] = "PASS"
    except Exception as exc:
        report["error"] = f"{type(exc).__name__}: {exc}"
        raise
    finally:
        (output_dir / "report.json").write_text(json.dumps(report, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    return report


def main():
    parser = argparse.ArgumentParser(description="T05.4 smoke test Stan sintético")
    parser.add_argument("--cmdstan", type=Path, help="CmdStan 2.40.0 previamente compilado")
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--fixtures-only", action="store_true", help="Somente fixtures; NAO executa Stan")
    args = parser.parse_args()
    if args.fixtures_only:
        fixtures, paths = write_fixtures(args.output_dir)
        print(json.dumps({k: {"path": str(paths[k]), **v} for k, v in fixtures.items()}, indent=2, ensure_ascii=False))
        return
    parser.error("--cmdstan obrigatorio sem --fixtures-only") if args.cmdstan is None else None
    report = run_native(args.cmdstan, args.output_dir)
    print(f"T05.4: {report['status']} — report.json e logs em {args.output_dir}")


if __name__ == "__main__":
    main()
