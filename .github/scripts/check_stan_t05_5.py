#!/usr/bin/env python3
"""T05.5: checks independentes de prior e checks CmdStan posterior predictive/HMC.

Nao altera modelos Stan nem importa dados institucionais. Stdlib Python 3.12.8.
"""
import argparse
import csv
import importlib.util
import json
import math
import random
import shutil
import statistics
from datetime import datetime, timezone
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / ".github/scripts/smoke_stan_t05_4.py"
spec = importlib.util.spec_from_file_location("smoke_t054", SRC)
smoke = importlib.util.module_from_spec(spec)
spec.loader.exec_module(smoke)

PRIOR_N = 8192
CHAINS = 4
WARMUP = 500
DRAWS = 600
MAX_DEPTH = 10
SEEDS = {
    "taxa": (20261121, 20261122, 20261123, 20261124),
    "multinomial": (20261125, 20261126, 20261127, 20261128),
}
RHAT_LIMIT = 1.01
ESS_LIMIT = 400.0
BFMI_LIMIT = 0.30
RATE_PPC_TOL = 0.15
MULTI_PPC_TOL = 0.025


def require(condition, message):
    if not condition:
        raise ValueError(message)


def sample_category(rng, p):
    u = rng.random()
    s = 0.0
    for i, v in enumerate(p):
        s += v
        if u < s or i == len(p) - 1:
            return i
    raise AssertionError("Categoria inacessivel")


def prior_checks(n=PRIOR_N, enforce=True):
    require(isinstance(n, int) and n > 1, "Numero prior invalido")
    rate_rng = random.Random(55101)
    rp = []
    residual = []
    for _ in range(n):
        p = rate_rng.random()  # Beta(1,1) = Uniform(0,1).
        y = 100 * p + rate_rng.gauss(0, 0.55)
        rp.append(p)
        residual.append(y - 100 * p)
        require(0 <= p <= 1 and math.isfinite(y), "Prior taxa invalido")
    rate = {"mean_p": statistics.fmean(rp),
            "mean_y": statistics.fmean(100 * p + r for p, r in zip(rp, residual)),
            "mean_residual": statistics.fmean(residual)}
    multi_rng = random.Random(55102)
    means = [0.0] * 4
    for _ in range(n):
        z = [multi_rng.gauss(0, 0.35) for _ in range(4)]
        alpha = [v - statistics.fmean(z) for v in z]
        probs_unnormalized = [math.exp(v) for v in alpha]
        denom = sum(probs_unnormalized)
        p = [v / denom for v in probs_unnormalized]
        require(abs(sum(alpha)) <= 1e-10 and abs(sum(p) - 1) <= 1e-12,
                "Prior multinomial sem soma-zero/simplex")
        require(all(0 <= x <= 1 and math.isfinite(x) for x in p), "Prior p invalido")
        for k, v in enumerate(p):
            means[k] += v
        for total in (750, 900):
            counts = [0, 0, 0, 0]
            for _ in range(total):
                counts[sample_category(multi_rng, p)] += 1
            require(sum(counts) == total and min(counts) >= 0, "Prior predictive invalido")
    means = [v / n for v in means]
    multi = {"mean_p": means, "theoretical_mean_p": [0.25] * 4,
             "marginal_alpha_variance": 0.35 ** 2 * (1 - 1 / 4)}
    if enforce:
        require(abs(rate["mean_p"] - 0.5) <= 0.03, "Prior taxa: media p")
        require(abs(rate["mean_residual"]) <= 0.05, "Prior taxa: ruido preditivo")
        require(max(abs(v - .25) for v in means) <= 0.03, "Prior multinomial: simetria")
    return {"draws": n, "seed_rate": 55101, "seed_multi": 55102,
            "rate": rate, "multinomial": multi,
            "method": "simulacao matematica independente; nao e sampling prior nativo do Stan"}


def checked_float(row, key):
    require(key in row and row[key] is not None, "Coluna ausente: " + key)
    try:
        value = float(row[key])
    except (ValueError, TypeError) as exc:
        raise ValueError("Valor nao numerico em " + key) from exc
    require(math.isfinite(value), "Valor nao finito em " + key)
    return value


def check_chain(rows):
    require(len(rows) == DRAWS, f"CSV truncado: esperado {DRAWS}, recebido {len(rows)}")
    need = {"energy__", "divergent__", "treedepth__"}
    require(need <= set(rows[0]), "Colunas HMC obrigatorias ausentes")
    for row in rows:
        for key in row:
            checked_float(row, key)
    energy = [checked_float(r, "energy__") for r in rows]
    v = statistics.pvariance(energy)
    require(v > 0, "E-BFMI invalido: energia constante")
    bfmi = (sum((b - a) ** 2 for a, b in zip(energy, energy[1:]))
            / (len(energy) - 1) / v)
    div = sum(checked_float(r, "divergent__") for r in rows)
    saturated = sum(checked_float(r, "treedepth__") >= MAX_DEPTH for r in rows)
    require(div == 0, f"Divergencias HMC: {div}")
    require(saturated == 0, f"Treedepth saturado: {saturated}")
    require(bfmi >= BFMI_LIMIT, f"E-BFMI {bfmi:.4f} menor que {BFMI_LIMIT}")
    return {"n_draws": len(rows), "divergences": div,
            "saturated_treedepth": saturated, "ebfmi": bfmi}


def summary_diagnostics(path, model):
    with Path(path).open(encoding="utf-8", newline="") as f:
        lines = [ln for ln in f if ln.strip() and not ln.lstrip().startswith("#")]
    reader = csv.DictReader(lines)
    require(reader.fieldnames, "stansummary CSV sem cabecalho")
    fields = {x.strip().lower(): x for x in reader.fieldnames}
    require({"ess_bulk", "ess_tail", "r_hat"} <= set(fields),
            "stansummary sem diagnosticos ESS_bulk/ESS_tail/R_hat")
    namefield = reader.fieldnames[0]
    records = {row[namefield].strip().replace(".", "[", 0): row for row in reader}
    needed = (["p"] if model == "taxa" else
              [f"alpha[{k}]" for k in range(1, 5)] +
              [f"p[{k}]" for k in range(1, 5)])
    details = {}
    for var in needed:
        # stansummary usa indices entre colchetes; fallback para nomes pontuados.
        row = records.get(var) or records.get(var.replace("[", ".").replace("]", ""))
        require(row is not None, "Parametro ausente no stansummary: " + var)
        vals = {k: checked_float(row, fields[k]) for k in ("ess_bulk", "ess_tail", "r_hat")}
        require(vals["r_hat"] < RHAT_LIMIT, f"R-hat >= {RHAT_LIMIT} em {var}")
        require(vals["ess_bulk"] >= ESS_LIMIT, f"ESS_bulk baixo em {var}")
        require(vals["ess_tail"] >= ESS_LIMIT, f"ESS_tail baixo em {var}")
        details[var] = vals
    return details


def posterior_predictive(rows, model, data):
    require(bool(rows), "PPC sem draws")
    if model == "taxa":
        p = statistics.fmean(checked_float(r, "p") for r in rows)
        rep = [checked_float(r, "y_rep") for r in rows]
        theta = [checked_float(r, "theta") for r in rows]
        error = abs(statistics.fmean(rep) - 100 * p)
        require(error <= RATE_PPC_TOL, f"PPC taxa: media condicional desajustada {error}")
        y = data["y"]
        pvalue = sum(abs(a - b) >= abs(y - b) for a, b in zip(rep, theta)) / len(rows)
        return {"conditional_mean_error": error, "tolerance": RATE_PPC_TOL,
                "descriptive_discrepancy_tail_fraction": pvalue,
                "observed_y_synthetic": y, "not_a_calibration_test": True}
    d, g = data["D"], data["G"]
    errors = {}
    pvalues = []
    for i in range(1, g + 1):
        n = data["n_slots"][i - 1]
        require(sum(data["y"][i-1]) == n, "Fixture y inconsistente")
        for k in range(1, d + 1):
            prob = [checked_float(r, f"p.{k}") for r in rows]
            reps = [checked_float(r, f"y_rep_multinomial.{i}.{k}") for r in rows]
            require(all(v.is_integer() and 0 <= v <= n for v in reps), "PPC rep invalido")
            err = abs(statistics.fmean(reps) / n - statistics.fmean(prob))
            require(err <= MULTI_PPC_TOL, f"PPC multinomial discrepante grupo {i} cat {k}: {err}")
            errors[f"{i}.{k}"] = err
            observed = data["y"][i-1][k-1]
            pvalues.append(sum(abs(x - n*p) >= abs(observed - n*p)
                               for x, p in zip(reps, prob)) / len(rows))
    return {"conditional_mean_errors": errors, "tolerance": MULTI_PPC_TOL,
            "descriptive_discrepancy_tail_fraction_by_cell": pvalues,
            "not_a_calibration_test": True}


def native_checks(cmdstan, output):
    require(smoke.version_check(cmdstan), "CmdStan 2.40.0 necessario")
    fixtures, files = smoke.write_fixtures(output / "fixtures")
    report = {}
    for model, path in smoke.MODEL_PATHS.items():
        work = output / model
        work.mkdir(exist_ok=True)
        source = ROOT / path
        require(source.exists(), "Stan original ausente: " + path)
        executable = work / source.stem
        shutil.copy2(source, executable.with_suffix(".stan"))
        elapsed_compile = smoke.checked_run(["make", "-C", cmdstan, "-j2", executable],
                                            work / "compile.log", timeout=1800)
        require(executable.is_file(), "Compilacao nao criou exe: " + model)
        csvs, chains = [], []
        for i, seed in enumerate(SEEDS[model], 1):
            csvpath = work / f"chain_{i}.csv"
            command = [executable, "sample", f"num_warmup={WARMUP}", f"num_samples={DRAWS}",
                       "adapt", "delta=0.9", "random", f"seed={seed}", f"id={i}",
                       "data", f"file={files[model]}",
                       "output", f"file={csvpath}", "refresh=0"]
            smoke.checked_run(command, work / f"chain_{i}.log", timeout=480)
            csvs.append(csvpath)
            chains.append(check_chain(smoke.read_draws(csvpath, expected=DRAWS)))
        summary = work / "stansummary.csv"
        smoke.checked_run([cmdstan / "bin/stansummary", "--csv_filename", summary,
                           "--sig_figs", "8", *csvs], work / "stansummary.log", timeout=180)
        smoke.checked_run([cmdstan / "bin/diagnose", *csvs],
                          work / "diagnose.txt", timeout=180)
        diag = summary_diagnostics(summary, model)
        allrows = [r for csvpath in csvs for r in smoke.read_draws(csvpath, expected=DRAWS)]
        # T05.4 valida dimensoes e recuperacao; tolerancias originais permanecem.
        recovery = (smoke.check_rate(allrows) if model == "taxa" else
                    smoke.check_multinomial(allrows, fixtures[model]["data"]))
        prediction = posterior_predictive(allrows, model, fixtures[model]["data"])
        report[model] = {
            "source": path, "source_sha256": smoke.sha256(source),
            "fixture_sha256": smoke.sha256(files[model]),
            "compile_seconds": elapsed_compile, "seeds": list(SEEDS[model]),
            "n_draws": len(allrows), "chains": chains,
            "rhat_ess_model_parameters": diag, "recovery_T05_4": recovery,
            "posterior_predictive": prediction,
            "generated_quantities_rhat": "descritivo no stansummary; nao usado como gate de parametros",
        }
    return report


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--cmdstan", type=Path)
    ap.add_argument("--output-dir", type=Path, required=True)
    ap.add_argument("--prior-only", action="store_true")
    args = ap.parse_args()
    output = args.output_dir.resolve()
    output.mkdir(parents=True, exist_ok=True)
    report = {"task": "T05.5", "status": "FAIL",
              "timestamp_utc": datetime.now(timezone.utc).isoformat(),
              "base_main": "34c60eb7ac404d6fb5463cc60bcf771e7669e0ec",
              "limits": {"rhat_strictly_below": RHAT_LIMIT,
                         "ess_bulk_tail_at_least": ESS_LIMIT,
                         "ebfmi_at_least": BFMI_LIMIT, "divergences": 0,
                         "saturated_treedepth": 0, "max_depth": MAX_DEPTH},
              "scope": "dois modelos; somente fixtures sinteticas; nao valida modelos em geral"}
    try:
        report["prior_independent"] = prior_checks()
        if not args.prior_only:
            require(args.cmdstan is not None, "--cmdstan obrigatorio")
            report["native"] = native_checks(args.cmdstan.resolve(), output)
        else:
            report["native"] = "NAO EXECUTADO (--prior-only); nao e gate cientifico T05.5"
        report["status"] = "PASS-PRIOR-ONLY" if args.prior_only else "PASS"
    except Exception as exc:
        report["error"] = f"{type(exc).__name__}: {exc}"
        raise
    finally:
        (output / "report.json").write_text(json.dumps(report, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    print("T05.5: " + report["status"])


if __name__ == "__main__":
    main()
