#!/usr/bin/env python3
"""Gate T05.3: traduz modelos Stan selecionados a C++ sem executar MCMC."""
from __future__ import annotations

import argparse
from pathlib import Path
import subprocess
import tempfile

STANC_VERSION = "2.40.0"
MODELS = (
    "scripts/pnadc_estudo_i/estudo_I/e07/stan/contagem_marginal.stan",
    "scripts/caes_raca_f5/original/E08_MP_Entregaveis/02_codigo/stan/M1_skew_normal_E08_MP.stan",
    "scripts/exercises_dataset/modelo_ordinal_dificuldade_sem_equipamento.stan",
    "scripts/importacoes_world_bank/modelo_principal_hierarquico_student_t.stan",
    "scripts/sobrevivencia_cadastral_poa/modelo_M0_rw2_centrado_qr.stan",
)


def run_stanc(stanc: Path, source: Path, output: Path, root: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        (str(stanc), "--O0", f"--o={output}", str(source)),
        cwd=root, capture_output=True, text=True, timeout=60, check=False
    )


def validate(stanc: Path, root: Path) -> None:
    version = subprocess.run(
        (str(stanc), "--version"),
        cwd=root, capture_output=True, text=True, timeout=15, check=False
    )
    version_text = version.stdout + version.stderr
    if version.returncode != 0 or STANC_VERSION not in version_text:
        raise RuntimeError(f"stanc versão não fixada em {STANC_VERSION}: {version_text.strip()!r}")
    print(f"Compiler: {version_text.strip()}")

    with tempfile.TemporaryDirectory(prefix="stan-t05-3-") as tmp:
        working = Path(tmp)
        for index, relative in enumerate(MODELS, 1):
            model = root / relative
            if not model.is_file():
                raise RuntimeError(f"Modelo selecionado ausente: {relative}")
            output = working / f"modelo_{index}.hpp"
            outcome = run_stanc(stanc, model, output, root)
            if outcome.returncode != 0:
                raise RuntimeError(
                    f"Falha stanc {relative} (exit={outcome.returncode}):\n"
                    + outcome.stdout + outcome.stderr
                )
            if not output.is_file() or output.stat().st_size == 0:
                raise RuntimeError(f"stanc não gerou C++ em {relative}")
            print(f"PASS {index}/{len(MODELS)}: {relative}")

        invalid = working / "modelo_invalido.stan"
        invalid.write_text("data { real y }\n", encoding="utf-8")
        invalid_cpp = working / "invalido.hpp"
        negative = run_stanc(stanc, invalid, invalid_cpp, root)
        if negative.returncode == 0:
            raise RuntimeError("Falso negativo: stanc aceitou sintaxe Stan propositalmente inválida")
        print("PASS negativo: programa Stan inválido foi recusado")
    print("Gate T05.3: tradução Stan -> C++ aprovada; C++ nativo/MCMC NÃO executados.")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--stanc", required=True, type=Path, help="Executável stanc3 fixado")
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    try:
        validate(args.stanc.resolve(), root)
    except (RuntimeError, OSError, subprocess.TimeoutExpired) as error:
        parser.exit(1, f"ERROR T05.3: {error}\n")


if __name__ == "__main__":
    main()
