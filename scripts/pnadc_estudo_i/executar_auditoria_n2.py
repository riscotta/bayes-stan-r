#!/usr/bin/env python3
"""Gate CI para a auditoria pública N2, sem alterar o auditor E10 histórico."""
from __future__ import annotations

import runpy
import sys
from pathlib import Path


def main() -> int:
    # auditar_e10.py valida com assert: -O/PYTHONOPTIMIZE os desativaria.
    if sys.flags.optimize != 0 or not __debug__:
        print(
            "N2 FALHOU: Python otimizado (-O/-OO/PYTHONOPTIMIZE) "
            "desativa os assert do auditor histórico; execução recusada.",
            file=sys.stderr,
        )
        return 2

    try:
        runpy.run_path(str(Path(__file__).with_name("auditar_e10.py")), run_name="__main__")
    except Exception as exc:
        print(f"N2 FALHOU: {type(exc).__name__}: {exc}", file=sys.stderr)
        return 1

    print(
        "ESCOPO N2: somente auditoria da projeção descritiva E09 pública "
        "(SHA-256, 13 indicadores, 26 contrastes, janelas e deltas). "
        "Não reexecuta E04–E08, SIDRA, Stan/MCMC, probabilidades, N3 ou N4; "
        "não homologa dados institucionais ou licenças."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
