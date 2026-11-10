"""Testes estruturais do gate T05.3; não substituem stanc real na CI."""
import csv
import importlib.util
import os
from pathlib import Path
import tempfile
import unittest
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
RUNNER = ROOT / ".github/scripts/validar_stan_t05_3.py"
spec = importlib.util.spec_from_file_location("validar_stan_t05_3", RUNNER)
gate = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gate)

FAKE = """#!/usr/bin/env python3
import os
from pathlib import Path
import sys

args = sys.argv[1:]
if args == ["--version"]:
    print("stanc3 v" + os.environ.get("FAKE_STANC_VERSION", "2.40.0"))
    sys.exit(0)

source = Path(args[-1])
if "invalido" in source.name and os.environ.get("FAKE_ACCEPT_INVALID") != "1":
    sys.exit(1)
destination = Path(next(arg.partition("=")[2] for arg in args if arg.startswith("--o=")))
destination.write_text("// C++ traduzido no double de teste\\n", encoding="utf-8")
"""


class StanT053Tests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="test-t05-3-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.stanc = self.root / "fake_stanc"
        self.stanc.write_text(FAKE, encoding="utf-8")
        self.stanc.chmod(0o755)
        self.model = "scripts/estudo_exemplo/modelo.stan"
        path = self.root / self.model
        path.parent.mkdir(parents=True)
        path.write_text("parameters {real a;} model {a ~ normal(0, 1);}", encoding="utf-8")

    def test_manifesto_cobre_estudos_distintos_e_inventario(self):
        inventory = ROOT / "reports/t05_1/inventario_scripts.csv"
        with inventory.open(encoding="utf-8-sig", newline="") as handle:
            stan = [row for row in csv.DictReader(handle, delimiter=";")
                    if row["linguagem"] == "Stan"]
        all_paths = {row["caminho"] for row in stan}
        self.assertEqual(len(stan), 20)
        self.assertEqual(len({row["unidade"] for row in stan}), 9)
        self.assertEqual(len(gate.MODELS), 5)
        self.assertEqual(len(set(gate.MODELS)), 5)
        self.assertTrue(set(gate.MODELS).issubset(all_paths))
        for name in gate.MODELS:
            self.assertTrue((ROOT / name).is_file(), name)

    def test_fluxo_positivo_e_negativo_sinteticos(self):
        with mock.patch.object(gate, "MODELS", (self.model,)):
            gate.validate(self.stanc, self.root)

    def test_recusa_compilador_versao_diferente(self):
        with mock.patch.dict(os.environ, {"FAKE_STANC_VERSION": "2.39.0"}):
            with self.assertRaisesRegex(RuntimeError, "versão não fixada"):
                gate.validate(self.stanc, self.root)

    def test_recusa_modelo_ausente(self):
        with mock.patch.object(gate, "MODELS", ("scripts/ausente.stan",)):
            with self.assertRaisesRegex(RuntimeError, "ausente"):
                gate.validate(self.stanc, self.root)

    def test_falha_se_compilador_aceitar_sintaxe_invalida(self):
        with mock.patch.object(gate, "MODELS", (self.model,)):
            with mock.patch.dict(os.environ, {"FAKE_ACCEPT_INVALID": "1"}):
                with self.assertRaisesRegex(RuntimeError, "Falso negativo"):
                    gate.validate(self.stanc, self.root)


if __name__ == "__main__":
    unittest.main()
