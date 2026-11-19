#!/usr/bin/env python3
"""T07.6: casos sintéticos contra falso positivo no cotejo operacional."""
from __future__ import annotations

import copy
import hashlib
import importlib.util
import json
from pathlib import Path
import shutil
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "comparador_operacional_t07_6", ROOT / "tools/comparar_operacional_pnad_t07_6.py")
MOD = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MOD)


def sha(b: bytes) -> str:
    return hashlib.sha256(b).hexdigest()


class ComparadorOperacionalT076(unittest.TestCase):
    def setUp(self):
        self.t = tempfile.TemporaryDirectory()
        self.addCleanup(self.t.cleanup)
        self.root = Path(self.t.name)
        self.run_id = "t076-fixture"
        self.output = "outputs/pnadc_estudo_i/logs/t076-fixture"
        self.source = "scripts/pnadc_estudo_i/auditar_e10.py"
        self.input = "data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv"
        self.manifest = "reports/t07_2/manifesto_codigos_originais_sha256.csv"
        runner_path = self.root / "adapters/t07_3/runner.py"
        runner_path.parent.mkdir(parents=True)
        shutil.copyfile(ROOT / "adapters/t07_3/runner.py", runner_path)
        self.write(self.source, "print('SENTINELA PUBLICA SINTETICA')\n")
        self.write(self.input, "id;valor\n1;2\n")
        self.write(self.manifest, f"path,sha256\n{self.source},{sha((self.root / self.source).read_bytes())}\n")
        self.write(self.output + "/stdout.txt", "SENTINELA PUBLICA SINTETICA\n")
        self.write(self.output + "/stderr.txt", "")
        self.report = {
            "contract": "T07.3/v1", "study": "pnadc_estudo_i",
            "mode": "auditoria_rapida_N2", "script": self.source,
            "input": self.input, "output": self.output,
            "state": "EXECUTED_N2_CHECK", "returncode": 0,
            "source_sha256": sha((self.root / self.source).read_bytes()),
            "input_sha256": sha((self.root / self.input).read_bytes()),
            "manifest_sha256": sha((self.root / self.manifest).read_bytes()),
            "command": ["python", "-I", self.source],
        }
        self.save_report()

    def write(self, rel, content):
        path = self.root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content, encoding="utf-8")

    def save_report(self):
        self.write(self.output + "/report.json",
                   json.dumps(self.report, ensure_ascii=False) + "\n")

    def cmp(self):
        return MOD.comparar(self.root, self.run_id, python=sys.executable)

    def fail(self):
        with self.assertRaises((MOD.ComparacaoBloqueada, ValueError)):
            self.cmp()

    def test_01_stdout_stderr_identicos_em_fixture(self):
        result = self.cmp()
        self.assertEqual(result["status"], "IGUALDADE_OPERACIONAL_N2")
        self.assertIs(result["referencia_historica_independente"], False)
        self.assertIs(result["comparacao_historica"], False)
        self.assertIs(result["n3_n4_certificados"], False)

    def test_02_stdout_adulterado_rejeitado(self):
        self.write(self.output + "/stdout.txt", "SAIDA FALSA\n")
        self.fail()

    def test_03_stderr_adulterado_rejeitado(self):
        self.write(self.output + "/stderr.txt", "ERRO INEXISTENTE\n")
        self.fail()

    def test_04_report_json_false_pass_rejeitado(self):
        self.report["state"] = "FAILED"
        self.save_report()
        self.fail()

    def test_05_metadados_sha_origem_adulterado(self):
        self.report["source_sha256"] = "0"*64
        self.save_report()
        self.fail()

    def test_06_entrada_adulterada_rejeitada(self):
        self.write(self.input, "id;valor\n1;999\n")
        self.fail()

    def test_07_original_adulterado_rejeitado(self):
        self.write(self.source, "print('OUTRA SAIDA')\n")
        self.fail()

    def test_08_sem_saida_rejeitado(self):
        (self.root / self.output / "stdout.txt").unlink()
        self.fail()

    def test_09_id_traversal_rejeitado(self):
        with self.assertRaises(ValueError):
            MOD.comparar(self.root, "../outro", python=sys.executable)

    def test_10_symlink_de_stdout_rejeitado(self):
        original = self.root / self.output / "stdout.txt"
        tmp = original.with_suffix(".bak")
        original.rename(tmp)
        try:
            original.symlink_to(tmp)
        except (OSError, NotImplementedError):
            self.skipTest("symlink indisponível nesta plataforma")
        self.fail()

    def test_11_report_de_outro_id_rejeitado(self):
        self.report["output"] = "outputs/pnadc_estudo_i/logs/outro"
        self.save_report()
        self.fail()

    def test_12_nao_gera_nova_saida_no_sucesso(self):
        before = {p.name: p.read_bytes()
                  for p in (self.root / self.output).iterdir()}
        self.cmp()
        after = {p.name: p.read_bytes()
                 for p in (self.root / self.output).iterdir()}
        self.assertEqual(before, after)


if __name__ == "__main__":
    unittest.main()
