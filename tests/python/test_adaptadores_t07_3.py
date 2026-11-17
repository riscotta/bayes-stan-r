"""T07.3: testes de contrato/hostilidade, sem inferência ou dados externos."""
import contextlib
import csv
import hashlib
import importlib.util
import io
import json
from pathlib import Path
import tempfile
import unittest

MODULE = Path(__file__).resolve().parents[2] / "adapters/t07_3/runner.py"
spec = importlib.util.spec_from_file_location("t07_3_runner", MODULE)
runner = importlib.util.module_from_spec(spec)
spec.loader.exec_module(runner)


class AdapterContract(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.script = self.root / "scripts/pnadc_estudo_i/auditar_e10.py"
        self.script.parent.mkdir(parents=True)
        # Fixture de harness, NÃO é dado nem saída científica.
        self.script.write_text("print('fixture de auditoria')\n", encoding="utf-8")
        self.manifest = self.root / "reports/t07_2/manifesto_codigos_originais_sha256.csv"
        self.manifest.parent.mkdir(parents=True)
        self.write_manifest()
        self.inp = self.root / runner.STUDIES["pnadc_estudo_i"]["input"]
        self.inp.parent.mkdir(parents=True)
        self.inp.write_text("public_fixture\n", encoding="utf-8")

    def write_manifest(self):
        with self.manifest.open("w", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, fieldnames=["path", "sha256", "bytes", "git_blob_sha1"])
            w.writeheader()
            b = self.script.read_bytes()
            w.writerow({"path": "scripts/pnadc_estudo_i/auditar_e10.py",
                        "sha256": hashlib.sha256(b).hexdigest(),
                        "bytes": str(len(b)), "git_blob_sha1": "0" * 40})

    def call(self, study="pnadc_estudo_i", action="planejar", run_id="unit1"):
        with contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
            return runner.main(["--estudo", study, "--modo", "auditoria_rapida_N2",
                                "--acao", action, "--id", run_id], root=self.root)

    def test_plan_no_writes_and_contract(self):
        self.assertEqual(self.call(), 0)
        self.assertEqual(list(self.root.glob("outputs/**/*")), [])
        d = runner.plan("pnadc_estudo_i", "unit1", self.root)
        self.assertEqual(d["output"], "outputs/pnadc_estudo_i/logs/unit1")
        self.assertTrue(d["allowed_execute"])

    def test_execute_creates_only_standard_logs_and_no_overwrite(self):
        before = self.script.read_bytes(), self.inp.read_bytes()
        self.assertEqual(self.call(action="executar"), 0)
        dest = self.root / "outputs/pnadc_estudo_i/logs/unit1"
        self.assertEqual({p.name for p in dest.iterdir()},
                         {"stdout.txt", "stderr.txt", "report.json"})
        result = json.loads((dest / "report.json").read_text(encoding="utf-8"))
        self.assertEqual(result["state"], "EXECUTED_N2_CHECK")
        self.assertEqual(self.call(action="executar"), 2)
        self.assertEqual(before, (self.script.read_bytes(), self.inp.read_bytes()))

    def test_reject_traversal_malformed_ids_and_output_symlink(self):
        for bad in ("../x", "X", "a/b", "a\\b", ".", "", "ä", "a" * 65):
            self.assertEqual(self.call(run_id=bad), 2)
        (self.root / "outputs").symlink_to(self.root / "scripts", target_is_directory=True)
        self.assertEqual(self.call(action="executar"), 2)

    def test_reject_corrupted_missing_original_or_input(self):
        self.script.write_text("print('adulterado')\n", encoding="utf-8")
        self.assertEqual(self.call(action="executar"), 2)
        self.script.write_text("print('fixture de auditoria')\n", encoding="utf-8")
        self.inp.unlink()
        self.assertEqual(self.call(action="executar"), 2)

    def test_reject_symlink_to_original(self):
        extra = self.root / "out.py"
        extra.write_bytes(self.script.read_bytes())
        self.script.unlink()
        self.script.symlink_to(extra)
        self.assertEqual(self.call(action="executar"), 2)

    def test_block_r_and_stan_routes(self):
        for study in ("caes_raca_f5", "mortalidade_sp_0_74"):
            self.assertEqual(self.call(study=study), 0)
            self.assertEqual(self.call(study=study, action="executar"), 2)
        self.assertFalse((self.root / "outputs").exists())

    def test_failed_subprocess_preserves_diagnostic(self):
        self.script.write_text("raise SystemExit(17)\n", encoding="utf-8")
        self.write_manifest()
        self.assertEqual(self.call(action="executar"), 2)
        record = json.loads((self.root / "outputs/pnadc_estudo_i/logs/unit1/report.json").read_text())
        self.assertEqual(record["returncode"], 17)
        self.assertEqual(record["state"], "FAILED")


if __name__ == "__main__":
    unittest.main()
