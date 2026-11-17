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
from unittest.mock import patch
import subprocess
import shutil

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

    def test_io_failure_after_output_creation_preserves_report(self):
        """Leitura do insumo falha após mkdir, antes de montar os SHA-256."""
        original_read = Path.read_bytes
        def injected_read(path):
            if path == self.inp:
                raise OSError("falha de I/O sintética após mkdir")
            return original_read(path)

        with patch.object(Path, "read_bytes", injected_read):
            self.assertEqual(self.call(action="executar"), 2)
        dest = self.root / "outputs/pnadc_estudo_i/logs/unit1"
        self.assertTrue(dest.is_dir())
        report = json.loads((dest / "report.json").read_text(encoding="utf-8"))
        self.assertEqual(report["state"], "ERROR")
        self.assertEqual(report["error"], "OSError")
        self.assertIsNone(report["returncode"])
        self.assertEqual(self.call(action="executar"), 2)  # ID consumido

    def test_timeout_after_output_creation_preserves_report(self):
        with patch.object(runner.subprocess, "run", side_effect=subprocess.TimeoutExpired("python", 30)):
            self.assertEqual(self.call(action="executar"), 2)
        report_path = self.root / "outputs/pnadc_estudo_i/logs/unit1/report.json"
        report = json.loads(report_path.read_text(encoding="utf-8"))
        self.assertEqual(report["state"], "ERROR")
        self.assertEqual(report["error"], "TimeoutExpired")

    def test_reject_symlink_on_input_ancestor(self):
        external = self.root / "external"
        destination = external / "raw/pnadc_estudo_i/resultados_auditados"
        destination.mkdir(parents=True)
        (destination / self.inp.name).write_bytes(self.inp.read_bytes())
        shutil.rmtree(self.root / "data")
        (self.root / "data").symlink_to(external, target_is_directory=True)
        self.assertEqual(self.call(action="executar"), 2)
        self.assertFalse((self.root / "outputs").exists())

    def test_report_storage_failure_is_explicit(self):
        """Nenhum software garante o registro quando o próprio disco falha."""
        original_write = runner.write_new_artifact
        def injected_write(target, filename, content):
            if filename == "report.json":
                raise OSError("armazenamento indisponível")
            return original_write(target, filename, content)
        with patch.object(runner, "write_new_artifact", injected_write):
            stderr = io.StringIO()
            with contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(stderr):
                rc = runner.main(["--estudo", "pnadc_estudo_i", "--modo", "auditoria_rapida_N2",
                                  "--acao", "executar", "--id", "unit1"], root=self.root)
        self.assertEqual(rc, 2)
        self.assertIn("Não foi possível preservar report.json", stderr.getvalue())
        dest = self.root / "outputs/pnadc_estudo_i/logs/unit1"
        self.assertTrue(dest.is_dir())
        self.assertFalse((dest / "report.json").exists())

    def test_failed_subprocess_preserves_diagnostic(self):
        self.script.write_text("raise SystemExit(17)\n", encoding="utf-8")
        self.write_manifest()
        self.assertEqual(self.call(action="executar"), 2)
        record = json.loads((self.root / "outputs/pnadc_estudo_i/logs/unit1/report.json").read_text())
        self.assertEqual(record["returncode"], 17)
        self.assertEqual(record["state"], "FAILED")


if __name__ == "__main__":
    unittest.main()
