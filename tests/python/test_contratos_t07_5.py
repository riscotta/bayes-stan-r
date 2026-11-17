"""T07.5: contratos CLI e smoke end-to-end em checkout inteiramente sintético.

Nenhuma entrada institucional, original histórico ou modelo R/Stan é executado.
"""
from __future__ import annotations

import csv
from datetime import datetime
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


RUNNER = Path(__file__).resolve().parents[2] / "adapters/t07_3/runner.py"
SPEC = importlib.util.spec_from_file_location("runner_t075_contract", RUNNER)
runner = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(runner)


class ContractAndSmokeT075(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory(prefix="t075-synthetic-")
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.cli = self.root / "adapters/t07_3/runner.py"
        self.cli.parent.mkdir(parents=True)
        self.cli.write_bytes(RUNNER.read_bytes())
        spec = runner.STUDIES["pnadc_estudo_i"]
        self.script = self.root / spec["script"]
        self.script.parent.mkdir(parents=True)
        self.set_script("print('SMOKE_T075_FIXTURE')\n")
        self.input = self.root / spec["input"]
        self.input.parent.mkdir(parents=True)
        self.input.write_text("fixture,valor\nsynthetic,1\n", encoding="utf-8")
        self.manifest = self.root / runner.MANIFEST_REL
        self.manifest.parent.mkdir(parents=True)
        self.refresh_manifest()
        self.out = self.root / "outputs/pnadc_estudo_i/logs/smoke-t075"

    def set_script(self, content):
        self.script.write_text(content, encoding="utf-8")

    def refresh_manifest(self):
        raw = self.script.read_bytes()
        with self.manifest.open("w", encoding="utf-8", newline="") as handle:
            writer = csv.DictWriter(handle, fieldnames=["path", "sha256", "bytes", "git_blob_sha1"])
            writer.writeheader()
            writer.writerow({"path": runner.STUDIES["pnadc_estudo_i"]["script"],
                             "sha256": hashlib.sha256(raw).hexdigest(),
                             "bytes": str(len(raw)), "git_blob_sha1": "0" * 40})

    def invoke(self, study="pnadc_estudo_i", action="planejar", run_id="smoke-t075", mode="auditoria_rapida_N2"):
        env = {**os.environ, "PYTHONDONTWRITEBYTECODE": "1"}
        return subprocess.run([sys.executable, "-I", str(self.cli), "--estudo", study,
                               "--modo", mode, "--acao", action, "--id", run_id],
                              cwd=self.root, env=env, text=True, capture_output=True,
                              timeout=12, check=False)

    def read_report(self):
        return json.loads((self.out / "report.json").read_text(encoding="utf-8"))

    def test_plan_matrix_public_contract_without_side_effects(self):
        for name, allowed in (("pnadc_estudo_i", True),
                              ("mortalidade_sp_0_74", False),
                              ("caes_raca_f5", False)):
            with self.subTest(name=name):
                p = self.invoke(study=name)
                self.assertEqual(p.returncode, 0, p.stderr)
                record = json.loads(p.stdout)
                self.assertEqual(record["contract"], "T07.3/v1")
                self.assertEqual(record["study"], name)
                self.assertEqual(record["mode"], "auditoria_rapida_N2")
                self.assertEqual(record["script"], runner.STUDIES[name]["script"])
                self.assertEqual(record["input"], runner.STUDIES[name]["input"])
                self.assertEqual(record["output"], f"outputs/{name}/logs/smoke-t075")
                self.assertIs(record["allowed_execute"], allowed)
                self.assertEqual(record["execution"], "python -I <script>" if allowed else "BLOCKED")
                self.assertIn("N2", record["scientific_level"])
        self.assertFalse((self.root / "outputs").exists())

    def test_help_and_required_arguments(self):
        h = subprocess.run([sys.executable, "-I", str(self.cli), "--help"],
                           cwd=self.root, text=True, capture_output=True, timeout=12)
        self.assertEqual(h.returncode, 0)
        for flag in ("--estudo", "--modo", "--acao", "--id"):
            self.assertIn(flag, h.stdout)
        missing = subprocess.run([sys.executable, "-I", str(self.cli)],
                                 cwd=self.root, text=True, capture_output=True, timeout=12)
        self.assertEqual(missing.returncode, 2)
        self.assertFalse((self.root / "outputs").exists())

    def test_mode_and_study_are_closed_enumerations(self):
        for kwargs in ({"mode": "sampling"}, {"study": "outro"}):
            with self.subTest(kwargs=kwargs):
                self.assertEqual(self.invoke(**kwargs).returncode, 2)
        self.assertFalse((self.root / "outputs").exists())

    def test_id_contract_no_traversal_or_noncanonical_forms(self):
        for bad in ("../x", "a/b", "a\\b", "", ".", "UPPER", "espaço", "a" * 65):
            with self.subTest(id=bad):
                self.assertEqual(self.invoke(run_id=bad).returncode, 2)
        for good in ("a", "0", "x_y-z", "a" * 64):
            with self.subTest(id=good):
                self.assertEqual(self.invoke(run_id=good).returncode, 0)
        self.assertFalse((self.root / "outputs").exists())

    def test_output_types_and_paths_are_closed_contracts(self):
        for kind in sorted(runner.OUTPUT_KINDS):
            p = runner.canonical_output(self.root, "pnadc_estudo_i", kind, "smoke-t075")
            self.assertEqual(p.relative_to(self.root).as_posix(),
                             f"outputs/pnadc_estudo_i/{kind}/smoke-t075")
        with self.assertRaises(runner.GateError):
            runner.canonical_output(self.root, "pnadc_estudo_i", "../logs", "id")
        with self.assertRaises(runner.GateError):
            runner.canonical_output(self.root, "unknown", "logs", "id")
        self.assertFalse((self.root / "outputs").exists())

    def test_smoke_real_cli_success_and_artifact_schema(self):
        before = (self.script.read_bytes(), self.input.read_bytes(), self.manifest.read_bytes())
        p = self.invoke(action="executar")
        self.assertEqual(p.returncode, 0, p.stderr)
        value = json.loads(p.stdout)
        report = self.read_report()
        self.assertEqual(value, report)
        self.assertEqual(report["state"], "EXECUTED_N2_CHECK")
        self.assertEqual(report["contract"], "T07.3/v1")
        self.assertEqual(report["returncode"], 0)
        self.assertEqual(report["command"], ["python", "-I", runner.STUDIES["pnadc_estudo_i"]["script"]])
        self.assertEqual(report["source_sha256"], hashlib.sha256(before[0]).hexdigest())
        self.assertEqual(report["input_sha256"], hashlib.sha256(before[1]).hexdigest())
        self.assertEqual(report["manifest_sha256"], hashlib.sha256(before[2]).hexdigest())
        self.assertIsNotNone(datetime.fromisoformat(report["timestamp_utc"]).tzinfo)
        self.assertEqual({x.name for x in self.out.iterdir()},
                         {"stdout.txt", "stderr.txt", "report.json"})
        self.assertEqual((self.out / "stdout.txt").read_text().strip(), "SMOKE_T075_FIXTURE")
        self.assertEqual((self.out / "stderr.txt").read_text(), "")
        self.assertEqual(before, (self.script.read_bytes(), self.input.read_bytes(), self.manifest.read_bytes()))
        self.assertEqual({x.name for x in (self.root / "outputs").iterdir()}, {"pnadc_estudo_i"})

    def test_repeated_id_does_not_modify_successful_artifacts(self):
        self.assertEqual(self.invoke(action="executar").returncode, 0)
        old = {f.name: f.read_bytes() for f in self.out.iterdir()}
        p = self.invoke(action="executar")
        self.assertEqual(p.returncode, 2)
        self.assertIn("BLOQUEADO", p.stderr)
        self.assertEqual(old, {f.name: f.read_bytes() for f in self.out.iterdir()})

    def test_denied_r_stan_routes_have_no_side_effects(self):
        for name in ("mortalidade_sp_0_74", "caes_raca_f5"):
            with self.subTest(study=name):
                p = self.invoke(study=name, action="executar")
                self.assertEqual(p.returncode, 2)
                self.assertIn("BLOQUEADO", p.stderr)
        self.assertFalse((self.root / "outputs").exists())

    def test_mismatch_manifest_fails_before_reservation(self):
        self.set_script("print('adulterado')\n")
        self.assertEqual(self.invoke(action="executar").returncode, 2)
        self.assertFalse((self.root / "outputs").exists())

    def test_missing_manifest_fails_before_reservation(self):
        self.manifest.unlink()
        self.assertEqual(self.invoke(action="executar").returncode, 2)
        self.assertFalse((self.root / "outputs").exists())

    def test_missing_public_input_fails_before_reservation(self):
        self.input.unlink()
        self.assertEqual(self.invoke(action="executar").returncode, 2)
        self.assertFalse((self.root / "outputs").exists())

    def test_nonzero_subprocess_preserves_failure_evidence(self):
        self.set_script("raise SystemExit(19)\n")
        self.refresh_manifest()
        p = self.invoke(action="executar")
        self.assertEqual(p.returncode, 2)
        record = self.read_report()
        self.assertEqual(record["returncode"], 19)
        self.assertEqual(record["state"], "FAILED")
        self.assertEqual({x.name for x in self.out.iterdir()},
                         {"stdout.txt", "stderr.txt", "report.json"})
        self.assertEqual(self.invoke(action="executar").returncode, 2)

    def test_output_symlink_blocks_execution_without_following_it(self):
        outside = self.root / "outside"
        outside.mkdir()
        (self.root / "outputs").symlink_to(outside, target_is_directory=True)
        self.assertEqual(self.invoke(action="executar").returncode, 2)
        self.assertEqual(list(outside.iterdir()), [])

    def test_output_path_is_exclusive_even_if_preexisting_empty(self):
        self.out.mkdir(parents=True)
        self.assertEqual(self.invoke(action="executar").returncode, 2)
        self.assertEqual(list(self.out.iterdir()), [])


if __name__ == "__main__":
    unittest.main()
