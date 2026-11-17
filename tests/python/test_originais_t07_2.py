"""Regressão e mutações adversariais do contrato T07.2 (sem dados institucionais)."""
import csv
import hashlib
import importlib.util
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[2] / "tools/verificar_originais_t07_2.py"
spec = importlib.util.spec_from_file_location("verificar_originais_t07_2", SCRIPT)
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)


class IntegrityContract(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        (self.root / "scripts/demo").mkdir(parents=True)
        self.source = self.root / "scripts/demo/original.R"
        self.model = self.root / "scripts/demo/model.stan"
        self.source.write_bytes(b"cat('exemplo')\n")
        self.model.write_bytes(b"parameters {real y;} model {y ~ normal(0, 1);}\n")
        self.manifest = self.root / "manifest.csv"
        self.write_manifest()

    def write_manifest(self, mutate=None):
        rows = []
        for p in (self.model, self.source):
            b = p.read_bytes()
            rows.append({"path": p.relative_to(self.root).as_posix(),
                         "sha256": hashlib.sha256(b).hexdigest(), "bytes": str(len(b)),
                         "git_blob_sha1": mod.git_blob_sha1(b)})
        rows.sort(key=lambda row: row["path"])
        if mutate is not None:
            mutate(rows)
        with self.manifest.open("w", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=mod.HEADER)
            writer.writeheader()
            writer.writerows(rows)

    def check(self):
        data = mod.load_manifest(self.manifest)
        mod.verify_tree(self.root, data)

    def test_valid(self):
        self.check()
        self.assertEqual(mod.main(["--root", str(self.root), "--manifest", str(self.manifest)]), 0)

    def test_modified_byte(self):
        self.source.write_bytes(self.source.read_bytes() + b"\n")
        with self.assertRaisesRegex(mod.IntegrityError, "alterado"):
            self.check()

    def test_removed_original(self):
        self.source.unlink()
        with self.assertRaisesRegex(mod.IntegrityError, "removido"):
            self.check()

    def test_new_code(self):
        (self.root / "scripts/demo/novo.py").write_text("print(1)\n")
        with self.assertRaisesRegex(mod.IntegrityError, "novo"):
            self.check()

    def test_tampered_hash(self):
        self.write_manifest(lambda rows: rows[0].update(sha256="0" * 64))
        with self.assertRaisesRegex(mod.IntegrityError, "hash alterado"):
            self.check()

    def test_invalid_hash(self):
        self.write_manifest(lambda rows: rows[0].update(sha256="abcd"))
        with self.assertRaisesRegex(mod.IntegrityError, "hash invalido"):
            self.check()

    def test_repeated_entry(self):
        self.write_manifest(lambda rows: rows.append(dict(rows[0])))
        with self.assertRaisesRegex(mod.IntegrityError, "duplicado"):
            self.check()

    def test_path_escape(self):
        self.write_manifest(lambda rows: rows[0].update(path="scripts/../escape.R"))
        with self.assertRaisesRegex(mod.IntegrityError, "caminho invalido"):
            self.check()

    def test_new_non_code_file_is_outside_contract(self):
        (self.root / "scripts/demo/anotacoes.md").write_text("texto")
        self.check()


if __name__ == "__main__":
    unittest.main()
