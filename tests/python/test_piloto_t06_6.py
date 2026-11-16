"""T06.6: teste de contrato e adversariais antes do piloto R em CI."""
import csv
import importlib.util
from pathlib import Path
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / ".github" / "scripts" / "t06_6_piloto_f5.py"
spec = importlib.util.spec_from_file_location("piloto_t066", SRC)
pilot = importlib.util.module_from_spec(spec)
spec.loader.exec_module(pilot)


def write_rows(path, rows, fields):
    with path.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


class TestPilotoT066(unittest.TestCase):
    def setUp(self):
        self.expected = pilot.expected_rows()
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.out = Path(self.temp.name) / "comparacao.csv"
        self.rows = [
            {key: str(value) for key, value in record.items()}
            for record in self.expected
        ]
        write_rows(self.out, self.rows, pilot.FIELDS)

    def test_oraculo_independente_fonte_publica(self):
        self.assertEqual(len(self.expected), 3)
        self.assertEqual(pilot.compare_generated(self.expected, self.out), 30)

    def test_mutacao_numericamente_proxima_dentro_da_tolerancia(self):
        self.rows[0]["rmse_M1"] = str(self.expected[0]["rmse_M1"] + pilot.ATOL / 2)
        write_rows(self.out, self.rows, pilot.FIELDS)
        self.assertEqual(pilot.compare_generated(self.expected, self.out), 30)

    def test_mutacao_erro_de_rmse_falha(self):
        self.rows[1]["rmse_M2"] = str(self.expected[1]["rmse_M2"] + pilot.ATOL * 100)
        write_rows(self.out, self.rows, pilot.FIELDS)
        with self.assertRaisesRegex(ValueError, "Divergência"):
            pilot.compare_generated(self.expected, self.out)

    def test_mutacao_nan_falha(self):
        self.rows[0]["ganho_pct"] = "NaN"
        write_rows(self.out, self.rows, pilot.FIELDS)
        with self.assertRaisesRegex(ValueError, "não finito"):
            pilot.compare_generated(self.expected, self.out)

    def test_mutacao_familia_duplicada_falha(self):
        self.rows[1]["familia"] = "t5"
        write_rows(self.out, self.rows, pilot.FIELDS)
        with self.assertRaisesRegex(ValueError, "Famílias"):
            pilot.compare_generated(self.expected, self.out)

    def test_mutacao_coluna_omitida_falha(self):
        fields = pilot.FIELDS[:-1]
        write_rows(self.out, [{k: r[k] for k in fields} for r in self.rows], fields)
        with self.assertRaisesRegex(ValueError, "Esquema"):
            pilot.compare_generated(self.expected, self.out)

    def test_mutacao_linha_duplicada_falha(self):
        write_rows(self.out, self.rows + [self.rows[0]], pilot.FIELDS)
        with self.assertRaisesRegex(ValueError, "Famílias"):
            pilot.compare_generated(self.expected, self.out)

    def test_mutacao_fonte_sem_fold_falha(self):
        folder = Path(self.temp.name) / "inputs"
        folder.mkdir()
        for name in pilot.SOURCES:
            (folder / name).write_bytes((pilot.INPUT / name).read_bytes())
        csv_path = folder / "contrastes_por_fold_auditados.csv"
        rows = pilot.read_csv(csv_path)
        write_rows(csv_path, rows[1:], list(rows[0]))
        with self.assertRaisesRegex(ValueError, "Contagem"):
            pilot.expected_rows(folder)

    def test_mutacao_fonte_ic_invertido_falha(self):
        folder = Path(self.temp.name) / "inputs"
        folder.mkdir()
        for name in pilot.SOURCES:
            (folder / name).write_bytes((pilot.INPUT / name).read_bytes())
        csv_path = folder / "bootstrap_independente_2000.csv"
        rows = pilot.read_csv(csv_path)
        rows[0]["LS95"] = "-1"
        write_rows(csv_path, rows, list(rows[0]))
        with self.assertRaisesRegex(ValueError, "Intervalo"):
            pilot.expected_rows(folder)


if __name__ == "__main__":
    unittest.main()
