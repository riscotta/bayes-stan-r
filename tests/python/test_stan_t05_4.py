#!/usr/bin/env python3
"""Testes de protocolo T05.4, stdlib; NAO fingem executar CmdStan."""
import importlib.util
import json
import math
import tempfile
import unittest
from pathlib import Path

SOURCE = Path(__file__).resolve().parents[2] / ".github/scripts/smoke_stan_t05_4.py"
spec = importlib.util.spec_from_file_location("smoke_t054", SOURCE)
smoke = importlib.util.module_from_spec(spec)
spec.loader.exec_module(smoke)


class SmokeProtocolTests(unittest.TestCase):
    def test_fixtures_are_reproducible_and_have_valid_dimensions(self):
        a, b = smoke.synthetic_cases(), smoke.synthetic_cases()
        self.assertEqual(a, b)
        self.assertEqual(sum(a["multinomial"]["truth"]), 1.0)
        x = a["multinomial"]["data"]
        self.assertEqual((x["G"], x["D"]), (2, 4))
        self.assertEqual([sum(y) for y in x["y"]], x["n_slots"])
        with tempfile.TemporaryDirectory() as tmp:
            _, paths = smoke.write_fixtures(tmp)
            self.assertEqual(json.loads(paths["taxa"].read_text()), a["taxa"]["data"])
            self.assertEqual(json.loads(paths["multinomial"].read_text()), x)

    def test_rate_accepts_correct_dims_and_recovery(self):
        rows = [{"lp__": "-1", "p": "0.675", "theta": "67.5", "y_rep": "67"},
                {"lp__": "-2", "p": "0.685", "theta": "68.5", "y_rep": "69"}]
        self.assertAlmostEqual(smoke.check_rate(rows)["posterior_mean"], 0.68)

    def test_rate_rejects_bad_recovery_and_theta(self):
        row = {"lp__": "-1", "p": "0.1", "theta": "10", "y_rep": "12"}
        with self.assertRaisesRegex(ValueError, "Recuperacao"):
            smoke.check_rate([row])
        row["p"] = "0.68"
        with self.assertRaisesRegex(ValueError, "theta"):
            smoke.check_rate([row])

    def multi_row(self):
        result = {"lp__": "-1"}
        p = smoke.TRUTH_MULTINOMIAL
        for k in range(1, 5):
            result[f"p.{k}"] = str(p[k - 1])
            result[f"alpha.{k}"] = "0"
        for i, counts in enumerate(([135, 173, 202, 240], [162, 207, 243, 288]), 1):
            result[f"log_lik.{i}"] = "-10"
            for k, y in enumerate(counts, 1):
                result[f"y_rep_multinomial.{i}.{k}"] = str(y)
        return result

    def test_multinomial_accepts_shapes_simplex_recovery_and_replicates(self):
        data = smoke.synthetic_cases()["multinomial"]["data"]
        got = smoke.check_multinomial([self.multi_row()], data)
        self.assertLess(got["max_abs_error"], 1e-10)

    def test_multinomial_rejects_missing_dimension(self):
        data = smoke.synthetic_cases()["multinomial"]["data"]
        row = self.multi_row()
        del row["p.4"]
        with self.assertRaisesRegex(ValueError, "Dimensao"):
            smoke.check_multinomial([row], data)

    def test_multinomial_rejects_wrong_replicate_total(self):
        data = smoke.synthetic_cases()["multinomial"]["data"]
        row = self.multi_row()
        row["y_rep_multinomial.1.1"] = "1"
        with self.assertRaisesRegex(ValueError, "y_rep"):
            smoke.check_multinomial([row], data)

    def test_multinomial_rejects_bad_parameter_recovery(self):
        data = smoke.synthetic_cases()["multinomial"]["data"]
        row = self.multi_row()
        for k, v in enumerate([0.7, 0.1, 0.1, 0.1], 1):
            row[f"p.{k}"] = str(v)
        with self.assertRaisesRegex(ValueError, "Recuperacao"):
            smoke.check_multinomial([row], data)

    def test_csv_parser_refuses_incomplete_or_nonfinite_sampling(self):
        with tempfile.TemporaryDirectory() as tmp:
            f = Path(tmp) / "stan.csv"
            f.write_text("# metadata\nlp__,p,theta,y_rep\n# adaptation\n-1,0.68,68,67\n", encoding="utf-8")
            self.assertEqual(len(smoke.read_draws(f, expected=1)), 1)
            with self.assertRaisesRegex(ValueError, "Esperados"):
                smoke.read_draws(f, expected=2)
        with self.assertRaisesRegex(ValueError, "nao finito"):
            smoke.check_rate([{"lp__": "-1", "p": "nan", "theta": "68", "y_rep": "67"}])

    def test_does_not_depend_on_external_data(self):
        self.assertTrue(all(path.endswith(".stan") for path in smoke.MODEL_PATHS.values()))
        self.assertEqual(smoke.CMDSTAN_VERSION, "2.40.0")
        self.assertEqual(len(smoke.CMDSTAN_RELEASE_SHA256), 64)


if __name__ == "__main__":
    unittest.main()
