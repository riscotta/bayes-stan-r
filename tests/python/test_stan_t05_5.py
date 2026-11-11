#!/usr/bin/env python3
"""T05.5: testes adversariais de protocolo, nao simulam CmdStan nativo."""
import csv
import importlib.util
import tempfile
import unittest
from pathlib import Path

PATH = Path(__file__).resolve().parents[2] / ".github/scripts/check_stan_t05_5.py"
sp = importlib.util.spec_from_file_location("t055", PATH)
t = importlib.util.module_from_spec(sp)
sp.loader.exec_module(t)


class T055Protocol(unittest.TestCase):
    def test_prior_deterministic(self):
        a = t.prior_checks(24, enforce=False)
        self.assertEqual(a, t.prior_checks(24, enforce=False))
        self.assertAlmostEqual(a["multinomial"]["marginal_alpha_variance"],
                               0.35**2 * 0.75)
        self.assertIn("independente", a["method"])

    def test_prior_rejects_bad_n(self):
        for n in (0, 1, -5, 2.3):
            with self.subTest(n=n), self.assertRaises(ValueError):
                t.prior_checks(n)

    def base_chain(self):
        # Serie oscilante intencionalmente simples: apenas teste estrutural.
        return [{"energy__": str(100 + ((i % 7) - 3) * .5),
                 "divergent__": "0", "treedepth__": "2", "p": "0.68"}
                for i in range(t.DRAWS)]

    def test_hmc_accepts_finite_complete_chain(self):
        got = t.check_chain(self.base_chain())
        self.assertEqual(got["n_draws"], t.DRAWS)
        self.assertGreater(got["ebfmi"], t.BFMI_LIMIT)

    def test_hmc_rejects_divergence(self):
        rows = self.base_chain()
        rows[9]["divergent__"] = "1"
        with self.assertRaisesRegex(ValueError, "Divergencias"):
            t.check_chain(rows)

    def test_hmc_rejects_treedepth(self):
        rows = self.base_chain()
        rows[2]["treedepth__"] = "10"
        with self.assertRaisesRegex(ValueError, "Treedepth"):
            t.check_chain(rows)

    def test_hmc_rejects_low_bfmi(self):
        rows = self.base_chain()
        for i, row in enumerate(rows):
            row["energy__"] = str(100 + i * 0.01)
        with self.assertRaisesRegex(ValueError, "E-BFMI"):
            t.check_chain(rows)

    def test_hmc_rejects_truncation_and_nonfinite(self):
        with self.assertRaisesRegex(ValueError, "truncado"):
            t.check_chain(self.base_chain()[:-1])
        rows = self.base_chain()
        rows[0]["p"] = "nan"
        with self.assertRaisesRegex(ValueError, "nao finito"):
            t.check_chain(rows)

    def write_summary(self, folder, model="taxa", rhat="1.001", bulk="900", tail="750"):
        path = Path(folder) / "summary.csv"
        fields = ["name", "Mean", "ESS_bulk", "ESS_tail", "R_hat"]
        vars = (["p"] if model == "taxa" else
                [f"alpha[{k}]" for k in range(1, 5)] +
                [f"p[{k}]" for k in range(1, 5)])
        with path.open("w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=fields)
            writer.writeheader()
            for var in vars:
                writer.writerow({"name": var, "Mean": "0.2",
                                 "ESS_bulk": bulk, "ESS_tail": tail, "R_hat": rhat})
        return path

    def test_summary_accepts_both_models(self):
        with tempfile.TemporaryDirectory() as d:
            self.assertEqual(len(t.summary_diagnostics(self.write_summary(d), "taxa")), 1)
            self.assertEqual(len(t.summary_diagnostics(self.write_summary(d, "multinomial"),
                                                       "multinomial")), 8)

    def test_summary_rejects_rhat_boundary(self):
        with tempfile.TemporaryDirectory() as d:
            p = self.write_summary(d, rhat="1.01")
            with self.assertRaisesRegex(ValueError, "R-hat"):
                t.summary_diagnostics(p, "taxa")

    def test_summary_rejects_ess_bulk_and_tail(self):
        with tempfile.TemporaryDirectory() as d:
            for bulk, tail in (("399", "900"), ("900", "399")):
                with self.subTest(bulk=bulk, tail=tail):
                    p = self.write_summary(d, bulk=bulk, tail=tail)
                    with self.assertRaisesRegex(ValueError, "ESS"):
                        t.summary_diagnostics(p, "taxa")

    def test_summary_rejects_missing_parameter(self):
        with tempfile.TemporaryDirectory() as d:
            p = self.write_summary(d, model="taxa")
            with self.assertRaisesRegex(ValueError, "Parametro ausente"):
                t.summary_diagnostics(p, "multinomial")

    def test_rate_ppc_accepts_and_rejects_adulteration(self):
        data = {"y": 68, "s": 0.55}
        rows = [{"p": "0.68", "theta": "68", "y_rep": "68.03"}] * 50
        result = t.posterior_predictive(rows, "taxa", data)
        self.assertTrue(result["not_a_calibration_test"])
        bad = [{"p": "0.68", "theta": "68", "y_rep": "70.0"}]
        with self.assertRaisesRegex(ValueError, "PPC taxa"):
            t.posterior_predictive(bad, "taxa", data)

    def test_multinomial_ppc_accepts_and_rejects(self):
        data = t.smoke.synthetic_cases()["multinomial"]["data"]
        row = {}
        for k, p in enumerate(t.smoke.TRUTH_MULTINOMIAL, 1):
            row[f"p.{k}"] = str(p)
            for i, n in enumerate(data["n_slots"], 1):
                row[f"y_rep_multinomial.{i}.{k}"] = str(round(n*p))
        report = t.posterior_predictive([row] * 10, "multinomial", data)
        self.assertEqual(len(report["conditional_mean_errors"]), 8)
        row["y_rep_multinomial.1.1"] = "0"
        with self.assertRaisesRegex(ValueError, "PPC multinomial"):
            t.posterior_predictive([row], "multinomial", data)

    def test_scope_does_not_change_stan(self):
        self.assertEqual(set(t.smoke.MODEL_PATHS), {"taxa", "multinomial"})
        self.assertEqual(len(t.SEEDS["taxa"]), 4)
        self.assertEqual(t.CHAINS, 4)
        self.assertEqual(t.ESS_LIMIT, 100 * t.CHAINS)
        self.assertEqual(t.MAX_DEPTH, 10)


if __name__ == "__main__":
    unittest.main()
