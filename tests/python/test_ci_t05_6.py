#!/usr/bin/env python3
"""T05.6: contrato estático da CI; somente stdlib, sem alterar inferência.

Não substitui execução real dos jobs nem auditoria do ruleset GitHub.
"""
import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
WF = ROOT / ".github" / "workflows"


def section(source, key, indent=0):
    """Bloco YAML por indentação, restrito ao contrato conhecido destes arquivos."""
    lines = source.splitlines()
    needle = " " * indent + key + ":"
    for i, line in enumerate(lines):
        if line == needle or line.startswith(needle + " "):
            result = []
            for entry in lines[i + 1:]:
                if entry.strip() and not entry.lstrip().startswith("#"):
                    depth = len(entry) - len(entry.lstrip())
                    if depth <= indent:
                        break
                result.append(entry)
            return "\n".join(result)
    raise AssertionError("Bloco YAML ausente: " + key)


def check_concurrency(source, pr_cancellable):
    block = section(source, "concurrency")
    assert "github.workflow" in block and "github.ref" in block
    if pr_cancellable:
        assert "github.event.pull_request.number || github.ref" in block
        assert "cancel-in-progress: \${{ github.event_name == 'pull_request' }}" in block
    else:
        assert "cancel-in-progress: false" in block


def check_fast(source):
    on = section(source, "on")
    events = set(re.findall(r"^  (push|pull_request|workflow_dispatch):", on, re.M))
    assert events == {"push", "pull_request", "workflow_dispatch"}, events
    for event in ("push", "pull_request"):
        trigger = section(on, event, 2)
        assert ("- main" in trigger or "branches: [main]" in trigger), event
        assert "paths:" not in trigger and "paths-ignore:" not in trigger, event
    check_concurrency(source, pr_cancellable=True)
    assert "continue-on-error: true" not in source

    jobs = section(source, "jobs")
    python = section(jobs, "pnad_n2", 2)
    stan = section(jobs, "stan_syntax", 2)
    required = section(jobs, "validate", 2)
    assert "name: R 4.5.1 — syntax and tests" in required
    assert "needs: [pnad_n2, stan_syntax]" in required
    assert "if: \${{ always() }}" in required
    assert "needs.pnad_n2.result != 'success'" in required
    assert "needs.stan_syntax.result != 'success'" in required
    assert required.count("exit 1") >= 2
    assert "test_ci_t05_6.py" in python
    for test in ("test_stan_t05_3.py", "test_stan_t05_4.py", "test_stan_t05_5.py"):
        assert test in stan, test
    for job in (python, stan, required):
        found = re.search(r"^\s+timeout-minutes:\s*(\d+)\s*$", job, re.M)
        assert found and 0 < int(found.group(1)) <= 15


def check_scientific(source):
    on = section(source, "on")
    events = set(re.findall(r"^  (push|pull_request|workflow_dispatch):", on, re.M))
    assert events == {"pull_request", "workflow_dispatch"}, events
    pr = section(on, "pull_request", 2)
    assert "branches: [main]" in pr
    paths = set(re.findall(r"^\s*-\s*['\"]?([^'\"\n]+)['\"]?\s*$", pr, re.M))
    required = {
        ".github/workflows/stan-t05-5.yml",
        ".github/scripts/check_stan_t05_5.py",
        ".github/scripts/smoke_stan_t05_4.py",
        "scripts/pnadc_estudo_i/estudo_I/e07/stan/taxa_marginal.stan",
        "scripts/mega_sena_dezenas/modelo_multinomial_global.stan",
        "tests/python/test_stan_t05_5.py",
        "reports/t05_5/**",
    }
    assert required <= paths, sorted(required - paths)
    check_concurrency(source, pr_cancellable=True)
    assert "continue-on-error: true" not in source
    assert "timeout-minutes: 60" in source
    assert "if: always()" in source
    assert "actions/upload-artifact@v4" in source
    assert "retention-days: 14" in source
    assert "if-no-files-found: warn" in source


def check_smoke(source):
    on = section(source, "on")
    events = set(re.findall(r"^  (push|pull_request|workflow_dispatch):", on, re.M))
    assert events == {"workflow_dispatch"}, events
    check_concurrency(source, pr_cancellable=False)
    assert "continue-on-error: true" not in source
    assert "timeout-minutes: 55" in source
    assert "if: always()" in source
    assert "actions/upload-artifact@v4" in source
    assert "retention-days: 14" in source


class CIContractT056(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.fast = (WF / "ci.yml").read_text(encoding="utf-8")
        cls.scientific = (WF / "stan-t05-5.yml").read_text(encoding="utf-8")
        cls.smoke = (WF / "stan-smoke-t05-4.yml").read_text(encoding="utf-8")

    def test_actual_workflows(self):
        check_fast(self.fast)
        check_scientific(self.scientific)
        check_smoke(self.smoke)

    def test_reject_skipped_mandatory_job(self):
        broken = self.fast.replace("if: \${{ always() }}", "if: \${{ success() }}")
        self.assertNotEqual(broken, self.fast)
        with self.assertRaises(AssertionError):
            check_fast(broken)

    def test_reject_missing_dependency(self):
        broken = self.fast.replace("needs: [pnad_n2, stan_syntax]", "needs: [pnad_n2]")
        with self.assertRaises(AssertionError):
            check_fast(broken)

    def test_reject_fail_open_dependency(self):
        broken = self.fast.replace("needs.stan_syntax.result != 'success'",
                                   "needs.stan_syntax.result == 'success'")
        with self.assertRaises(AssertionError):
            check_fast(broken)

    def test_reject_path_filter_on_required_ci(self):
        broken = self.fast.replace("  pull_request:\n    branches:",
                                   "  pull_request:\n    paths: ['README.md']\n    branches:")
        with self.assertRaises(AssertionError):
            check_fast(broken)

    def test_reject_missing_scientific_source_trigger(self):
        broken = self.scientific.replace(
            "      - '.github/scripts/smoke_stan_t05_4.py'\n", "")
        with self.assertRaises(AssertionError):
            check_scientific(broken)

    def test_reject_missing_scientific_artifact(self):
        broken = self.scientific.replace("retention-days: 14", "retention-days: 0")
        with self.assertRaises(AssertionError):
            check_scientific(broken)

    def test_reject_fail_open_scientific_job(self):
        with self.assertRaises(AssertionError):
            check_scientific(self.scientific + "\ncontinue-on-error: true\n")

    def test_reject_automatic_expensive_smoke(self):
        broken = self.smoke.replace("  workflow_dispatch:",
                                    "  pull_request:\n    branches: [main]\n  workflow_dispatch:")
        with self.assertRaises(AssertionError):
            check_smoke(broken)

    def test_reject_loss_of_pr_cancellation(self):
        broken = self.fast.replace(
            "cancel-in-progress: \${{ github.event_name == 'pull_request' }}",
            "cancel-in-progress: false")
        with self.assertRaises(AssertionError):
            check_fast(broken)


if __name__ == "__main__":
    unittest.main()
