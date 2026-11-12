#!/usr/bin/env python3
"""T05.7: checagens estruturais adversariais do relatório; sem validar ciência."""
import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "reports/t05_7/relatorio_evidencias_T05_7.md"
MAIN_SHA = "18174166f990fc537bd032068315189386690744"
PR_HEAD = "a30de87f5cf50ba1cf62cd036293630d70437a80"


def verify_report(source):
    required = [
        "PASS — infraestrutura", "PASS — reprodução limitada",
        "NÃO DEMONSTRADO — validade científica", MAIN_SHA, PR_HEAD,
        "#43199024122", "#43064300752", "#43064300872",
        "#13874882250", "2/20", "T01.3–T01.7", "QA T05.7 pendente",
        "Etapa 05 aberta", "cancel-in-progress: false",
        "não é compilação C++ nativa", "não dispara por `push/main`",
    ]
    missing = [item for item in required if item not in source]
    if missing:
        raise AssertionError("Evidências/limitações ausentes: " + repr(missing))
    if not re.search(r"\|\s*\*\*NÃO DEMONSTRADO — validade científica\*\*", source):
        raise AssertionError("Validade geral indevidamente classificada como PASS")
    if re.search(r"\*\*PASS\s*[—:-]\s*validade científica", source, re.I):
        raise AssertionError("PASS geral de validade não sustentado")
    if "4.800 draws totais" not in source or "quatro cadeias × 600 draws/modelo" not in source:
        raise AssertionError("Quantidade e escopo de draws inconsistentes")
    if "https://github.com/riscotta/bayes-stan-r/actions/runs/43199024122" not in source:
        raise AssertionError("Run da main sem localização auditável")
    if re.search(r"\*\*QA T05\.7\s*(?:APROVADO|☒)", source, re.I):
        raise AssertionError("Autoaprovação da tarefa")


class ReportT057(unittest.TestCase):
    def test_report(self):
        verify_report(REPORT.read_text(encoding="utf-8"))

    def test_reject_missing_scientific_boundary(self):
        original = REPORT.read_text(encoding="utf-8")
        with self.assertRaises(AssertionError):
            verify_report(original.replace("NÃO DEMONSTRADO — validade científica", "PASS — validade científica"))

    def test_reject_dropped_pr_evidence(self):
        original = REPORT.read_text(encoding="utf-8")
        with self.assertRaises(AssertionError):
            verify_report(original.replace("#43064300872", "#EXECUCAO_INDEFINIDA"))

    def test_reject_unsanctioned_qa(self):
        original = REPORT.read_text(encoding="utf-8")
        with self.assertRaises(AssertionError):
            verify_report(original + "\n**QA T05.7 APROVADO**\n")


if __name__ == "__main__":
    unittest.main()