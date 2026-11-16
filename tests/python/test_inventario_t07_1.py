"""T07.1: contrato documental, sem executar scripts de pesquisa ou obter dados."""
import csv
import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CSV = ROOT / "reports/t07_1/inventario_scripts_T07_1.csv"
EXT = {".r", ".py", ".ps1"}
TAGS = {"LEITURA", "ESCRITA", "SHELL", "LEGADO", "ABSOLUTO", "SEM_SINAL_REGEX"}
IMPORTANT = {
    "scripts/isus_sia/mc_isus_sia.R": "ABSOLUTO",
    "scripts/pnadc_estudo_i/estudo_I/e04/construir_base_e04.py": "LEGADO",
    "scripts/pnadc_estudo_i/estudo_I/e05/explorar_e05.py": "LEGADO",
    "scripts/pnadc_estudo_i/estudo_I/e09/consolidar_e09.py": "LEGADO",
    "scripts/caes_raca_f5/original/E08_MP_Entregaveis/02_codigo/EXECUTAR_E08_MP.ps1": "SHELL",
    "scripts/caes_raca_f5/original/E08_MP_Entregaveis/02_codigo/executar_E08_MP.R": "LEGADO",
    "scripts/pnadc_estudo_i/estudo_I/e07/run.ps1": "SHELL",
    "scripts/mortalidade_sp_0_74/reproduzir_E10.R": "SHELL",
    "scripts/censo_escolar_tempo_integral/censo_escolar_tempo_integral_cmdstanr.R": "SHELL",
}
SHELL_RX = re.compile(r"\\b(?:system2?|subprocess\\.\\w+|os\\.system)\\s*\\(|\\bStart-Process\\b|^\\s*&\\s*\\$Rscript\\b", re.I | re.M)


def load_rows(text=None):
    if text is None:
        text = CSV.read_text(encoding="utf-8")
    rows = list(csv.DictReader(text.splitlines()))
    return rows


def validate(rows, script_paths, strict_lines=True):
    errors = []
    keys = [r.get("caminho", "") for r in rows]
    if len(keys) != len(set(keys)):
        errors.append("Caminhos duplicados")
    if set(keys) != set(script_paths):
        errors.append("Cobertura dos scripts divergente")
    if keys != sorted(keys):
        errors.append("Inventario fora da ordem alfabetica")
    for row in rows:
        path = row.get("caminho", "")
        tags = set(row.get("indicios_lexicais", "").split(","))
        if not tags or not tags <= TAGS or (
            "SEM_SINAL_REGEX" in tags and len(tags) != 1
        ):
            errors.append("Tag invalida: " + path)
        if path not in script_paths:
            continue
        content = (ROOT / path).read_text(encoding="utf-8-sig")
        line = row.get("linha_exemplo", "")
        if line:
            if not line.isdigit() or not 1 <= int(line) <= len(content.splitlines()):
                errors.append("Linha de evidência invalida: " + path)
        elif tags != {"SEM_SINAL_REGEX"}:
            errors.append("Evidencia de linha ausente: " + path)
        if "SHELL" in tags and not SHELL_RX.search(content):
            errors.append("Shell nao verificado: " + path)
        if "ABSOLUTO" in tags and '"/mnt/data/' not in content:
            errors.append("Absoluto nao verificado: " + path)
        if not row.get("nota_de_triagem", "").strip() and tags != {"SEM_SINAL_REGEX"}:
            errors.append("Nota de triagem ausente: " + path)
    for path, tag in IMPORTANT.items():
        found = next((r for r in rows if r["caminho"] == path), None)
        if found is None or tag not in found["indicios_lexicais"].split(","):
            errors.append("Caso adversarial nao classificado: " + path)
    return errors


class TestInventarioT071(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.paths = sorted(
            f.relative_to(ROOT).as_posix()
            for f in (ROOT / "scripts").rglob("*")
            if f.is_file() and f.suffix.lower() in EXT
        )
        cls.rows = load_rows()

    def test_59_scripts_cobertos_e_nenhum_faltante(self):
        self.assertEqual(len(self.paths), 59)
        self.assertEqual(len(self.rows), 59)
        self.assertEqual(validate(self.rows, self.paths), [])

    def test_documentacao_e_riscos_prioritarios(self):
        md = (ROOT / "reports/t07_1/README.md").read_text(encoding="utf-8")
        for token in ("T07.1", "N3/N4", "P0", "falso positivo",
                      "57", "59", "não executa", "T07.2"):
            self.assertIn(token, md)
        self.assertNotIn("LEGADO", next(
            r for r in self.rows if r["caminho"] ==
            "scripts/sherlock_holmes_terms/sherlock_holmes_terms_cmdstanr.R"
        )["indicios_lexicais"])

    def test_mutacoes_adversariais(self):
        from copy import deepcopy
        items = deepcopy(self.rows)
        items.pop()
        self.assertTrue(validate(items, self.paths))
        items = deepcopy(self.rows)
        items[0]["indicios_lexicais"] = "N3_APROVADO"
        self.assertTrue(any("Tag invalida" in x for x in validate(items, self.paths)))
        items = deepcopy(self.rows)
        found = next(r for r in items if r["caminho"] ==
                     "scripts/isus_sia/mc_isus_sia.R")
        found["linha_exemplo"] = "999999"
        self.assertTrue(any("Linha de evidência" in x for x in validate(items, self.paths)))
        items = deepcopy(self.rows)
        found = next(r for r in items if r["caminho"] ==
                     "scripts/pnadc_estudo_i/estudo_I/e07/run.ps1")
        found["indicios_lexicais"] = "LEITURA"
        self.assertTrue(any("Caso adversarial" in x for x in validate(items, self.paths)))


if __name__ == "__main__":
    unittest.main()
