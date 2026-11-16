"""T07.1: contrato documental, sem executar scripts de pesquisa ou obter dados."""
import csv
import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CSV = ROOT / "reports/t07_1/inventario_scripts_T07_1.csv"
EXT = {".r", ".py", ".ps1"}
TAGS = {"LEITURA", "ESCRITA", "SHELL", "LEGADO", "ABSOLUTO", "AMBIENTE", "SEM_SINAL_REGEX"}
IMPORTANT = {
    "scripts/isus_sia/mc_isus_sia.R": "ABSOLUTO",
    "scripts/caes_raca_f5/caes_raca_f5_cmdstanr.R": "SHELL",
    "scripts/_setup/init_renv.R": "AMBIENTE",
    "scripts/_setup/install_deps.R": "AMBIENTE",
    "scripts/_setup/install_cmdstan.R": "AMBIENTE",
    "scripts/_setup/check_external_links.py": "ESCRITA",
    "scripts/deck_15wins/deck_15wins_negbin_beta_cmdstanr.R": "ESCRITA",
    "scripts/hierarchical/baseball_batting_by_position_3level_cmdstanr.R": "ESCRITA",
    "scripts/saheart/saheart_logistic_cmdstanr.R": "ESCRITA",
    "scripts/pnadc_estudo_i/estudo_I/e04/construir_base_e04.py": "LEGADO",
    "scripts/pnadc_estudo_i/estudo_I/e05/explorar_e05.py": "LEGADO",
    "scripts/pnadc_estudo_i/estudo_I/e09/consolidar_e09.py": "LEGADO",
    "scripts/caes_raca_f5/original/E08_MP_Entregaveis/02_codigo/EXECUTAR_E08_MP.ps1": "SHELL",
    "scripts/caes_raca_f5/original/E08_MP_Entregaveis/02_codigo/executar_E08_MP.R": "LEGADO",
    "scripts/pnadc_estudo_i/estudo_I/e07/run.ps1": "SHELL",
    "scripts/mortalidade_sp_0_74/reproduzir_E10.R": "SHELL",
    "scripts/censo_escolar_tempo_integral/censo_escolar_tempo_integral_cmdstanr.R": "SHELL",
}
SHELL_RX = re.compile(r"\b(?:system2?|subprocess\.\w+|os\.system)\s*\(|\bStart-Process\b|^\s*&\s*\$Rscript\b", re.I | re.M)

# Padroes de triagem estatica: nao comprovam efeitos nem semantica completa.
INSTALL_RX = re.compile(
    r"(?m)^\s*(?:install\.packages|pak::pkg_install|cmdstanr::install_cmdstan)\s*\("
)
IMPLICIT_WRITE_RX = re.compile(r"\bwrite_stan_file\s*\(")
OVERWRITE_RX = re.compile(r"\boverwrite\s*=\s*TRUE\b", re.I)
ABSOLUTE_RX = re.compile(r"/mnt/data/(?:[A-Za-z0-9_.-]+)")


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
            elif strict_lines:
                example = content.splitlines()[int(line) - 1].strip()
                if not example or example.startswith(("#", "//")):
                    errors.append("Linha sem evidência de codigo: " + path)
        elif tags != {"SEM_SINAL_REGEX"}:
            errors.append("Evidencia de linha ausente: " + path)
        # Verificar TODAS as fontes: nao condicionar a descoberta a etiqueta ja presente.
        if bool(SHELL_RX.search(content)) != ("SHELL" in tags):
            errors.append("Shell omitido ou sem lastro: " + path)
        if bool(INSTALL_RX.search(content)) != ("AMBIENTE" in tags):
            errors.append("Ambiente omitido ou sem lastro: " + path)
        if IMPLICIT_WRITE_RX.search(content) and "ESCRITA" not in tags:
            errors.append("Gravacao Stan indireta omitida: " + path)
        if OVERWRITE_RX.search(content) and "ESCRITA" not in tags:
            errors.append("Sobrescrita condicional nao mapeada: " + path)
        if OVERWRITE_RX.search(content) and "overwrite=TRUE" not in row.get("nota_de_triagem", ""):
            errors.append("Risco de sobrescrita nao documentado: " + path)
        if bool(ABSOLUTE_RX.search(content)) != ("ABSOLUTO" in tags):
            errors.append("Caminho absoluto omitido ou sem lastro: " + path)
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
        for token in ("t07.1", "n3/n4", "p0", "falso positivo",
                      "59", "não executa", "t07.2", "write_stan_file",
                      "overwrite=true", "renv.lock", "system2"):
            self.assertIn(token, md.lower())
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


    def test_regressao_shell_omitido_sem_condicionar_a_tag(self):
        from copy import deepcopy
        changed = deepcopy(self.rows)
        path = "scripts/caes_raca_f5/caes_raca_f5_cmdstanr.R"
        row = next(r for r in changed if r["caminho"] == path)
        row["indicios_lexicais"] = row["indicios_lexicais"].replace(",SHELL", "")
        errors = validate(changed, self.paths)
        self.assertTrue(any("Shell omitido" in msg and path in msg for msg in errors), errors)

    def test_regressao_instalacoes_e_escrita_indireta(self):
        from copy import deepcopy
        for path, tag, snippet in (
            ("scripts/_setup/init_renv.R", "AMBIENTE", "Ambiente omitido"),
            ("scripts/_setup/install_cmdstan.R", "AMBIENTE", "Ambiente omitido"),
            ("scripts/deck_15wins/deck_15wins_negbin_beta_cmdstanr.R", "ESCRITA", "Gravacao Stan indireta"),
        ):
            changed = deepcopy(self.rows)
            row = next(r for r in changed if r["caminho"] == path)
            row["indicios_lexicais"] = ",".join(
                x for x in row["indicios_lexicais"].split(",") if x != tag
            )
            errors = validate(changed, self.paths)
            self.assertTrue(any(snippet in msg and path in msg for msg in errors), errors)
        self.assertEqual(sum("SHELL" in r["indicios_lexicais"].split(",") for r in self.rows), 5)
        self.assertEqual(sum("AMBIENTE" in r["indicios_lexicais"].split(",") for r in self.rows), 13)

    def test_strict_lines_e_risco_de_sobrescrita(self):
        from copy import deepcopy
        changed = deepcopy(self.rows)
        path = "scripts/missing_migrants/missing_migrants_cmdstanr.R"
        row = next(r for r in changed if r["caminho"] == path)
        row["nota_de_triagem"] = "sem identificacao de risco"
        errors = validate(changed, self.paths)
        self.assertTrue(any("Risco de sobrescrita" in x and path in x for x in errors), errors)
        changed = deepcopy(self.rows)
        path = "scripts/_setup/init_renv.R"
        row = next(r for r in changed if r["caminho"] == path)
        lines = (ROOT / path).read_text(encoding="utf-8-sig").splitlines()
        blank_index = next(i for i, x in enumerate(lines, 1) if not x.strip())
        row["linha_exemplo"] = str(blank_index)
        self.assertTrue(any("Linha sem evidência" in x for x in validate(changed, self.paths)))
        self.assertFalse(any("Linha sem evidência" in x for x in
                             validate(changed, self.paths, strict_lines=False)))

if __name__ == "__main__":
    unittest.main()
