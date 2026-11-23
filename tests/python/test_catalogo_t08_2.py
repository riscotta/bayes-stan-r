"""T08.2: validação estrutural documental, sem prova de direitos ou de validade científica.

O teste consulta fontes independentes do catálogo (T02.1, T06.1, T06.2 e T06.3).
Não executa código R/Stan, não abre datasets e não substitui revisão semântica E2.
"""
from __future__ import annotations

import json
import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CATALOG = ROOT / "scripts/STUDY_CATALOG_T08_2.md"
TAXONOMY = ROOT / "scripts/STUDY_TAXONOMY_T02_1.md"
MANIFEST = ROOT / "reports/t06_1/manifesto_estudos_T06_1.json"
SCIENTIFIC = ROOT / "reports/t06_2/estados_reprodutibilidade_T06_2.json"
MODES = ROOT / "reports/t06_3/modos_operacionais_T06_3.json"
PROVENANCE = ROOT / "data/DATA_PROVENANCE.md"

IDS = re.compile(r"(?m)^### \x60([a-z0-9_]+)\x60[ \t]*$")
TAX = re.compile(r"(?m)^\| \[\x60([^/]+)\/\x60\]\([^)]*\) \| ([^|]+) \|")
LINK = re.compile(r"(?<!!)\[[^\]]+\]\(([^)\s]+)\)")
FIELDS = ("id", "classe", "pergunta", "objeto", "dados", "metodo",
          "estado", "resultados", "limites", "responsavel",
          "estado_cientifico", "modos_execucao", "proveniencia_direitos",
          "fontes", "corte")
AXES = ("integridade", "sintese", "pipeline", "inferencia", "validacao_cientifica")
OPERATIONS = ("auditoria_rapida", "reexecucao_modelo", "reproducao_desde_dados")
MAIN_SHA = "9c5f026f0fb4d75517c0a7b11d52d34225b804cc"
DATE = "2026-11-23"


def require(value: bool, detail: str) -> None:
    if not value:
        raise ValueError(detail)


def split_records(raw: str) -> tuple[str, dict[str, str]]:
    matches = list(IDS.finditer(raw))
    require(bool(matches), "nenhuma ficha encontrada")
    records: dict[str, str] = {}
    for i, match in enumerate(matches):
        name = match.group(1)
        require(name not in records, "ID repetido: " + name)
        finish = matches[i + 1].start() if i + 1 < len(matches) else raw.find("\n## Gates abertos", match.end())
        require(finish > match.end(), "fim de ficha ausente: " + name)
        records[name] = raw[match.end():finish]
    return raw[:matches[0].start()], records


def validate(raw: str, taxonomy: str, manifest: dict, scientific: dict,
             modes: dict, *, inspect_paths: bool = True) -> bool:
    preamble, entries = split_records(raw)
    classes = dict((name, kind.strip()) for name, kind in TAX.findall(taxonomy))
    require(len(classes) == 33 and len(entries) == 33,
            "cobertura historica de 33 IDs")
    require(set(classes) == set(entries), "IDs distintos da taxonomia")
    require(sum(kind == "Laboratório de modelos" for kind in classes.values()) == 12,
            "laboratorios diferentes de 12")
    require(sum(kind == "Estudo aplicado" for kind in classes.values()) == 21,
            "aplicados diferentes de 21")
    by_m = {r["id"]: r for r in manifest["estudos"]}
    by_s = {r["id"]: r for r in scientific["estudos"]}
    by_o = {r["id"]: r for r in modes["estudos"]}
    require(set(entries) == set(by_m) == set(by_s) == set(by_o),
            "IDs incoerentes com fontes independentes")
    require(scientific["data_corte"] == "2026-11-12", "data do snapshot")
    require(MAIN_SHA in preamble and DATE in preamble, "corte editorial")
    require("issue #14" in preamble and "T06.6" in preamble,
            "gates e evidencias temporais")
    require("não" in preamble.lower(), "fronteira cientifica sem negacao")

    for study_id, section in entries.items():
        require("[\x60" + study_id + "\x60](#" + study_id + ")" in preamble,
                "âncora errada: " + study_id)
        require(re.search(r"(?m)^- \*\*id:\*\* \x60scripts/" + re.escape(study_id) + r"/\x60", section) is not None,
                "caminho historico: " + study_id)
        require("[README do estudo](" + study_id + "/README.md)" in section,
                "README local omitido: " + study_id)
        for field in FIELDS:
            count = len(re.findall(r"(?m)^- \*\*" + field + r":\*\*", section))
            require(count == 1, "campo " + field + " deve ocorrer uma vez: " + study_id)
        require("**classe:** " + classes[study_id] in section,
                "classe incorreta: " + study_id)
        require("2026-11-23" in section and MAIN_SHA in section and "2026-11-12" in section,
                "fonte/corte não declarado: " + study_id)
        require("PENDENTE_DE_VERIFICACAO" in section,
                "lacunas omitidas: " + study_id)
        require("não" in section.lower(), "falta limitação: " + study_id)
        for axis in AXES:
            match = re.search(r"(?m)^  - \x60" + axis + r"\x60: \x60([A-Z0-9_]+)\x60", section)
            require(match is not None, "eixo ausente: " + study_id + "/" + axis)
            require(match.group(1) == by_s[study_id]["estados"][axis]["estado"],
                    "status T06.2 adulterado: " + study_id + "/" + axis)
        for mode in OPERATIONS:
            match = re.search(r"(?m)^  - \x60" + mode + r"\x60[^\n]+", section)
            require(match is not None, "modo omitido: " + study_id + "/" + mode)
            line = match.group(0)
            for required in ("**descrição**", "**comando**", "**pré-requisito/gate**",
                             "**condição de acesso**", "**evidência observada",
                             "**restrição**"):
                require(required in line, "dimensao modo: " + study_id + "/" + mode)
            route = modes.get("rotas_documentadas", {}).get(study_id, {}).get(mode)
            expected_status = route["situacao"] if route else by_o[study_id][mode]
            require("\x60" + expected_status + "\x60" in line,
                    "T06.3 não corresponde: " + study_id + "/" + mode)
            if route and route.get("comando"):
                require("\x60" + route["comando"] + "\x60" in line,
                        "comando nao comprovado: " + study_id + "/" + mode)
            if not route or not route.get("comando"):
                require("PENDENTE_DE_VERIFICACAO" in line,
                        "comando inventado: " + study_id + "/" + mode)
        for source in by_m[study_id]["entradas"]:
            license_label = ("license_status=" + source["license_status_fonte"])
            redist_label = ("redistribution_status=" + source["redistribution_status_fonte"])
            require(license_label in section and redist_label in section,
                    "direitos separados: " + study_id + "/" + source["dataset_id"])
            require("\x60" + source["dataset_id"] + "\x60" in section,
                    "dataset sem identificador: " + study_id)
        require("[DATA_PROVENANCE](../data/DATA_PROVENANCE.md)" in section,
                "ponteiro de direitos: " + study_id)
        if study_id == "caes_raca_f5":
            require("**evidencia_posterior_t06_6:**" in section and "2026-11-16" in section,
                    "piloto F5 não separado")
            require("\x60sintese=NAO_AFERIDO_NESTA_TAREFA\x60" in section,
                    "retroatividade indevida F5")
        else:
            require("**evidencia_posterior_t06_6:**" not in section,
                    "T06.6 extrapolada para " + study_id)
        if inspect_paths:
            require((ROOT / "scripts" / study_id / "README.md").is_file(),
                    "README não existe: " + study_id)

    # Verifica TODOS os destinos Markdown do catálogo, não só nomes.
    for target in LINK.findall(raw):
        if target.startswith(("https://", "http://", "#")):
            continue
        require("data/raw/" not in target and "/outputs/" not in target,
                "link novo para material de dados/resultados: " + target)
        path = (ROOT / "scripts" / target.split("#")[0]).resolve()
        require(path.is_relative_to(ROOT), "link escapa do repositório")
        if inspect_paths:
            require(path.is_file(), "link relativo quebrado: " + target)
    return True


class TestCatalogoT082(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.content = CATALOG.read_text(encoding="utf-8")
        cls.taxonomy = TAXONOMY.read_text(encoding="utf-8")
        cls.manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
        cls.scientific = json.loads(SCIENTIFIC.read_text(encoding="utf-8"))
        cls.modes = json.loads(MODES.read_text(encoding="utf-8"))

    def test_33_fichas_e_evidencias(self) -> None:
        self.assertTrue(validate(self.content, self.taxonomy, self.manifest,
                                 self.scientific, self.modes))
        self.assertTrue(PROVENANCE.is_file())

    def reject_mutation(self, needle: str, replacement: str) -> None:
        self.assertIn(needle, self.content)
        with self.assertRaises(ValueError):
            validate(self.content.replace(needle, replacement, 1), self.taxonomy,
                     self.manifest, self.scientific, self.modes,
                     inspect_paths=False)

    def test_rejeita_id_ausente(self) -> None:
        self.reject_mutation("### \x60caes_raca_f5\x60", "### \x60pseudoestudo_f5\x60")

    def test_rejeita_classe_invertida(self) -> None:
        self.reject_mutation("**classe:** Estudo aplicado", "**classe:** Laboratório de modelos")

    def test_rejeita_campo_ausente(self) -> None:
        self.reject_mutation("**responsavel:**", "**autor_nao_verificado:**")

    def test_rejeita_eixo_n2_promovido(self) -> None:
        self.reject_mutation(
            "\x60sintese\x60: \x60NAO_AFERIDO_NESTA_TAREFA\x60",
            "\x60sintese\x60: \x60AUDITORIA_DESCRITIVA_LIMITADA_DOCUMENTADA\x60",
        )

    def test_rejeita_inferencia_inventada(self) -> None:
        self.reject_mutation(
            "\x60inferencia\x60: \x60NAO_AFERIDO_NESTA_TAREFA\x60",
            "\x60inferencia\x60: \x60INFERENCIA_REEXECUTADA\x60",
        )

    def test_rejeita_modo_omitido(self) -> None:
        self.reject_mutation(
            "\x60reproducao_desde_dados\x60 (reprodução desde dados)",
            "\x60modo_unico\x60 (reprodução desde dados)",
        )

    def test_rejeita_licenca_transformada_em_autorizacao(self) -> None:
        self.reject_mutation("license_status=source_terms_verified",
                             "license_status=redistribuicao_livre_homologada")

    def test_rejeita_link_dados_protegidos(self) -> None:
        self.reject_mutation("[README do estudo](caes_raca_f5/README.md)",
                             "[README do estudo](../data/raw/caes_raca_f5/tabelas.csv)")

    def test_rejeita_retroatividade_f5(self) -> None:
        self.reject_mutation(
            "\x60sintese=NAO_AFERIDO_NESTA_TAREFA\x60",
            "\x60sintese=AUDITORIA_DESCRITIVA_LIMITADA_DOCUMENTADA\x60",
        )

    def test_rejeita_fonte_quebrada(self) -> None:
        self.reject_mutation("[README do estudo](caes_raca_f5/README.md)",
                             "[README do estudo](caes_raca_f5/NAO_EXISTE.md)")

    def test_rejeita_corte_historico_falso(self) -> None:
        self.reject_mutation("manifesto T06.1 \x602026-11-12\x60",
                             "manifesto T06.1 \x602027-11-12\x60")


if __name__ == "__main__":
    unittest.main()
