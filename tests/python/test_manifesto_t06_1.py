"""T06.1 — verificação estrutural do manifesto, sem leitura de dados institucionais."""
import copy
import csv
import json
from pathlib import Path
import re
import subprocess
import unittest

ROOT = Path(__file__).resolve().parents[2]
MANIFEST = ROOT / "reports/t06_1/manifesto_estudos_T06_1.json"
SHA = re.compile(r"^[0-9a-f]{40}$")
SOURCE_EXT = (".R", ".r", ".py", ".stan", ".Rmd", ".qmd", ".sh", ".ps1")


# Contratos de cobertura confrontados com data/DATA_PROVENANCE.csv e READMEs.
# Revisões do catálogo exigem revisão explícita deste mapa (não é gerado do manifesto sob teste).
EXPECTED_DATASET_LINKS = {
    "caes_raca_f5": [
        "caes_raca_f5"
    ],
    "censo_escolar_tempo_integral": [
        "censo_escolar"
    ],
    "consumer_shopping_trends_beta": [
        "consumer_shopping_trends"
    ],
    "deter_mensal_bioma_uf": [
        "deter_mensal_bioma_uf"
    ],
    "exercises_dataset": [
        "exercises_dataset"
    ],
    "hierarchical": [
        "batting_average"
    ],
    "importacoes_world_bank": [
        "importacoes_world_bank"
    ],
    "isus_sia": [
        "isus_sia"
    ],
    "mega_sena_dezenas": [
        "mega_sena_dezenas"
    ],
    "missing_migrants": [
        "missing_migrants"
    ],
    "mortalidade_sp_0_74": [
        "mortalidade_sp_0_74"
    ],
    "mortality": [
        "mortality"
    ],
    "pms_servicos": [
        "pms_servicos"
    ],
    "pnadc_desocupacao": [
        "pnadc_desocupacao"
    ],
    "pnadc_estudo_i": [
        "pnadc_estudo_i"
    ],
    "prisons_prisoners_unodc": [
        "prisons_prisoners_unodc"
    ],
    "retractions_reason_composition": [
        "retractions_time_to_retraction"
    ],
    "retractions_time_to_retraction": [
        "retractions_time_to_retraction"
    ],
    "retractions_trend": [
        "retractions_time_to_retraction"
    ],
    "rs_seguro": [
        "rs_seguro_macrocrime",
        "rs_seguro_macrocrime_profile",
        "rs_seguro_crime_optional"
    ],
    "saheart": [
        "saheart"
    ],
    "sat_model_selection": [
        "sat"
    ],
    "sherlock_holmes_terms": [
        "sherlock_holmes_terms"
    ],
    "sobrevivencia_cadastral_poa": [
        "sobrevivencia_cadastral_poa"
    ],
    "therapeutic_touch": [
        "therapeutic_touch"
    ]
}
EXPECTED_README_SEEDS = {
    "censo_escolar_tempo_integral": 42,
    "consumer_shopping_trends_beta": 20260410,
    "deter_mensal_bioma_uf": 20260316,
    "fruitflies_aft": 42,
    "hierarchical": 123,
    "inhaler_ordinal": 20260302,
    "ipca_sidra_1419": 20260318,
    "pms_servicos": 20260403,
    "pnadc_desocupacao": 20260402,
    "retractions_reason_composition": 20260407,
    "retractions_trend": 20260407,
    "saheart": 123,
    "sat_model_selection": 42,
    "sobrevivencia_cadastral_poa": 20260721
}
EXPECTED_EXTERNAL_GAPS = {
    "coortes_aposentadoria": [
        "Yahoo Finance",
        "BCB/SGS **4390**",
        "BCB/SGS **433**"
    ],
    "fruitflies_aft": [
        "Stat2Data::FruitFlies"
    ],
    "inhaler_ordinal": [
        "pacote **`brms`**"
    ],
    "ipca_sidra_1419": [
        "tabela `1737`",
        "tabela `7060`"
    ],
    "kidney_survival": [
        "pacote **`survival`**"
    ]
}
README_DATASET_ANCHORS = {
    "censo_escolar_tempo_integral": "data/raw/censo_escolar/",
    "retractions_reason_composition": "data/raw/retractions_time_to_retraction/global_scientific_retractions_1927_2026.csv",
    "retractions_trend": "data/raw/retractions_time_to_retraction/global_scientific_retractions_1927_2026.csv",
}
SEED_IN_README = re.compile(r"--seed(?:_model)?\s*=\s*(\d+)|\bseed\s+`(\d+)`", re.IGNORECASE)

def _readme(ident):
    """Somente README público versionado: não lê dados brutos ou fontes institucionais."""
    return (ROOT / "scripts" / ident / "README.md").read_text(encoding="utf-8")


def git_tree():
    output = subprocess.check_output(["git", "ls-tree", "-r", "HEAD"], cwd=ROOT, text=True)
    return {line.split("\t", 1)[1]: line.split()[2] for line in output.splitlines()}


def validate(manifest, tree, provenance):
    if manifest.get("schema_version") != "1.0.0" or manifest.get("tarefa") != "T06.1":
        raise ValueError("schema ou tarefa")
    if not SHA.fullmatch(manifest.get("base_git_commit", "")):
        raise ValueError("SHA-base")
    studies = manifest.get("estudos", [])
    if len(studies) != 33 or len({s["id"] for s in studies}) != 33:
        raise ValueError("cobertura de 33 estudos")
    expected = {
        p.split("/")[1] for p in tree
        if re.fullmatch(r"scripts/[^/]+/README\.md", p) and p != "scripts/_setup/README.md"
    }
    if {s["id"] for s in studies} != expected:
        raise ValueError("unidades omitidas ou inventadas")
    for study in studies:
        ident = study["id"]
        if not re.fullmatch(r"[a-z0-9_]+", ident):
            raise ValueError("id")
        if study["licenca_e_permissoes"]["estado"] != "PENDENTE_DE_HOMOLOGACAO":
            raise ValueError("homologação indevida")
        if study["estado_reproducao"] != "NAO_CERTIFICADO_PELA_T06_1":
            raise ValueError("falso PASS reprodução")
        if study["versao"]["git_commit_de_referencia"] != manifest["base_git_commit"]:
            raise ValueError("versão inconsistente")
        if study["seed"]["estado"] == "PENDENTE_DE_VERIFICACAO" and study["seed"]["valor"] is not None:
            raise ValueError("seed não documentada")
        if study["ambiente"]["estado"] != "REFERENCIA_REPOSITORIO_NAO_VALIDADA_POR_ESTUDO":
            raise ValueError("ambiente certificado indevidamente")
        files = study["arquivos"]
        for label in ("codigo", "documentacao", "relatorios_versionados", "outputs_versionados"):
            if label not in files:
                raise ValueError("grupo de arquivos ausente")
            for item in files[label]:
                path = item["caminho"]
                if path not in tree or tree[path] != item["git_blob_sha"] or not SHA.fullmatch(item["git_blob_sha"]):
                    raise ValueError("arquivo/SHA divergente: " + path)
        codes = {x["caminho"] for x in files["codigo"]}
        expected_codes = {p for p in tree if p.startswith(f"scripts/{ident}/") and p.endswith(SOURCE_EXT)}
        if codes != expected_codes:
            raise ValueError("códigos não inventariados: " + ident)
        if {x["caminho"] for x in files["documentacao"]} != {f"scripts/{ident}/README.md"}:
            raise ValueError("README não rastreável")
        for item in study["entradas"]:
            src = provenance.get(item["dataset_id"])
            if src is None or any(item[key] != src[source] for key, source in (
                ("escopo_caminho", "path_scope"),
                ("license_status_fonte", "license_status"),
                ("redistribution_status_fonte", "redistribution_status"),
            )):
                raise ValueError("proveniência não corresponde")
            if item["estado_permissao"] != "PENDENTE_DE_HOMOLOGACAO":
                raise ValueError("licença promovida")
            for file in item["arquivos_git"]:
                if tree.get(file["caminho"]) != file["git_blob_sha"]:
                    raise ValueError("arquivo de entrada divergente")
        for cmd in study["comandos_documentados"]:
            if cmd["execucao_t06_1"] != "NAO_EXECUTADO_NESTA_TAREFA":
                raise ValueError("execução não demonstrada")
            if not cmd["comando"] or cmd["fonte"] not in tree:
                raise ValueError("comando não rastreável")


    # Confronto independente: contrato curado de vínculos, CSV e READMEs públicos.
    actual_links = {s["id"]: [e["dataset_id"] for e in s["entradas"]]
                    for s in studies if s["entradas"]}
    if actual_links != EXPECTED_DATASET_LINKS:
        raise ValueError("vínculo obrigatório ausente, duplicado ou atribuído ao estudo errado")
    if set(provenance) != {dataset for links in EXPECTED_DATASET_LINKS.values()
                          for dataset in links}:
        raise ValueError("dataset do catálogo sem cobertura ou dataset não catalogado no contrato")
    if manifest.get("escopo", {}).get("total_registros_proveniencia") != len(provenance):
        raise ValueError("contagem de catálogo divergente")
    if manifest["escopo"].get("total_vinculos_catalogo_por_estudo") != sum(
            map(len, EXPECTED_DATASET_LINKS.values())):
        raise ValueError("contagem de vínculos divergente")
    if manifest["escopo"].get("total_seeds_documentadas_em_readmes") != len(EXPECTED_README_SEEDS):
        raise ValueError("contagem de seeds divergente")
    n_placeholders = 0
    for study in studies:
        ident = study["id"]
        readme = _readme(ident)
        # Uma fonte existente não prova que o comando informado nela existe.
        for cmd in study["comandos_documentados"]:
            source = cmd["fonte"]
            if source == study["documentacao_de_estudo"]:
                source_text = readme
            elif source == ".github/workflows/ci.yml":
                source_text = (ROOT / source).read_text(encoding="utf-8")
            else:
                raise ValueError("fonte do comando fora do escopo documental: " + ident)
            if cmd["comando"] not in source_text:
                raise ValueError("comando literal não encontrado na fonte: " + ident)
        if ident in README_DATASET_ANCHORS and README_DATASET_ANCHORS[ident] not in readme:
            raise ValueError("fonte documental da entrada não encontrada: " + ident)
        documented = {int(a or b) for a, b in SEED_IN_README.findall(readme)}
        expected_seed = EXPECTED_README_SEEDS.get(ident)
        if documented != ({expected_seed} if expected_seed is not None else set()):
            raise ValueError("seed do README diverge do contrato independente: " + ident)
        seed = study["seed"]
        if expected_seed is not None:
            if seed["valor"] != expected_seed or seed["estado"] != "DOCUMENTADA_NO_README_NAO_AFERIDA_EM_EXECUCAO":
                raise ValueError("seed documentada omitida, errada ou com estado falso: " + ident)
            if seed["fonte"] != study["documentacao_de_estudo"]:
                raise ValueError("seed sem proveniência documental: " + ident)
            if "históricas não verificada" not in seed["alcance"]:
                raise ValueError("seed com garantia histórica indevida: " + ident)
        elif seed["valor"] is not None or seed["estado"] != "PENDENTE_DE_VERIFICACAO":
            raise ValueError("seed sem respaldo no README: " + ident)
        gaps = study.get("lacunas_entradas_externas_nao_catalogadas")
        required = EXPECTED_EXTERNAL_GAPS.get(ident, [])
        if not isinstance(gaps, list) or [g["referencia_readme"] for g in gaps] != required:
            raise ValueError("lacuna externa documentada ausente ou inventada: " + ident)
        for gap in gaps:
            if gap["referencia_readme"] not in readme:
                raise ValueError("lacuna não comprovada no README: " + ident)
            if gap["estado"] != "LACUNA_NAO_HOMOLOGADA":
                raise ValueError("licença externa falsamente homologada: " + ident)
            if gap["documentacao_de_estudo"] != study["documentacao_de_estudo"]:
                raise ValueError("fonte de lacuna incorreta: " + ident)
        # O vínculo catalogado prevalece na classificação; lacunas externas
        # permanecem explicitamente rastreadas no campo próprio.
        expected_entry_state = (
            "REFERENCIAS_DOCUMENTADAS_COM_RESTRICOES" if study["entradas"]
            else "FONTE_EXTERNA_DECLARADA_SEM_REGISTRO_DE_PROVENIENCIA" if required
            else "SEM_VINCULO_INVENTARIO_VERIFICADO"
        )
        if study.get("estado_entrada") != expected_entry_state:
            raise ValueError("estado de entrada incoerente: " + ident)
        files = study["arquivos"]
        if any(e["caminho"].endswith("/.gitkeep") for e in files["outputs_versionados"]):
            raise ValueError("placeholder tratado como output científico: " + ident)
        keeps = files.get("placeholders_diretorio_versionados")
        if not isinstance(keeps, list):
            raise ValueError("placeholders não classificados: " + ident)
        n_placeholders += len(keeps)
        for item in keeps:
            path = item["caminho"]
            if not path.startswith("outputs/" + ident + "/") or not path.endswith("/.gitkeep"):
                raise ValueError("placeholder fora do estudo: " + ident)
            if tree.get(path) != item["git_blob_sha"] or item["bytes"] != 0:
                raise ValueError("placeholder não rastreável: " + path)
        # Cobertura não autorreferente: todos os arquivos versionados nas
        # pastas do estudo devem figurar no manifesto, inclusive placeholders.
        expected_categories = {
            "relatorios_versionados": {
                p for p in tree if p.startswith(f"reports/{ident}/")
            },
            "outputs_versionados": {
                p for p in tree
                if p.startswith(f"outputs/{ident}/") and not p.endswith("/.gitkeep")
            },
            "placeholders_diretorio_versionados": {
                p for p in tree
                if p.startswith(f"outputs/{ident}/") and p.endswith("/.gitkeep")
            },
        }
        for category, expected_paths in expected_categories.items():
            declared_paths = [item["caminho"] for item in files[category]]
            if (len(declared_paths) != len(set(declared_paths))
                    or set(declared_paths) != expected_paths):
                raise ValueError("cobertura de arquivos incompleta: " + ident + "/" + category)
    if n_placeholders != manifest["escopo"].get("total_placeholders_de_saida"):
        raise ValueError("contagem de placeholders divergente")


class ManifestTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.data = json.loads(MANIFEST.read_text(encoding="utf-8"))
        cls.tree = git_tree()
        with (ROOT / "data/DATA_PROVENANCE.csv").open(encoding="utf-8", newline="") as f:
            cls.prov = {x["dataset_id"]: x for x in csv.DictReader(f)}

    def test_33_unidades_e_contratos(self):
        validate(self.data, self.tree, self.prov)

    def test_sem_falsa_homologacao(self):
        bad = copy.deepcopy(self.data)
        bad["estudos"][0]["licenca_e_permissoes"]["estado"] = "HOMOLOGADA"
        with self.assertRaisesRegex(ValueError, "homologação indevida"):
            validate(bad, self.tree, self.prov)

    def test_hash_adulterado_e_detectado(self):
        bad = copy.deepcopy(self.data)
        bad["estudos"][0]["arquivos"]["codigo"][0]["git_blob_sha"] = "0" * 40
        with self.assertRaisesRegex(ValueError, "arquivo/SHA divergente"):
            validate(bad, self.tree, self.prov)

    def test_seed_e_execucao_nao_inventadas(self):
        bad = copy.deepcopy(self.data)
        s = next(x for x in bad["estudos"] if x["seed"]["estado"] == "PENDENTE_DE_VERIFICACAO")
        s["seed"]["valor"] = 42
        with self.assertRaisesRegex(ValueError, "seed não documentada"):
            validate(bad, self.tree, self.prov)
        bad = copy.deepcopy(self.data)
        bad["estudos"][0]["comandos_documentados"][0]["execucao_t06_1"] = "PASS"
        with self.assertRaisesRegex(ValueError, "execução não demonstrada"):
            validate(bad, self.tree, self.prov)


    def test_vinculos_obrigatorios_incluindo_compartilhamento(self):
        for study_id in ("censo_escolar_tempo_integral", "retractions_reason_composition",
                         "retractions_trend", "rs_seguro"):
            bad = copy.deepcopy(self.data)
            study = next(s for s in bad["estudos"] if s["id"] == study_id)
            study["entradas"].pop()
            with self.subTest(study=study_id), self.assertRaisesRegex(ValueError, "vínculo obrigatório"):
                validate(bad, self.tree, self.prov)

    def test_seeds_documentadas_omitidas_e_incorretas(self):
        for study_id in ("ipca_sidra_1419", "pnadc_desocupacao", "sobrevivencia_cadastral_poa"):
            for value in (None, 987654321):
                bad = copy.deepcopy(self.data)
                seed = next(s for s in bad["estudos"] if s["id"] == study_id)["seed"]
                seed["valor"] = value
                with self.subTest(study=study_id, value=value), self.assertRaisesRegex(ValueError, "seed documentada"):
                    validate(bad, self.tree, self.prov)

    def test_lacunas_remotas_documentadas_sem_licencas_inventadas(self):
        bad = copy.deepcopy(self.data)
        study = next(s for s in bad["estudos"] if s["id"] == "ipca_sidra_1419")
        study["lacunas_entradas_externas_nao_catalogadas"] = []
        with self.assertRaisesRegex(ValueError, "lacuna externa"):
            validate(bad, self.tree, self.prov)
        bad = copy.deepcopy(self.data)
        study = next(s for s in bad["estudos"] if s["id"] == "coortes_aposentadoria")
        study["lacunas_entradas_externas_nao_catalogadas"][0]["estado"] = "LICENCA_HOMOLOGADA"
        with self.assertRaisesRegex(ValueError, "licença externa"):
            validate(bad, self.tree, self.prov)

    def test_placeholders_nao_sao_resultados(self):
        bad = copy.deepcopy(self.data)
        study = next(s for s in bad["estudos"] if s["id"] == "censo_escolar_tempo_integral")
        study["arquivos"]["outputs_versionados"].extend(
            study["arquivos"]["placeholders_diretorio_versionados"][:1])
        with self.assertRaisesRegex(ValueError, "placeholder tratado"):
            validate(bad, self.tree, self.prov)

    def test_cobertura_do_catalogo_nao_e_autorreferente(self):
        bad = copy.deepcopy(self.data)
        study = next(s for s in bad["estudos"] if s["id"] == "retractions_reason_composition")
        study["entradas"][0]["dataset_id"] = "retractions_trend"
        with self.assertRaisesRegex(ValueError, "proveniência não corresponde|vínculo obrigatório"):
            validate(bad, self.tree, self.prov)


    def test_estado_entrada_coerente_com_vinculos_e_lacunas(self):
        for study_id in ("censo_escolar_tempo_integral", "retractions_reason_composition",
                         "retractions_trend"):
            bad = copy.deepcopy(self.data)
            study = next(s for s in bad["estudos"] if s["id"] == study_id)
            study["estado_entrada"] = "SEM_VINCULO_INVENTARIO_VERIFICADO"
            with self.subTest(study=study_id), self.assertRaisesRegex(
                    ValueError, "estado de entrada incoerente"):
                validate(bad, self.tree, self.prov)
        for study_id, state in (
                ("coortes_aposentadoria", "SEM_VINCULO_INVENTARIO_VERIFICADO"),
                ("deck_15wins", "REFERENCIAS_DOCUMENTADAS_COM_RESTRICOES")):
            bad = copy.deepcopy(self.data)
            next(s for s in bad["estudos"] if s["id"] == study_id)["estado_entrada"] = state
            with self.subTest(study=study_id), self.assertRaisesRegex(
                    ValueError, "estado de entrada incoerente"):
                validate(bad, self.tree, self.prov)

    def test_relatorios_omitidos_detectados_pela_arvore_git(self):
        for study_id in ("mortalidade_sp_0_74", "pnadc_estudo_i"):
            bad = copy.deepcopy(self.data)
            study = next(s for s in bad["estudos"] if s["id"] == study_id)
            study["arquivos"]["relatorios_versionados"].pop()
            with self.subTest(study=study_id), self.assertRaisesRegex(
                    ValueError, "cobertura de arquivos incompleta"):
                validate(bad, self.tree, self.prov)

    def test_placeholders_omitidos_com_contagem_adulterada(self):
        for study_id in ("censo_escolar_tempo_integral", "mortalidade_sp_0_74"):
            bad = copy.deepcopy(self.data)
            study = next(s for s in bad["estudos"] if s["id"] == study_id)
            study["arquivos"]["placeholders_diretorio_versionados"].pop()
            bad["escopo"]["total_placeholders_de_saida"] -= 1
            with self.subTest(study=study_id), self.assertRaisesRegex(
                    ValueError, "cobertura de arquivos incompleta"):
                validate(bad, self.tree, self.prov)

    def test_comandos_inventados_rejeitados_por_fonte_literal(self):
        for study_id in ("caes_raca_f5", "mortalidade_sp_0_74"):
            bad = copy.deepcopy(self.data)
            study = next(s for s in bad["estudos"] if s["id"] == study_id)
            study["comandos_documentados"][0]["comando"] = "echo comando_inventado_T06_1"
            with self.subTest(study=study_id), self.assertRaisesRegex(
                    ValueError, "comando literal não encontrado"):
                validate(bad, self.tree, self.prov)


if __name__ == "__main__":
    unittest.main()
