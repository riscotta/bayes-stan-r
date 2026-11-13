"""Contratos N1 da T06.5. Não lê dados brutos nem executa modelos."""
from __future__ import annotations

import copy
import json
import subprocess
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
T065 = ROOT / "reports/t06_5/dependencias_externas_T06_5.json"
T061 = ROOT / "reports/t06_1/manifesto_estudos_T06_1.json"
EXPECTED_OBJECTIVE = "Registrar dependências ausentes dos estudos com dados externos, incluindo os caminhos históricos."
BASE_GIT_SHA = "f5f782d19bebc0a09ef08831982d601569e33d95"

# Fonte independente: pares (trecho literal do README, referência completa inventariada).
# Não derivar estes pares da contagem nem do próprio JSON que está sendo validado.
DOCUMENTED_REQUIREMENTS = {
    "mortalidade_sp_0_74": (
        ("E07_R_validacao_anual.csv", "E07_R_validacao_anual.csv"),
        ("E07_R_diagnosticos_ajuste.csv", "E07_R_diagnosticos_ajuste.csv"),
        ("E08_incerteza_cobertura_bootstrap_distrito.csv", "E08_incerteza_cobertura_bootstrap_distrito.csv"),
    ),
    "pnadc_estudo_i": (
        ("e08/diagnosticos_e081.csv", "e08/diagnosticos_e081.csv"),
    ),
    "censo_escolar_tempo_integral": tuple(
        (f"https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_{year}{'_' if year == 2025 else ''}.zip",
         f"https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_{year}{'_' if year == 2025 else ''}.zip")
        for year in range(2021, 2026)
    ),
    "pnadc_desocupacao": (
        ("pnadc_mensal_taxa_desocupacao_jan_2026.csv", "data/raw/pnadc_desocupacao/pnadc_mensal_taxa_desocupacao_jan_2026.csv"),
    ),
    "prisons_prisoners_unodc": (
        ("model_inputs/base_analitica_nucleo_nacional.csv", "data/raw/prisons_prisoners_unodc/model_inputs/base_analitica_nucleo_nacional.csv"),
        ("stan_data_M1.json", "data/raw/prisons_prisoners_unodc/model_inputs/stan_data_M1.json"),
        ("stan_data_M2.json", "data/raw/prisons_prisoners_unodc/model_inputs/stan_data_M2.json"),
        ("stan_data_M3.json", "data/raw/prisons_prisoners_unodc/model_inputs/stan_data_M3.json"),
    ),
}


def load(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def git_paths(ref: str) -> set[str]:
    out = subprocess.check_output(
        ["git", "ls-tree", "-r", "--name-only", ref],
        cwd=ROOT, text=True
    )
    return set(out.splitlines())


def expected_git_state(path: str, tracked: set[str]) -> str:
    if path.startswith("https://"):
        return "URL_EXTERNA_NAO_TESTADA"
    if path in tracked:
        return "ARQUIVO_VERSIONADO_NESTE_SNAPSHOT"
    prefix = path if path.endswith("/") else path + "/"
    if any(p.startswith(prefix) for p in tracked):
        return "DIRETORIO_COM_ENTRADAS_VERSIONADAS_NESTE_SNAPSHOT"
    if "/" not in path or path.startswith(("estudo_I/", "e08/")):
        return "REFERENCIA_RELATIVA_OU_NOME_NAO_VERIFICADO_NO_GIT"
    return "NAO_LOCALIZADO_NA_ARVORE_GIT_DO_SNAPSHOT"


def errors(inventory: dict, manifest: dict, tracked: set[str]) -> list[str]:
    bad = []
    studies = inventory.get("estudos", [])
    originals = manifest.get("estudos", [])
    indexes = {s["id"]: s for s in originals}
    ids = [s.get("id") for s in studies]
    if len(originals) != 33 or len(ids) != 33 or len(set(ids)) != 33:
        bad.append("cobertura duplicada/omissa")
    if set(ids) != set(indexes):
        bad.append("ids não coincidem com manifesto")
    if inventory.get("objetivo_literal") != EXPECTED_OBJECTIVE:
        bad.append("objetivo diferente")
    if not str(inventory.get("base_main_git_sha", "")).isalnum():
        bad.append("SHA base ausente")
    if inventory.get("base_main_git_sha") != BASE_GIT_SHA:
        bad.append("snapshot base não corresponde ao SHA auditado")
    total_cat, total_gap, historical_count = 0, 0, 0
    for row in studies:
        sid = row.get("id")
        if sid not in indexes:
            continue
        source = indexes[sid]
        if row.get("readme") != source["documentacao_de_estudo"]:
            bad.append(f"{sid}: README incorreto")
        deps = row.get("dependencias", [])
        catalog = [d for d in deps if d.get("dataset_id") is not None]
        gaps = [d for d in deps if d.get("dataset_id") is None]
        if len(catalog) != len(source["entradas"]):
            bad.append(f"{sid}: entradas catalogadas omitidas")
        if len(gaps) != len(source["lacunas_entradas_externas_nao_catalogadas"]):
            bad.append(f"{sid}: referências não catalogadas omitidas")
        by_id = {e["dataset_id"]: e for e in source["entradas"]}
        if set(d.get("dataset_id") for d in catalog) != set(by_id):
            bad.append(f"{sid}: dataset_ids incorretos")
        for dep in catalog:
            exp = by_id.get(dep.get("dataset_id"))
            if exp is None:
                continue
            for key, val in (
                ("caminho_catalogado", exp["escopo_caminho"]),
                ("fonte_documentada", exp["fonte_declarada"]),
                ("url_origem_declarada", exp["origem_url"]),
                ("licenca_status_historico", exp["license_status_fonte"]),
                ("redistribuicao_status_historico", exp["redistribution_status_fonte"]),
                ("quantidade_referencias_git_manifesto", len(exp["arquivos_git"])),
            ):
                if dep.get(key) != val:
                    bad.append(f"{sid}: {key} divergente")
            p = exp["escopo_caminho"]
            if dep.get("estado_caminho_catalogado_git") != expected_git_state(p, tracked):
                bad.append(f"{sid}: status Git de {p} inconsistente")
        expected_gaps = [e["referencia_readme"] for e in source["lacunas_entradas_externas_nao_catalogadas"]]
        if [d.get("fonte_documentada") for d in gaps] != expected_gaps:
            bad.append(f"{sid}: lacuna README divergente")
        for dep in deps:
            if dep.get("permissao_homologada_t065") is not False:
                bad.append(f"{sid}: falsa homologação")
            if not dep.get("lacuna_documental") or not dep.get("fonte_evidencia"):
                bad.append(f"{sid}: documentação de lacuna insuficiente")
        historical = row.get("caminhos_ou_referencias_historicas_documentadas", [])
        historical_count += len(historical)
        historical_names = [h.get("referencia_literal") for h in historical]
        if len(historical_names) != len(set(historical_names)):
            bad.append(f"{sid}: referências históricas duplicadas")
        requirements = DOCUMENTED_REQUIREMENTS.get(sid, ())
        readme_path = ROOT / row["readme"]
        try:
            documentary_source = readme_path.read_text(encoding="utf-8")
        except OSError as exc:
            bad.append(f"{sid}: README inacessível: {exc}")
            documentary_source = ""
        for source_literal, inventory_literal in requirements:
            if source_literal not in documentary_source:
                bad.append(f"{sid}: fonte README não contém {source_literal}")
            if inventory_literal not in historical_names:
                bad.append(f"{sid}: referência da fonte documental ausente: {inventory_literal}")
        for h in historical:
            if h.get("evidencia_documental") not in {row["readme"], "data/DATA_PROVENANCE.csv"}:
                bad.append(f"{sid}: fonte da referência histórica inválida")
            if h.get("evidencia_documental") == "data/DATA_PROVENANCE.csv" and sid != "mortality":
                bad.append(f"{sid}: fonte de catálogo atribuída indevidamente")
            if h.get("estado_no_git") != expected_git_state(h["referencia_literal"], tracked):
                bad.append(f"{sid}: status Git de caminho divergente")
            if h.get("presenca_no_ambiente_do_usuario") != "NAO_VERIFICADA":
                bad.append(f"{sid}: inventou disponibilidade local")
            if h.get("integridade_e_direitos") != "NAO_HOMOLOGADOS":
                bad.append(f"{sid}: direitos indevidos")
        if row.get("estado_cobertura") != (
            "DEPENDENCIA_EXTERNA_DOCUMENTADA" if deps else "SEM_DEPENDENCIA_EXTERNA_IDENTIFICADA_NO_RECORTE"
        ):
            bad.append(f"{sid}: estado de cobertura incoerente")
        total_cat += len(catalog)
        total_gap += len(gaps)
    summary = inventory.get("resumo", {})
    expected = {
        "total_estudos": len(originals),
        "estudos_com_dependencias": sum(bool(s["entradas"] or s["lacunas_entradas_externas_nao_catalogadas"]) for s in originals),
        "vinculos_catalogados": total_cat,
        "lacunas_sem_catalogo": total_gap,
        "estudos_sem_vinculo_identificado": [
            s["id"] for s in originals if not s["entradas"] and not s["lacunas_entradas_externas_nao_catalogadas"]
        ],
        "caminhos_documentados": historical_count,
    }
    if summary != expected:
        bad.append("totais calculados incoerentes")
    return bad


class TestDependenciasExternasT065(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.inventory = load(T065)
        cls.manifest = load(T061)
        # Snapshot N1 imutável: HEAD do PR/merge e pushes futuros não alteram a base.
        cls.tracked = git_paths(BASE_GIT_SHA)

    def test_identidade_base_e_contrato(self):
        self.assertEqual(self.inventory["tarefa"], "T06.5")
        self.assertEqual(self.inventory["base_main_git_sha"], BASE_GIT_SHA)
        resolved = subprocess.check_output(
            ["git", "rev-parse", "--verify", f"{BASE_GIT_SHA}^{{commit}}"],
            cwd=ROOT, text=True
        ).strip()
        self.assertEqual(resolved, BASE_GIT_SHA)
        self.assertEqual(self.inventory["natureza"], "INVENTARIO_DOCUMENTAL_DE_LACUNAS_SEM_LEITURA_DE_DADOS_EXTERNOS")
        self.assertTrue((ROOT / "reports/t06_5/README.md").is_file())

    def test_cobertura_reconciliada(self):
        self.assertEqual(errors(self.inventory, self.manifest, self.tracked), [])

    def test_vinculos_e_lacunas_exatas(self):
        self.assertEqual(self.inventory["resumo"]["vinculos_catalogados"], 27)
        self.assertEqual(self.inventory["resumo"]["lacunas_sem_catalogo"], 8)
        self.assertEqual(self.inventory["resumo"]["estudos_com_dependencias"], 30)

    def test_especificidades_historicas(self):
        by_id = {s["id"]: s for s in self.inventory["estudos"]}
        def hints(study):
            return {r["referencia_literal"] for r in by_id[study]["caminhos_ou_referencias_historicas_documentadas"]}
        self.assertIn("base_F5_principal_E04.csv", " ".join(hints("caes_raca_f5")))
        self.assertIn("crosswalk_distritos_PROVISORIO.csv", hints("mortalidade_sp_0_74"))
        self.assertIn("estudo_I/e04/saida", hints("pnadc_estudo_i"))
        self.assertIn("data/raw/rs_seguro/rs_month_crime.csv", hints("rs_seguro"))
        self.assertIn("data/raw/consumer_shopping_trends/Consumer_Shopping_Trends_2026 (6).csv", hints("consumer_shopping_trends_beta"))
        self.assertIn("data/raw/retractions_time_to_retraction/global_scientific_retractions_1927_2026.csv", hints("retractions_trend"))

    def test_mutacao_entrada_removida_reprovada(self):
        damaged = copy.deepcopy(self.inventory)
        affected = next(s for s in damaged["estudos"] if s["id"] == "rs_seguro")
        affected["dependencias"].pop()
        self.assertTrue(errors(damaged, self.manifest, self.tracked))

    def test_mutacao_caminho_e_licenca_reprovadas(self):
        damaged = copy.deepcopy(self.inventory)
        affected = next(s for s in damaged["estudos"] if s["id"] == "isus_sia")
        affected["dependencias"][0]["caminho_catalogado"] = "data/raw/arquivo_inventado.csv"
        affected["dependencias"][0]["permissao_homologada_t065"] = True
        self.assertTrue(any("falsa homologação" in x for x in errors(damaged, self.manifest, self.tracked)))

    def test_estado_git_nao_confundido_com_inexistencia(self):
        by_id = {s["id"]: s for s in self.inventory["estudos"]}
        hints = by_id["caes_raca_f5"]["caminhos_ou_referencias_historicas_documentadas"]
        self.assertIn("NAO_LOCALIZADO_NA_ARVORE_GIT_DO_SNAPSHOT", {x["estado_no_git"] for x in hints})
        for s in self.inventory["estudos"]:
            self.assertIn("não significa inexistente", s["observacao"].lower())

    def test_cobertura_literais_readmes_independente_do_json(self):
        self.assertEqual(errors(self.inventory, self.manifest, self.tracked), [])
        for sid, requirements in DOCUMENTED_REQUIREMENTS.items():
            doc = (ROOT / next(s["readme"] for s in self.inventory["estudos"] if s["id"] == sid)).read_text(encoding="utf-8")
            refs = {h["referencia_literal"] for s in self.inventory["estudos"] if s["id"] == sid
                    for h in s["caminhos_ou_referencias_historicas_documentadas"]}
            for in_readme, in_inventory in requirements:
                with self.subTest(study=sid, reference=in_inventory):
                    self.assertIn(in_readme, doc)
                    self.assertIn(in_inventory, refs)

    def test_mutacoes_retirada_e_renomeacao_referencias_reprovadas(self):
        for sid, requirements in DOCUMENTED_REQUIREMENTS.items():
            for _, literal in requirements:
                with self.subTest(study=sid, reference=literal):
                    damaged = copy.deepcopy(self.inventory)
                    row = next(s for s in damaged["estudos"] if s["id"] == sid)
                    row["caminhos_ou_referencias_historicas_documentadas"] = [
                        h for h in row["caminhos_ou_referencias_historicas_documentadas"]
                        if h["referencia_literal"] != literal
                    ]
                    damaged["resumo"]["caminhos_documentados"] -= 1
                    failures = errors(damaged, self.manifest, self.tracked)
                    self.assertTrue(any("fonte documental ausente" in f for f in failures), failures)
                    changed = copy.deepcopy(self.inventory)
                    renamed = next(s for s in changed["estudos"] if s["id"] == sid)
                    target = next(h for h in renamed["caminhos_ou_referencias_historicas_documentadas"]
                                  if h["referencia_literal"] == literal)
                    target["referencia_literal"] = "referencia_inventada_N1.csv"
                    failures = errors(changed, self.manifest, self.tracked)
                    self.assertTrue(any("fonte documental ausente" in f for f in failures), failures)

    def test_git_snapshot_imutavel_com_hipotetica_adicao_futura_em_head(self):
        # Simula HEAD futura com novo arquivo sem alterar a árvore historicamente ancorada.
        future_path = "data/raw/SAheart.data"
        self.assertNotIn(future_path, self.tracked)
        self.assertEqual(expected_git_state(future_path, self.tracked),
                         "NAO_LOCALIZADO_NA_ARVORE_GIT_DO_SNAPSHOT")
        future_head = self.tracked | {future_path}
        self.assertEqual(expected_git_state(future_path, future_head),
                         "ARQUIVO_VERSIONADO_NESTE_SNAPSHOT")
        self.assertEqual(errors(self.inventory, self.manifest, self.tracked), [])
        self.assertTrue(any("saheart: status Git" in msg
                            for msg in errors(self.inventory, self.manifest, future_head)))
        self.assertEqual(git_paths(BASE_GIT_SHA), self.tracked)

    def test_urls_inep_documentadas_sem_homologacao(self):
        s = next(s for s in self.inventory["estudos"] if s["id"] == "censo_escolar_tempo_integral")
        by_name = {h["referencia_literal"]: h for h in s["caminhos_ou_referencias_historicas_documentadas"]}
        for year in range(2021, 2026):
            url = f"https://download.inep.gov.br/dados_abertos/microdados_censo_escolar_{year}{'_' if year == 2025 else ''}.zip"
            with self.subTest(ano=year):
                self.assertEqual(by_name[url]["estado_no_git"], "URL_EXTERNA_NAO_TESTADA")
                self.assertEqual(by_name[url]["integridade_e_direitos"], "NAO_HOMOLOGADOS")

    def test_sem_inferencia_ou_niveis_elevados(self):
        self.assertNotIn("HOMOLOGADO", str(self.inventory["resumo"]))
        self.assertIn("T06.5 não permite promoção para N2/N3/N4 ou fechamento da Etapa 06", self.inventory["gates"])


if __name__ == "__main__":
    unittest.main()
