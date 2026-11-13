"""T06.4: política e casos sintéticos adversariais, sem dados nem sampling."""
import copy
import hashlib
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
POLICY = ROOT / "reports/t06_4/politica_resultados_T06_4.json"
README = ROOT / "reports/t06_4/README.md"
SCRIPT = ROOT / ".github/scripts/reconciliar_resultados_t06_4.py"
MANIFEST = ROOT / "reports/t06_1/manifesto_estudos_T06_1.json"
STATES = ROOT / "reports/t06_2/estados_reprodutibilidade_T06_2.json"
MODES = ROOT / "reports/t06_3/modos_operacionais_T06_3.json"
CI = ROOT / ".github/workflows/ci.yml"
BASE = "34f34e83c4918cbb885d51647d5d0b2b785796bb"

spec = importlib.util.spec_from_file_location("reconciliar_t064", SCRIPT)
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)


def digest(data):
    return hashlib.sha256(data).hexdigest()


def validate_policy(p, m1, m2, m3, docs):
    if p.get("tarefa") != "T06.4" or p.get("base_main_sha") != BASE:
        raise ValueError("tarefa/base adulterada")
    if p.get("schema_version") != "1.0.0" or not p.get("natureza", "").startswith("POLITICA_"):
        raise ValueError("natureza/schema")
    if set(p.get("fontes", [])) != {"reports/t06_1/manifesto_estudos_T06_1.json",
                                     "reports/t06_2/estados_reprodutibilidade_T06_2.json",
                                     "reports/t06_3/modos_operacionais_T06_3.json"}:
        raise ValueError("fontes trocadas")
    ids1 = [e["id"] for e in m1["estudos"]]
    ids2 = [e["id"] for e in m2["estudos"]]
    ids3 = [e["id"] for e in m3["estudos"]]
    if len(ids1) != 33 or len(set(ids1)) != 33 or set(ids1) != set(ids2) or set(ids1) != set(ids3):
        raise ValueError("cobertura dos 33 estudos adulterada")
    if p.get("cobertura") != {"estudos": 33,
                             "fonte_ids": "reports/t06_1/manifesto_estudos_T06_1.json",
                             "aplicacao": "POLITICA_GERAL_SEM_CERTIFICACAO_INDIVIDUAL"}:
        raise ValueError("cobertura exagerada")
    if p.get("preservacao", {}).get("referencia") != "CONGELADA_NAO_SOBRESCREVER" or \
       p["preservacao"].get("candidato_prefixo") != "outputs/_reexecucoes_t06_4/<estudo>/<execucao_id>/" or \
       p["preservacao"].get("hash") != "SHA256_BYTES_REAIS_PARA_CADA_ARTEFATO":
        raise ValueError("preservação alterada")
    classes = p.get("classes", {})
    if set(classes) != mod.TIPOS or any(x.get("resultado_maximo") not in
                                     {"CONFERENCIA_TECNICA", "PENDENTE_QA"} for x in classes.values()) or \
       classes["csv_estocastico_resumo"]["resultado_maximo"] != "PENDENTE_QA":
        raise ValueError("classe promove inferência")
    if p.get("tolerancias", {}).get("deterministicas") != "PADRAO_EXATO_SEM_LIMIAR_IMPLICITO" or \
       p["tolerancias"].get("nao_zero") != "APENAS_COM_PROTOCOLO_PREAPROVADO_JUSTIFICATIVA_MATEMATICA_CALIBRACAO_QA":
        raise ValueError("tolerância sem gate")
    sec = p.get("seguranca", {})
    if sec.get("issue_14_p0") != "ABERTA_SEM_HOMOLOGACAO_DE_DIREITOS" or \
       sec.get("issues_15_16") != "ABERTAS" or \
       sec.get("n3_n4_exigem_direitos") is not True or \
       sec.get("execucao_mcmc_nesta_tarefa") is not False or \
       sec.get("modifica_dados_ou_resultados") is not False or \
       sec.get("qa_independente_requerido") is not True or \
       sec.get("etapa_06_encerrada") is not False:
        raise ValueError("gate P0 ou auto-QA removido")
    if set(p.get("estudos_exemplo", {})) != {"pnadc_estudo_i", "mortalidade_sp_0_74", "caes_raca_f5"}:
        raise ValueError("estudos críticos omitidos")
    if set(p.get("estados_de_saida", [])) != {"CONFERENCIA_TECNICA", "PENDENTE_QA", "BLOQUEADO", "DIVERGENCIA"}:
        raise ValueError("estados de saída adulterados")
    if "N1/N2 não certificam N3/N4" not in p.get("nao_implica", []):
        raise ValueError("promoção falsa")
    if "test_resultados_t06_4.py" not in docs["ci"] or \
       "reconciliar_resultados_t06_4.py" not in docs["readme"] or \
       "issue #14" not in docs["readme"] or "PENDENTE_QA" not in docs["readme"]:
        raise ValueError("documentação/CI incompletas")
    for sid in ("pnadc_estudo_i", "mortalidade_sp_0_74"):
        estado = next(x for x in m2["estudos"] if x["id"] == sid)
        if estado["estados"]["sintese"]["estado"] != "AUDITORIA_DESCRITIVA_LIMITADA_DOCUMENTADA":
            raise ValueError("fonte N2 alterada")
    if any(x["estados"]["pipeline"]["estado"] != "NAO_AFERIDO_NESTA_TAREFA"
           or x["estados"]["inferencia"]["estado"] != "NAO_AFERIDO_NESTA_TAREFA"
           for x in m2["estudos"]):
        raise ValueError("falsa certificação N3/N4 na fonte")
    return True


class TestContratoT064(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.p = json.loads(POLICY.read_text(encoding="utf-8"))
        cls.m1 = json.loads(MANIFEST.read_text(encoding="utf-8"))
        cls.m2 = json.loads(STATES.read_text(encoding="utf-8"))
        cls.m3 = json.loads(MODES.read_text(encoding="utf-8"))
        cls.docs = {"ci": CI.read_text(encoding="utf-8"),
                    "readme": README.read_text(encoding="utf-8")}

    def test_contrato_com_33_estudos(self):
        self.assertTrue(validate_policy(self.p, self.m1, self.m2, self.m3, self.docs))

    def mutate(self, fn):
        p = copy.deepcopy(self.p)
        fn(p)
        with self.assertRaises(ValueError):
            validate_policy(p, self.m1, self.m2, self.m3, self.docs)

    def test_rejeita_apagar_p0(self):
        self.mutate(lambda p: p["seguranca"].update({"issue_14_p0": "RESOLVIDA"}))

    def test_rejeita_autocertificar_n3(self):
        self.mutate(lambda p: p["classes"]["csv_estocastico_resumo"].update(
            {"resultado_maximo": "N3_APROVADO"}))

    def test_rejeita_sobrescrever_referencia(self):
        self.mutate(lambda p: p["preservacao"].update({"referencia": "ALTERAVEL"}))

    def test_rejeita_tolerancia_universal(self):
        self.mutate(lambda p: p["tolerancias"].update({"deterministicas": "EPS_1e-2"}))

    def test_rejeita_autoqa(self):
        self.mutate(lambda p: p["seguranca"].update({"qa_independente_requerido": False}))

    def test_rejeita_remocao_de_estudo_critico(self):
        self.mutate(lambda p: p["estudos_exemplo"].pop("pnadc_estudo_i"))


class TestComparadorT064(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.reference = "referencias/congelado.csv"
        self.candidate = "outputs/_reexecucoes_t06_4/piloto/run1/novo.csv"
        self.ref_path = self.root / self.reference
        self.cand_path = self.root / self.candidate
        self.ref_path.parent.mkdir(parents=True)
        self.cand_path.parent.mkdir(parents=True)
        self.ref_path.write_text("id,estimando\na,1.50\nb,0\n", encoding="utf-8")
        self.cand_path.write_text("id,estimando\na,1.50\nb,0\n", encoding="utf-8")
        self.record = {
            "schema": "t06_4_execucao_v1", "estudo": "piloto", "execucao_id": "run1",
            "modo": "auditoria_rapida", "tipo": "csv_deterministico",
            "codigo_sha": "a" * 40, "ambiente": "Python stdlib, sintético",
            "comando": "comparador offline (não executa estudo)", "seed": "NAO_APLICAVEL",
            "entrada_sha256": "b" * 64, "direitos": "NAO_HOMOLOGADOS",
            "referencia": {"path": self.reference, "sha256": digest(self.ref_path.read_bytes())},
            "candidato": {"path": self.candidate, "sha256": digest(self.cand_path.read_bytes())},
            "comparacao": {"chaves": ["id"], "colunas": {"estimando": {"abs": "0", "rel": "0"}}}
        }
        # Catálogo sintético fixado ANTES de qualquer mutação de teste.
        self.catalogo_path = self.root / "catalogo_congelado.json"
        self.catalogo_path.write_text(json.dumps({
            "schema": "t06_4_catalogo_v1", "git_commit": "a" * 40,
            "referencias": [{
                "path": self.reference, "sha256": digest(self.ref_path.read_bytes()),
                "git_blob_sha": mod.git_blob_sha(self.ref_path)
            }]
        }), encoding="utf-8")
        self.catalogo_sha256 = digest(self.catalogo_path.read_bytes())

    def check(self, record=None):
        return mod.conferir(self.record if record is None else record, self.root,
                           catalogo=self.catalogo_path,
                           catalogo_sha256_confiavel=self.catalogo_sha256)

    def update_candidate(self, text):
        self.cand_path.write_text(text, encoding="utf-8")
        self.record["candidato"]["sha256"] = digest(self.cand_path.read_bytes())

    def test_exato_sintetico_sem_certificar_n2(self):
        self.assertEqual(self.check()["estado"], "CONFERENCIA_TECNICA")
        self.assertIn("NAO_N3_N4", self.check()["escopo"])

    def test_nao_muda_arquivos(self):
        before = self.ref_path.read_bytes(), self.cand_path.read_bytes()
        self.check()
        self.assertEqual(before, (self.ref_path.read_bytes(), self.cand_path.read_bytes()))

    def test_detecta_referencia_adulterada(self):
        self.ref_path.write_text("id,estimando\na,10\nb,0\n", encoding="utf-8")
        with self.assertRaises(mod.Divergencia):
            self.check()

    def test_detecta_candidato_alterado_apos_registro(self):
        self.cand_path.write_text("id,estimando\na,0\nb,0\n", encoding="utf-8")
        with self.assertRaises(mod.Divergencia):
            self.check()

    def test_detecta_valor_mesmo_com_hash_atualizado(self):
        self.update_candidate("id,estimando\na,1.51\nb,0\n")
        with self.assertRaises(mod.Divergencia):
            self.check()

    def test_detecta_linha_omitida(self):
        self.update_candidate("id,estimando\na,1.50\n")
        with self.assertRaises(mod.Divergencia):
            self.check()

    def test_detecta_chave_duplicada(self):
        self.update_candidate("id,estimando\na,1.50\na,0\n")
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_detecta_coluna_extra(self):
        self.update_candidate("id,estimando,extra\na,1.50,z\nb,0,z\n")
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_rejeita_nao_finito(self):
        self.update_candidate("id,estimando\na,NaN\nb,0\n")
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_rejeita_pasta_nao_isolada(self):
        self.record["candidato"]["path"] = self.reference
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_rejeita_traversal_relativo(self):
        self.record["candidato"]["path"] = "outputs/_reexecucoes_t06_4/piloto/run1/../../../../referencias/congelado.csv"
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_rejeita_mudanca_de_pasta_do_estudo(self):
        self.record["candidato"]["path"] = self.candidate.replace("/piloto/", "/outro/")
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_rejeita_n3_sem_direitos_homologados(self):
        self.record["modo"] = "reexecucao_modelo"
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_rejeita_seed_omitida(self):
        self.record.pop("seed")
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_rejeita_tolerancia_nao_preaprovada(self):
        self.record["comparacao"]["colunas"]["estimando"]["abs"] = "0.1"
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_rejeita_tolerancia_negativa(self):
        self.record["comparacao"]["colunas"]["estimando"]["rel"] = "-0.1"
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_rejeita_oraculo_sem_calibracao_estocastica(self):
        self.record["tipo"] = "csv_estocastico_resumo"
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_comparacao_estocastica_nunca_aprova_inferencia(self):
        self.record["tipo"] = "csv_estocastico_resumo"
        self.record["comparacao"]["preaprovacao"] = {
            "id_protocolo": "synthetic-demo", "responsavel_qa": "responsavel-exemplo",
            "justificativa_matematica": "Oráculo inteiramente sintético para teste da máquina de estados.",
            "evidencia_calibracao": "teste-sintetico-nao-homologado"
        }
        self.record["oraculo_estimandos"] = "estimando demonstrativo"
        self.record["diagnosticos_mcse_ess_rhat"] = "evidencia sintética"
        self.assertEqual(self.check()["estado"], "PENDENTE_QA")

    def test_binario_identico_restringe_escopo(self):
        self.record["tipo"] = "bytes_exatos"
        self.assertEqual(self.check()["escopo"], "IDENTIDADE_BYTES_NAO_N3_N4")

    def test_binario_diferente_bloqueia(self):
        self.record["tipo"] = "bytes_exatos"
        self.update_candidate("id,estimando\na,1.50\nb,0.00\n")
        with self.assertRaises(mod.Divergencia):
            self.check()


    def test_b1_rejeita_symlink_candidato_fora_do_isolamento(self):
        outro = self.root / "referencias/outro_historico.csv"
        outro.write_bytes(self.ref_path.read_bytes())
        self.cand_path.unlink()
        self.cand_path.symlink_to(outro)
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_b1_rejeita_symlink_em_ancestral(self):
        origem = self.cand_path.parent
        destino = self.root / "run1_movido"
        origem.rename(destino)
        origem.symlink_to(destino)
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_b2_rejeita_symlink_referencia_para_temporario(self):
        outro = self.root / "outputs/_reexecucoes_t06_4/piloto/run2/falso.csv"
        outro.parent.mkdir(parents=True)
        outro.write_bytes(self.ref_path.read_bytes())
        self.ref_path.unlink()
        self.ref_path.symlink_to(outro)
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_b2_rejeita_referencia_temporaria_direta(self):
        self.record["referencia"]["path"] = self.candidate
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_b3_rejeita_ancora_ausente(self):
        with self.assertRaises(mod.Bloqueio):
            mod.conferir(self.record, self.root)

    def test_b3_rejeita_catalogo_editado_sem_novo_pin(self):
        catalogo = json.loads(self.catalogo_path.read_text(encoding="utf-8"))
        catalogo["git_commit"] = "b" * 40
        self.catalogo_path.write_text(json.dumps(catalogo), encoding="utf-8")
        with self.assertRaises(mod.Divergencia):
            self.check()

    def test_b3_rejeita_troca_conjunta_da_referencia_e_hash(self):
        novo = "id,estimando\na,9999\nb,0\n"
        self.ref_path.write_text(novo, encoding="utf-8")
        self.update_candidate(novo)
        self.record["referencia"]["sha256"] = digest(self.ref_path.read_bytes())
        with self.assertRaises(mod.Divergencia):
            self.check()

    def test_b3_rejeita_catalogo_com_git_blob_incorreto(self):
        catalogo = json.loads(self.catalogo_path.read_text(encoding="utf-8"))
        catalogo["referencias"][0]["git_blob_sha"] = "0" * 40
        self.catalogo_path.write_text(json.dumps(catalogo), encoding="utf-8")
        self.catalogo_sha256 = digest(self.catalogo_path.read_bytes())
        with self.assertRaises(mod.Divergencia):
            self.check()

    def test_g1_tolerancia_textual_nao_autentica_retorna_pendente(self):
        self.update_candidate("id,estimando\na,9999\nb,0\n")
        self.record["comparacao"]["colunas"]["estimando"]["abs"] = "10000"
        self.record["comparacao"]["preaprovacao"] = {
            "id_protocolo": "inventado",
            "justificativa_matematica": "X" * 40,
            "responsavel_qa": "pessoa_sem_credencial",
            "evidencia_calibracao": "inexistente"
        }
        self.assertEqual(self.check()["estado"], "PENDENTE_QA")

    def test_g1_rejeita_preaprovacao_tipo_invalido(self):
        self.record["comparacao"]["colunas"]["estimando"]["abs"] = "0.1"
        self.record["comparacao"]["preaprovacao"] = {
            "id_protocolo": 123, "justificativa_matematica": "X" * 40,
            "responsavel_qa": "responsavel", "evidencia_calibracao": "ficticia"
        }
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_g1_rejeita_tolerancia_booleano(self):
        self.record["comparacao"]["colunas"]["estimando"]["abs"] = True
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_g2_nao_aceita_direitos_autodeclarados(self):
        self.record["modo"] = "reexecucao_modelo"
        self.record["direitos"] = "HOMOLOGADOS"
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_rejeita_hardlink_entre_referencia_e_candidato(self):
        self.cand_path.unlink()
        self.cand_path.hardlink_to(self.ref_path)
        with self.assertRaises(mod.Bloqueio):
            self.check()


    def preparar_caso_numerico(self, referencia, candidato, abs_tol="0", rel_tol="0"):
        """Fixture sintética: reancora catálogo apenas dentro do teste, nunca em dados reais."""
        self.ref_path.write_text(f"id,estimando\na,{referencia}\nb,0\n", encoding="utf-8")
        self.update_candidate(f"id,estimando\na,{candidato}\nb,0\n")
        self.record["referencia"]["sha256"] = digest(self.ref_path.read_bytes())
        catalogo = json.loads(self.catalogo_path.read_text(encoding="utf-8"))
        catalogo["referencias"][0]["sha256"] = digest(self.ref_path.read_bytes())
        catalogo["referencias"][0]["git_blob_sha"] = mod.git_blob_sha(self.ref_path)
        self.catalogo_path.write_text(json.dumps(catalogo), encoding="utf-8")
        self.catalogo_sha256 = digest(self.catalogo_path.read_bytes())
        self.record["comparacao"]["colunas"]["estimando"] = {
            "abs": abs_tol, "rel": rel_tol
        }
        if abs_tol != "0" or rel_tol != "0":
            self.record["comparacao"]["preaprovacao"] = {
                "id_protocolo": "SINTETICO-NAO-AUTENTICADO",
                "justificativa_matematica":
                    "Fixture sintética para verificar comparação racional exata, sem homologação.",
                "responsavel_qa": "TESTE_SINTETICO",
                "evidencia_calibracao": "APENAS_FIXTURE_SINTETICA"
            }

    def test_b4_29_casas_limite_absoluto_excedido(self):
        self.preparar_caso_numerico("0", "1.0000000000000000000000000001", "1", "0")
        with self.assertRaises(mod.Divergencia):
            self.check()

    def test_b4_29_casas_limite_relativo_excedido(self):
        self.preparar_caso_numerico("1", "2.0000000000000000000000000001", "0", "1")
        with self.assertRaises(mod.Divergencia):
            self.check()

    def test_b4_frenteira_exata_absoluta_pendente(self):
        self.preparar_caso_numerico("0", "1", "1", "0")
        self.assertEqual(self.check()["estado"], "PENDENTE_QA")

    def test_b4_fronteira_exata_relativa_pendente(self):
        self.preparar_caso_numerico("1", "2", "0", "1")
        self.assertEqual(self.check()["estado"], "PENDENTE_QA")

    def test_b4_valor_negativo_excede_limite_relativo(self):
        self.preparar_caso_numerico("-1", "-2.0000000000000000000000000001", "0", "1")
        with self.assertRaises(mod.Divergencia):
            self.check()

    def test_b4_tolerancias_combinadas_na_fronteira(self):
        self.preparar_caso_numerico("1", "2", "0.5", "0.5")
        self.assertEqual(self.check()["estado"], "PENDENTE_QA")

    def test_b4_tolerancias_combinadas_acima_fronteira(self):
        self.preparar_caso_numerico("1", "2.0000000000000000000000000001", "0.5", "0.5")
        with self.assertRaises(mod.Divergencia):
            self.check()

    def test_b4_zero_tolerancia_decimais_longos_iguais(self):
        numero = "1.0000000000000000000000000000000000000001"
        self.preparar_caso_numerico(numero, numero)
        self.assertEqual(self.check()["estado"], "CONFERENCIA_TECNICA")

    def test_b4_expoente_excessivo_bloqueado(self):
        self.preparar_caso_numerico("1", "1e1001")
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_b4_coeficiente_excessivo_bloqueado(self):
        self.preparar_caso_numerico("1", "1" * 513)
        with self.assertRaises(mod.Bloqueio):
            self.check()

    def test_b5_tipos_json_invalidos_bloqueados(self):
        casos = [
            ("modo", [], "modo"),
            ("tipo", [], "tipo"),
            ("direitos", [], "direitos"),
            ("seed_lista", [], "seed"),
            ("seed_objeto", {}, "seed"),
            ("seed_booleano", True, "seed"),
            ("seed_vazia", "", "seed"),
            ("seed_float", 1.0, "seed"),
            ("seed_negativa", -1, "seed"),
            ("seed_excede_uint64", 2**64, "seed"),
            ("chaves_lista_aninhada", [["id"]], "chaves"),
            ("chaves_mistas", ["id", ["id"]], "chaves"),
        ]
        for caso, valor, campo in casos:
            with self.subTest(caso=caso):
                registro = copy.deepcopy(self.record)
                if campo == "chaves":
                    registro["comparacao"]["chaves"] = valor
                else:
                    registro[campo] = valor
                with self.assertRaises(mod.Bloqueio):
                    self.check(record=registro)

    def test_b5_seed_inteira_zero_e_uint64_permitidas(self):
        for valor in (0, "0", 2**64 - 1, str(2**64 - 1), "NAO_APLICAVEL"):
            with self.subTest(valor=valor):
                registro = copy.deepcopy(self.record)
                registro["seed"] = valor
                self.assertEqual(self.check(record=registro)["estado"], "CONFERENCIA_TECNICA")

    def test_b5_cli_erro_controlado_sem_traceback(self):
        import subprocess
        import sys
        for campo, valor in (("modo", []), ("seed", []), ("chaves", [["id"]])):
            with self.subTest(campo=campo):
                registro = copy.deepcopy(self.record)
                if campo == "chaves":
                    registro["comparacao"]["chaves"] = valor
                else:
                    registro[campo] = valor
                arquivo_registro = self.root / "registro_sintetico.json"
                arquivo_registro.write_text(json.dumps(registro), encoding="utf-8")
                processo = subprocess.run(
                    [sys.executable, str(SCRIPT), "--registro", str(arquivo_registro),
                     "--raiz", str(self.root), "--catalogo", str(self.catalogo_path),
                     "--catalogo-sha256-confiavel", self.catalogo_sha256],
                    capture_output=True, text=True, check=False
                )
                self.assertEqual(processo.returncode, 2)
                self.assertEqual(json.loads(processo.stdout)["estado"], "BLOQUEADO")
                self.assertNotIn("Traceback", processo.stderr)


    def cli_tolerancia_json_numerica(self, campo, literal, referencia, candidato,
                                    estado, saida):
        """Usa literal JSON sem aspas, preservando a precisão do token original."""
        import subprocess
        import sys
        self.preparar_caso_numerico(
            referencia, candidato,
            abs_tol="0.5" if campo == "abs" else "0",
            rel_tol="0.5" if campo == "rel" else "0")
        texto = json.dumps(self.record, ensure_ascii=False)
        token = f'"{campo}": "0.5"'
        self.assertIn(token, texto)
        texto = texto.replace(token, f'"{campo}": {literal}', 1)
        arquivo_registro = self.root / "registro_literal_decimal.json"
        arquivo_registro.write_text(texto, encoding="utf-8")
        processo = subprocess.run(
            [sys.executable, str(SCRIPT), "--registro", str(arquivo_registro),
             "--raiz", str(self.root), "--catalogo", str(self.catalogo_path),
             "--catalogo-sha256-confiavel", self.catalogo_sha256],
            capture_output=True, text=True, check=False, timeout=15)
        self.assertEqual(processo.returncode, saida, processo.stdout + processo.stderr)
        self.assertEqual(json.loads(processo.stdout)["estado"], estado)
        self.assertNotIn("Traceback", processo.stderr)

    def test_b6_cli_abs_alta_precisao_json(self):
        self.cli_tolerancia_json_numerica(
            "abs", "0.9999999999999999999999999999", "0", "1", "DIVERGENCIA", 2)

    def test_b6_cli_rel_alta_precisao_json(self):
        self.cli_tolerancia_json_numerica(
            "rel", "0.9999999999999999999999999999", "1", "2", "DIVERGENCIA", 2)

    def test_b6_cli_abs_expoente_pequeno_nao_vira_zero(self):
        self.cli_tolerancia_json_numerica(
            "abs", "1e-400", "0", "0", "PENDENTE_QA", 3)

    def test_b6_cli_rel_expoente_pequeno_nao_vira_zero(self):
        self.cli_tolerancia_json_numerica(
            "rel", "1e-400", "1", "1", "PENDENTE_QA", 3)

    def test_b6_cli_expoente_json_fora_limite_bloqueado(self):
        self.cli_tolerancia_json_numerica(
            "abs", "1e-1001", "0", "0", "BLOQUEADO", 2)

    def test_b7_cli_expoente_extremo_positivo_bloqueado_sem_traceback(self):
        self.cli_tolerancia_json_numerica(
            "abs", "1e10000000000000000000", "0", "0", "BLOQUEADO", 2)

    def test_b7_cli_expoente_extremo_negativo_bloqueado_sem_traceback(self):
        self.cli_tolerancia_json_numerica(
            "rel", "1e-10000000000000000000", "1", "1", "BLOQUEADO", 2)


    def test_b8_cli_csv_campo_excessivo_bloqueado(self):
        import subprocess
        import sys
        # CSV maior que field_size_limit padrão; deve bloquear sem traceback.
        self.update_candidate("id,estimando\na," + "1" * 131073 + "\nb,0\n")
        registro_path = self.root / "registro_csv_campo_excessivo.json"
        registro_path.write_text(json.dumps(self.record), encoding="utf-8")
        processo = subprocess.run(
            [sys.executable, str(SCRIPT), "--registro", str(registro_path),
             "--raiz", str(self.root), "--catalogo", str(self.catalogo_path),
             "--catalogo-sha256-confiavel", self.catalogo_sha256],
            capture_output=True, text=True, check=False, timeout=15)
        self.assertEqual(processo.returncode, 2, processo.stdout + processo.stderr)
        self.assertEqual(json.loads(processo.stdout)["estado"], "BLOQUEADO")
        self.assertNotIn("Traceback", processo.stderr)


    def test_b9_cli_json_aninhado_bloqueado(self):
        import subprocess
        import sys
        registro_json = json.dumps(self.record)
        token = '"chaves": ["id"]'
        self.assertIn(token, registro_json)
        registro_json = registro_json.replace(
            token, '"chaves": ' + '[' * 10000 + '"id"' + ']' * 10000, 1)
        registro_path = self.root / "registro_json_aninhado.json"
        registro_path.write_text(registro_json, encoding="utf-8")
        processo = subprocess.run(
            [sys.executable, str(SCRIPT), "--registro", str(registro_path),
             "--raiz", str(self.root), "--catalogo", str(self.catalogo_path),
             "--catalogo-sha256-confiavel", self.catalogo_sha256],
            capture_output=True, text=True, check=False, timeout=15)
        self.assertEqual(processo.returncode, 2, processo.stdout + processo.stderr)
        self.assertEqual(json.loads(processo.stdout)["estado"], "BLOQUEADO")
        self.assertNotIn("Traceback", processo.stderr)


if __name__ == "__main__":
    unittest.main()
