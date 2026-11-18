#!/usr/bin/env python3
"""Testes T07.6: somente metadados e mutações sintéticas do inventário."""
from __future__ import annotations

import copy
import importlib.util
import json
from pathlib import Path
import unittest

ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "tools/verificar_pares_t07_6.py"
SPEC = importlib.util.spec_from_file_location("verificador_t07_6", SRC)
MOD = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MOD)


class InventarioT076(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.baseline = json.loads(MOD.INVENTARIO.read_text(encoding="utf-8"))
        cls.runner = MOD.carregar_runner(ROOT)

    def dados(self):
        return copy.deepcopy(self.baseline)

    def falha(self, dados):
        with self.assertRaises(ValueError):
            MOD.verificar(dados, self.runner)

    def test_01_estado_nao_comparavel_transparente(self):
        r = MOD.verificar(self.dados(), self.runner)
        self.assertEqual(r["status"], "PARCIAL_NAO_COMPARAVEL")
        self.assertEqual(r["saidas_previstas"], 3)
        self.assertEqual(r["comparacoes_historicas_efetivas"], 0)

    def test_02_recusa_equivalencia_inventada(self):
        d = self.dados()
        d["equivalencia_historica_demonstrada"] = True
        self.falha(d)

    def test_03_recusa_referencia_nao_autenticada(self):
        d = self.dados()
        d["estudos"][0]["artefatos"][0]["referencia_historica"] = (
            "data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv")
        self.falha(d)

    def test_04_recusa_falso_pass(self):
        d = self.dados()
        d["estudos"][0]["artefatos"][1]["status"] = "PASS"
        self.falha(d)

    def test_05_recusa_entrada_como_saida(self):
        d = self.dados()
        d["estudos"][0]["classificacao_entrada"] = "OUTPUT_PAREADO"
        self.falha(d)

    def test_06_recusa_ativacao_r_historico(self):
        d = self.dados()
        d["estudos"][1]["execucao_permitida"] = True
        self.falha(d)

    def test_07_recusa_saidas_stan_bloqueadas(self):
        d = self.dados()
        d["estudos"][2]["artefatos"] = [{"nome": "cmdstan.csv"}]
        self.falha(d)

    def test_08_recusa_caminho_fora_de_logs(self):
        d = self.dados()
        d["estudos"][0]["artefatos"][2]["candidato"] = "outputs/outro/report.json"
        self.falha(d)

    def test_09_recusa_limite_relaxado_apos_observacao(self):
        d = self.dados()
        d["criterios_globais"]["tolerancia_numerica_deterministica"]["relativa"] = "0.01"
        self.falha(d)

    def test_10_recusa_custodia_nao_comprovada_declarada_como_valida(self):
        d = self.dados()
        d["custodia_independente_referencia"] = "COMPROVADA"
        self.falha(d)

    def test_11_recusa_blob_invalido(self):
        d = self.dados()
        d["estudos"][0]["entrada_git_blob"] = "x" * 40
        self.falha(d)

    def test_12_recusa_estudo_faltante(self):
        d = self.dados()
        d["estudos"].pop()
        self.falha(d)

    def test_13_recusa_ordem_artefatos_adulterada(self):
        d = self.dados()
        d["estudos"][0]["artefatos"].reverse()
        self.falha(d)

    def test_14_recusa_omissao_criterios(self):
        d = self.dados()
        d.pop("criterios_globais")
        self.falha(d)


if __name__ == "__main__":
    unittest.main()
