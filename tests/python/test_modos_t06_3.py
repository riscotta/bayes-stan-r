"""T06.3: contratos de modos; valida apenas metadados/código público, não N3/N4."""
import copy
import json
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DOC = ROOT / "reports/t06_3/modos_operacionais_T06_3.json"
README = ROOT / "reports/t06_3/README.md"
M1 = ROOT / "reports/t06_1/manifesto_estudos_T06_1.json"
M2 = ROOT / "reports/t06_2/estados_reprodutibilidade_T06_2.json"
CI = ROOT / ".github/workflows/ci.yml"
BASE = "f80343fdbb2d65ca7b726aefd919c611013c93df"
MODES = ("auditoria_rapida", "reexecucao_modelo", "reproducao_desde_dados")
CASES = ("pnadc_estudo_i", "mortalidade_sp_0_74", "caes_raca_f5")
COMMANDS = {
    "pnadc_estudo_i": {
        "auditoria_rapida": "python scripts/pnadc_estudo_i/auditar_e10.py",
        "reexecucao_modelo": None,
    },
    "mortalidade_sp_0_74": {
        "auditoria_rapida": "Rscript --vanilla scripts/mortalidade_sp_0_74/reproduzir_E10.R --modo=auditar",
        "reexecucao_modelo": "Rscript --vanilla scripts/mortalidade_sp_0_74/reproduzir_E10.R --modo=completo",
    },
    "caes_raca_f5": {
        "auditoria_rapida": "Rscript scripts/caes_raca_f5/caes_raca_f5_cmdstanr.R",
        "reexecucao_modelo": "Rscript scripts/caes_raca_f5/caes_raca_f5_cmdstanr.R --run_stan=1",
    },
}
POSITIVE = {"pnadc_estudo_i", "mortalidade_sp_0_74"}


def require(condition, message):
    if not condition:
        raise ValueError(message)


def validate(document, manifest, states, docs):
    require(document.get("schema_version") == "1.0.0", "schema")
    require(document.get("tarefa") == "T06.3", "id")
    require(document.get("base_main_sha") == BASE, "base")
    require(document.get("natureza") ==
            "CONTRATO_DOCUMENTAL_NAO_EXECUTA_AMOSTRAGEM_OU_DADOS_ORIGINAIS", "natureza")
    modes = document.get("modos")
    require(isinstance(modes, dict) and set(modes) == set(MODES), "modos")
    expected_levels = {
        "auditoria_rapida": "N1_N2_LIMITADOS",
        "reexecucao_modelo": "N3_SOMENTE_COM_EVIDENCIA_INDEPENDENTE",
        "reproducao_desde_dados": "N4_SOMENTE_COM_EVIDENCIA_INDEPENDENTE",
    }
    for mode in MODES:
        item = modes[mode]
        require(item.get("nivel_possivel") == expected_levels[mode], "nivel " + mode)
        require(item.get("execucao_n3_n4_na_t06_3") is False, "execucao " + mode)
        require(isinstance(item.get("parada"), str) and len(item["parada"]) >= 60,
                "parada " + mode)
        require(isinstance(item.get("condicao"), str) and len(item["condicao"]) >= 60,
                "condicao " + mode)
    require(len(manifest["estudos"]) == len(states["estudos"]) == 33, "fontes 33")
    by_manifest = {s["id"]: s for s in manifest["estudos"]}
    by_state = {s["id"]: s for s in states["estudos"]}
    studies = document.get("estudos")
    require(isinstance(studies, list) and len(studies) == 33, "33 estudos")
    require(len(by_manifest) == len(by_state) == 33, "IDs únicos das fontes")
    require(len(set(s.get("id") for s in studies)) == 33, "IDs únicos T06.3")
    require(set(by_manifest) == set(by_state) ==
            {s["id"] for s in studies}, "cobertura integral")
    for study in studies:
        sid = study["id"]
        require(study.get("fonte_estudo") == by_manifest[sid]["documentacao_de_estudo"] ==
                by_state[sid]["fonte_estudo"], "fonte do estudo " + sid)
        require((ROOT / study["fonte_estudo"]).is_file(), "README inexistente " + sid)
        synth = by_state[sid]["estados"]["sintese"]["estado"]
        expected = ("AUDITORIA_DESCRITIVA_LIMITADA_DOCUMENTADA" if sid in POSITIVE
                    else "NAO_AFERIDO_COM_EVIDENCIA_N2_NESTE_CONTRATO")
        require(study.get("auditoria_rapida") == expected, "auditoria promovida " + sid)
        if sid in POSITIVE:
            require(synth == "AUDITORIA_DESCRITIVA_LIMITADA_DOCUMENTADA",
                    "T06.2 sem fundamento N2 " + sid)
        else:
            require(synth == "NAO_AFERIDO_NESTA_TAREFA",
                    "contradicao T06.2 " + sid)
        for key in ("reexecucao_modelo", "reproducao_desde_dados"):
            require(study.get(key) == "NAO_AFERIDA_NESTA_TAREFA",
                    "N3/N4 indevido " + sid)
        require(study.get("execucao_n3_n4_t06_3") is False, "falso N3/N4 " + sid)
        require(by_state[sid]["estados"]["inferencia"]["estado"] ==
                "NAO_AFERIDO_NESTA_TAREFA", "T06.2 N3 ja afirmado")
        require(by_state[sid]["estados"]["pipeline"]["estado"] ==
                "NAO_AFERIDO_NESTA_TAREFA", "T06.2 pipeline ja afirmado")
    routes = document.get("rotas_documentadas")
    require(isinstance(routes, dict) and set(routes) == set(CASES), "rotas")
    for sid in CASES:
        routes_by_mode = routes[sid]
        require(set(routes_by_mode) == set(MODES), "rota incompleta " + sid)
        readme = docs[sid]
        for mode in ("auditoria_rapida", "reexecucao_modelo"):
            cmd = routes_by_mode[mode].get("comando")
            require(cmd == COMMANDS[sid][mode], "comando errado " + sid + " " + mode)
            if cmd is not None:
                require(cmd in readme, "comando nao documentado " + sid)
        require(routes_by_mode["reproducao_desde_dados"].get("comando") is None,
                "N4 inventado " + sid)
        require(routes_by_mode["reproducao_desde_dados"].get("situacao") ==
                "NAO_DEMONSTRADO", "N4 falsamente aprovado " + sid)
    require(routes["pnadc_estudo_i"]["reexecucao_modelo"]["situacao"] ==
            "INSUMOS_E07_E08_HISTORICOS_NAO_PRESENTES", "PNAD N3")
    require(routes["mortalidade_sp_0_74"]["reexecucao_modelo"]["situacao"] ==
            "CODIGO_IDENTIFICADO_NAO_EXECUTADO_T06_3", "mortalidade N3")
    require(routes["caes_raca_f5"]["reexecucao_modelo"]["situacao"] ==
            "CODIGO_IDENTIFICADO_NAO_EXECUTADO_T06_3", "caes N3")
    require(routes["caes_raca_f5"]["auditoria_rapida"]["situacao"] ==
            "CODIGO_PUBLICO_DOCUMENTADO_SEM_NOVA_CI_ESPECIFICA_T06_3", "caes N2")
    require("issue #14 P0" in " ".join(document.get("gates_abertos", [])),
            "P0 omitida")
    require("issues #15 e #16" in " ".join(document.get("gates_abertos", [])),
            "issues secundarias")
    require("N1/N2 não certificam N3/N4" in document.get("nao_implica", []),
            "limite inferencial")
    for name in ("Auditoria rápida", "Reexecução de modelo", "Reprodução desde dados"):
        require(name in docs["t06_3"], "README ausente " + name)
    require("N4" in docs["t06_3"] and "não" in docs["t06_3"],
            "limite N4 indisponivel")
    require("test_modos_t06_3.py" in docs["ci"], "CI nao chama T06.3")
    require("--modo=auditar" in docs["script_mortalidade"] and
            "--modo=completo" in docs["script_mortalidade"] and
            "--modo=conferir" in docs["script_mortalidade"], "flags nao implementadas")
    require("SHA256 =" in docs["script_pnad"], "auditoria PNAD sem checksum")
    return True


class TestT063(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.record = json.loads(DOC.read_text(encoding="utf-8"))
        cls.manifest = json.loads(M1.read_text(encoding="utf-8"))
        cls.states = json.loads(M2.read_text(encoding="utf-8"))
        cls.docs = {
            sid: (ROOT / "scripts" / sid / "README.md").read_text(encoding="utf-8")
            for sid in CASES
        }
        cls.docs["t06_3"] = README.read_text(encoding="utf-8")
        cls.docs["ci"] = CI.read_text(encoding="utf-8")
        cls.docs["script_mortalidade"] = (
            ROOT / "scripts/mortalidade_sp_0_74/reproduzir_E10.R"
        ).read_text(encoding="utf-8")
        cls.docs["script_pnad"] = (
            ROOT / "scripts/pnadc_estudo_i/auditar_e10.py"
        ).read_text(encoding="utf-8")

    def test_contrato_e_33_estudos(self):
        self.assertTrue(validate(self.record, self.manifest, self.states, self.docs))

    def mutate(self, fn):
        changed = copy.deepcopy(self.record)
        fn(changed)
        with self.assertRaises(ValueError):
            validate(changed, self.manifest, self.states, self.docs)

    def test_rejeita_estudo_omitido(self):
        self.mutate(lambda d: d["estudos"].pop())

    def test_rejeita_falsa_certificacao_n2(self):
        self.mutate(lambda d: next(x for x in d["estudos"]
                    if x["id"] == "caes_raca_f5").update(
                    {"auditoria_rapida": "AUDITORIA_DESCRITIVA_LIMITADA_DOCUMENTADA"}))

    def test_rejeita_n3_autodeclarado(self):
        self.mutate(lambda d: d["estudos"][0].update(
                    {"reexecucao_modelo": "EXECUTADA_APROVADA"}))

    def test_rejeita_n4_autodeclarado(self):
        self.mutate(lambda d: d["estudos"][0].update(
                    {"reproducao_desde_dados": "REPRODUZIDA"}))

    def test_rejeita_amostragem_fingida(self):
        self.mutate(lambda d: d["modos"]["reexecucao_modelo"].update(
                    {"execucao_n3_n4_na_t06_3": True}))

    def test_rejeita_modo_faltante(self):
        self.mutate(lambda d: d["modos"].pop("reproducao_desde_dados"))

    def test_rejeita_comando_de_conferencia_como_reexecucao(self):
        self.mutate(lambda d: d["rotas_documentadas"]["mortalidade_sp_0_74"]
                    ["reexecucao_modelo"].update({"comando":
                    "Rscript --vanilla scripts/mortalidade_sp_0_74/reproduzir_E10.R --modo=conferir"}))

    def test_rejeita_comando_n4_inventado(self):
        self.mutate(lambda d: d["rotas_documentadas"]["pnadc_estudo_i"]
                    ["reproducao_desde_dados"].update({"comando": "Rscript fantasia.R"}))

    def test_rejeita_saida_do_gate_p0(self):
        self.mutate(lambda d: d.update({"gates_abertos": ["Sem pendencias"]}))

    def test_rejeita_sem_criterio_de_parada(self):
        self.mutate(lambda d: d["modos"]["auditoria_rapida"].update({"parada": ""}))

    def test_rejeita_comando_documental_falso(self):
        self.mutate(lambda d: d["rotas_documentadas"]["caes_raca_f5"]
                    ["reexecucao_modelo"].update({"comando": "Rscript --fake-stan"}))


if __name__ == "__main__":
    unittest.main()
