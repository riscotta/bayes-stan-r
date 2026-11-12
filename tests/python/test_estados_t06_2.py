"""T06.2 — contratos de cinco estados independentes; sem acesso a dados brutos."""
import copy
import json
import re
import subprocess
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DOC = ROOT / "reports/t06_2/estados_reprodutibilidade_T06_2.json"
SOURCE = ROOT / "reports/t06_1/manifesto_estudos_T06_1.json"
AXES = ("integridade", "sintese", "pipeline", "inferencia", "validacao_cientifica")
BASE = "3cfa372ab260eec21c8474844d46e98d3a4d78e5"
HEAD_REVIEWED = "8c4a73d6ddd05fca7eae17feba52330b6242f54e"
REVIEW = "https://github.com/riscotta/bayes-stan-r/pull/51#pullrequestreview-5875850144"
CI = "https://github.com/riscotta/bayes-stan-r/actions/runs/43234521388"
POSITIVE_SYNTHESIS = {
    "pnadc_estudo_i": (
        "reports/pnadc_estudo_i/e10_matriz_evidencias_reproducao_T04_6.md",
        "Audit published PNAD E09 projection (N2 only)",
    ),
    "mortalidade_sp_0_74": (
        "scripts/mortalidade_sp_0_74/reproduzir_E10.R",
        "Verify published Sao Paulo mortality indicators",
    ),
}


def tracked_paths():
    """Somente caminhos Git: nenhum conteúdo de dataset ou fonte institucional é lido."""
    raw = subprocess.check_output(
        ["git", "ls-tree", "-r", "--name-only", "HEAD"], cwd=ROOT, text=True
    )
    return set(raw.splitlines())


def fail_if(condition, explanation):
    if condition:
        raise ValueError(explanation)


def validate(document, manifest, paths):
    fail_if(document.get("schema_version") != "1.0.0", "schema")
    fail_if(document.get("tarefa") != "T06.2", "tarefa")
    fail_if(document.get("main_base_sha") != BASE, "sha de partida")
    fail_if(document.get("qa_t06_1_head_sha") != HEAD_REVIEWED, "sha QA original")
    fail_if(document.get("base_manifesto_t06_1_sha") != manifest["base_git_commit"], "ancora manifesto")
    fail_if(document.get("semantica", {}).get("eixos") != list(AXES), "eixos independentes")
    fail_if("estado_global" in document or "nivel_maximo" in document, "nivel unico proibido")
    studies = document.get("estudos")
    fail_if(not isinstance(studies, list), "estudos ausentes")
    ids = [x.get("id") for x in studies]
    originals = {x["id"]: x for x in manifest["estudos"]}
    fail_if(len(ids) != 33 or len(set(ids)) != 33 or set(ids) != set(originals), "cobertura")
    required_paths = {"reports/t06_1/manifesto_estudos_T06_1.json"}
    required_paths.update(path for path, _ in POSITIVE_SYNTHESIS.values())
    fail_if(not required_paths <= paths, "referencia versionada ausente")
    for item in studies:
        sid = item["id"]
        fail_if(item.get("fonte_estudo") != originals[sid]["documentacao_de_estudo"],
                "fonte estudo " + sid)
        fail_if(item["fonte_estudo"] not in paths, "README ausente " + sid)
        fail_if(item.get("fonte_manifesto") != "reports/t06_1/manifesto_estudos_T06_1.json",
                "fonte manifesto " + sid)
        fail_if(item.get("status_permissoes") != "PENDENTE_DE_HOMOLOGACAO_P0",
                "homologacao inventada " + sid)
        states = item.get("estados")
        fail_if(not isinstance(states, dict) or set(states) != set(AXES),
                "cinco estados " + sid)
        for axis, state in states.items():
            fail_if(not isinstance(state, dict), "estado inválido " + sid)
            fail_if(set(state) != {"estado", "evidencia", "escopo", "limite"},
                    "campos " + sid + " " + axis)
            fail_if(not isinstance(state["evidencia"], list),
                    "evidencia " + sid + " " + axis)
            fail_if(not isinstance(state["escopo"], str) or len(state["escopo"]) < 35,
                    "escopo " + sid + " " + axis)
            fail_if(not isinstance(state["limite"], str) or len(state["limite"]) < 35,
                    "limite " + sid + " " + axis)
        inv = states["integridade"]
        fail_if(inv["estado"] != "N1_INVENTARIO_E_HASH_GIT_AUDITADOS",
                "integridade incorreta " + sid)
        inv_ev = inv["evidencia"]
        fail_if(len(inv_ev) != 2 or
                not any(e.get("url") == REVIEW and e.get("head_sha") == HEAD_REVIEWED
                        for e in inv_ev) or
                not any(e.get("url") == CI and e.get("main_sha") == BASE
                        for e in inv_ev),
                "auditoria N1 nao ancorada " + sid)
        synth = states["sintese"]
        if sid in POSITIVE_SYNTHESIS:
            expected_path, expected_step = POSITIVE_SYNTHESIS[sid]
            fail_if(synth["estado"] != "AUDITORIA_DESCRITIVA_LIMITADA_DOCUMENTADA",
                    "sintese comprovada omitida " + sid)
            evs = synth["evidencia"]
            fail_if(len(evs) != 2 or
                    not any(e.get("tipo") == "arquivo_versionado" and
                            e.get("caminho") == expected_path for e in evs) or
                    not any(e.get("tipo") == "ci_pos_squash" and e.get("url") == CI and
                            e.get("main_sha") == BASE and e.get("passo") == expected_step
                            for e in evs),
                    "evidencia da sintese adulterada " + sid)
        else:
            fail_if(synth["estado"] != "NAO_AFERIDO_NESTA_TAREFA" or synth["evidencia"],
                    "sintese promovida sem gate " + sid)
        for axis in ("pipeline", "inferencia", "validacao_cientifica"):
            state = states[axis]
            fail_if(state["estado"] != "NAO_AFERIDO_NESTA_TAREFA" or state["evidencia"],
                    "promocao indevida " + sid + " " + axis)
    counts = document.get("cobertura", {})
    fail_if(counts != {
        "estudos": 33,
        "integridade_n1_documental": 33,
        "sintese_auditoria_limitada": 2,
        "pipeline_reatestado": 0,
        "inferencia_reproduzida": 0,
        "validacao_cientifica_homologada": 0,
    }, "totais de estados")
    fail_if("issue #14 P0" not in " ".join(document.get("gates_abertos", [])),
            "P0 removido")
    return True


class TestT062(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.record = json.loads(DOC.read_text(encoding="utf-8"))
        cls.manifest = json.loads(SOURCE.read_text(encoding="utf-8"))
        cls.paths = tracked_paths()

    def test_base_e_cobertura_de_cinco_eixos(self):
        self.assertTrue(validate(self.record, self.manifest, self.paths))

    def mutate(self, name, fn):
        changed = copy.deepcopy(self.record)
        fn(changed)
        with self.assertRaises(ValueError, msg=name):
            validate(changed, self.manifest, self.paths)

    def test_rejeita_estudo_omitido(self):
        self.mutate("omissao", lambda d: d["estudos"].pop())

    def test_rejeita_eixo_faltante(self):
        self.mutate("eixo", lambda d: d["estudos"][0]["estados"].pop("inferencia"))

    def test_rejeita_estado_global_simplificador(self):
        self.mutate("estado global", lambda d: d.update({"nivel_maximo": "N4"}))

    def test_rejeita_validacao_cientifica_inventada(self):
        self.mutate("validacao", lambda d: d["estudos"][0]["estados"]
                    ["validacao_cientifica"].update({"estado": "PASS"}))

    def test_rejeita_pipeline_promovido_por_ci(self):
        self.mutate("pipeline", lambda d: d["estudos"][0]["estados"]
                    ["pipeline"].update({"estado": "REPRODUZIDO"}))

    def test_rejeita_sintese_falsamente_promovida(self):
        self.mutate("sintese inventada", lambda d: d["estudos"][0]["estados"]
                    ["sintese"].update({"estado": "AUDITORIA_DESCRITIVA_LIMITADA_DOCUMENTADA"}))

    def test_rejeita_falsa_ci_de_sintese(self):
        self.mutate("ci falsa", lambda d: next(x for x in d["estudos"]
                    if x["id"] == "pnadc_estudo_i")["estados"]["sintese"]
                    ["evidencia"][1].update({"url": "https://example.com/run-falso"}))

    def test_rejeita_fonte_de_sintese_alterada(self):
        self.mutate("fonte falsa", lambda d: next(x for x in d["estudos"]
                    if x["id"] == "mortalidade_sp_0_74")["estados"]["sintese"]
                    ["evidencia"][0].update({"caminho": "reports/resultado_inventado.csv"}))

    def test_rejeita_licenca_falsamente_homologada(self):
        self.mutate("licenca", lambda d: d["estudos"][0].update(
            {"status_permissoes": "HOMOLOGADA"}))

    def test_rejeita_omissao_de_ressalva(self):
        self.mutate("limite", lambda d: d["estudos"][0]["estados"]
                    ["integridade"].update({"limite": ""}))


if __name__ == "__main__":
    unittest.main()
