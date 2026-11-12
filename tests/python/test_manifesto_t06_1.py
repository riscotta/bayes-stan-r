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


if __name__ == "__main__":
    unittest.main()
