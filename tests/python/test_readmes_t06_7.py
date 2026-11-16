"""T06.7: contratos documentais, sem dados institucionais, Stan ou execução inferencial."""
from __future__ import annotations

import json
import re
import unittest
from pathlib import Path
from urllib.parse import unquote

ROOT = Path(__file__).resolve().parents[2]
DOCS = [
    "README.md",
    "scripts/README.md",
    "reports/README.md",
    "reports/t06_6/README.md",
    "reports/t06_7/README.md",
]
MANIFEST = ROOT / "reports/t06_1/manifesto_estudos_T06_1.json"
LINK_RE = re.compile(r"(?<!!)\[[^\]]+\]\(([^)]+)\)")


def validate(docs: dict[str, str], paths: set[str], ids: list[str]) -> None:
    if len(ids) != 33 or len(set(ids)) != 33:
        raise ValueError("Os 33 estudos devem possuir IDs distintos")
    for sid in ids:
        local = f"scripts/{sid}/README.md"
        if local not in paths:
            raise ValueError("README de estudo faltante: " + local)
    if any(p not in paths or p not in docs for p in DOCS):
        raise ValueError("Documentação T06.7 incompleta")
    required = {
        "README.md": ("30/30", "N3/N4", "T06.2", "T06.6", "#14"),
        "scripts/README.md": ("33 READMEs", "30/30", "N3/N4", "#14"),
        "reports/README.md": ("não é apenas um placeholder", "T06.6", "T06.7", "#14"),
        "reports/t06_6/README.md": ("30/30", "5e-13", "COMMENTED", "N3", "N4"),
        "reports/t06_7/README.md": ("33 READMEs", "não foi reescrita", "N3", "N4", "#14"),
    }
    for doc, phrases in required.items():
        for phrase in phrases:
            if phrase not in docs[doc]:
                raise ValueError(f"Limite/evidência ausente em {doc}: {phrase}")
        current = ROOT / doc
        for raw in LINK_RE.findall(docs[doc]):
            target = raw.split("#", 1)[0].strip()
            if not target or target.startswith(("https://", "http://", "mailto:")):
                continue
            resolved = (current.parent / unquote(target)).resolve()
            try:
                relative = resolved.relative_to(ROOT).as_posix()
            except ValueError as exc:
                raise ValueError(f"Link fora do repositório: {doc}: {raw}") from exc
            if relative not in paths and not resolved.is_dir():
                raise ValueError(f"Link local inexistente: {doc}: {raw}")


class TestReadmesT067(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.ids = [s["id"] for s in json.loads(MANIFEST.read_text(encoding="utf-8"))["estudos"]]
        cls.docs = {p: (ROOT / p).read_text(encoding="utf-8") for p in DOCS}
        cls.paths = {p.relative_to(ROOT).as_posix() for p in ROOT.rglob("*") if p.is_file()}

    def test_coverage_links_and_evidence_limits(self):
        validate(self.docs, self.paths, self.ids)

    def test_rejects_missing_study(self):
        with self.assertRaisesRegex(ValueError, "33 estudos"):
            validate(self.docs, self.paths, self.ids[:-1])

    def test_rejects_evidence_promotion_by_omission(self):
        altered = dict(self.docs)
        altered["README.md"] = altered["README.md"].replace("N3/N4", "")
        with self.assertRaisesRegex(ValueError, "N3/N4"):
            validate(altered, self.paths, self.ids)

    def test_rejects_missing_provenance_gate(self):
        altered = dict(self.docs)
        altered["reports/t06_6/README.md"] = altered["reports/t06_6/README.md"].replace("COMMENTED", "")
        with self.assertRaisesRegex(ValueError, "COMMENTED"):
            validate(altered, self.paths, self.ids)


if __name__ == "__main__":
    unittest.main()
