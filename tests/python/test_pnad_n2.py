"""Testes adversariais de N2: nenhuma amostra institucional é utilizada."""
from __future__ import annotations

import csv
import hashlib
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/pnadc_estudo_i/executar_auditoria_n2.py"
LEGADO = SCRIPT.with_name("auditar_e10.py")
CSV_PUBLICO = ROOT / "data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv"


class AuditoriaN2CI(unittest.TestCase):
    def executar(self, script: Path = SCRIPT, *, otimizar: bool = False, env=None):
        comando = [sys.executable]
        if otimizar:
            comando.append("-O")
        comando.append(str(script))
        return subprocess.run(
            comando, cwd=ROOT, text=True, capture_output=True, check=False, env=env
        )

    def replica_temporaria(self, pasta: Path) -> tuple[Path, Path, Path]:
        base = pasta / "scripts/pnadc_estudo_i"
        entrada = pasta / "data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv"
        base.mkdir(parents=True)
        entrada.parent.mkdir(parents=True)
        shutil.copyfile(SCRIPT, base / SCRIPT.name)
        shutil.copyfile(LEGADO, base / LEGADO.name)
        shutil.copyfile(CSV_PUBLICO, entrada)
        return base / SCRIPT.name, base / LEGADO.name, entrada

    def test_csv_publico_aprovado_com_mensagem_de_escopo(self):
        resultado = self.executar()
        self.assertEqual(resultado.returncode, 0, resultado.stderr)
        self.assertIn("AUDITORIA DE SÍNTESE: APROVADA", resultado.stdout)
        self.assertIn("ESCOPO N2:", resultado.stdout)
        self.assertIn("N3 ou N4", resultado.stdout)

    def test_python_menos_o_recusado_antes_da_auditoria(self):
        resultado = self.executar(otimizar=True)
        self.assertEqual(resultado.returncode, 2, resultado.stdout + resultado.stderr)
        self.assertIn("desativa os assert", resultado.stderr)
        self.assertNotIn("AUDITORIA DE SÍNTESE: APROVADA", resultado.stdout)

    def test_pythonoptimize_recusado(self):
        ambiente = dict(os.environ, PYTHONOPTIMIZE="1")
        resultado = self.executar(env=ambiente)
        self.assertEqual(resultado.returncode, 2, resultado.stdout + resultado.stderr)
        self.assertIn("N2 FALHOU", resultado.stderr)

    def test_csv_adulterado_reprovado_por_sha(self):
        with tempfile.TemporaryDirectory() as nome:
            script, _, entrada = self.replica_temporaria(Path(nome))
            entrada.write_bytes(entrada.read_bytes() + b"\n")
            resultado = self.executar(script)
        self.assertNotEqual(resultado.returncode, 0)
        self.assertIn("N2 FALHOU", resultado.stderr)
        self.assertNotIn("ESCOPO N2:", resultado.stdout)

    def test_delta_adversarial_reprovado_com_sha_recalculado(self):
        with tempfile.TemporaryDirectory() as nome:
            script, legado, entrada = self.replica_temporaria(Path(nome))
            with entrada.open(encoding="utf-8", newline="") as handle:
                leitor = csv.DictReader(handle, delimiter=";")
                colunas = leitor.fieldnames
                linhas = list(leitor)
            linhas[0]["Delta"] = str(float(linhas[0]["Delta"]) + 1)
            with entrada.open("w", encoding="utf-8", newline="") as handle:
                gravador = csv.DictWriter(handle, fieldnames=colunas, delimiter=";", lineterminator="\n")
                gravador.writeheader()
                gravador.writerows(linhas)
            digest = hashlib.sha256(entrada.read_bytes()).hexdigest()
            fonte = legado.read_text(encoding="utf-8")
            self.assertIn("8bd38ce75917d981431f4c2a9c09ba73648ff1151fd6ac717068637b682fe120", fonte)
            legado.write_text(
                fonte.replace("8bd38ce75917d981431f4c2a9c09ba73648ff1151fd6ac717068637b682fe120", digest),
                encoding="utf-8",
            )
            resultado = self.executar(script)
        self.assertNotEqual(resultado.returncode, 0)
        self.assertIn("AssertionError", resultado.stderr)
        self.assertNotIn("ESCOPO N2:", resultado.stdout)

    def test_cardinalidade_adversarial_reprovada_com_sha_recalculado(self):
        with tempfile.TemporaryDirectory() as nome:
            script, legado, entrada = self.replica_temporaria(Path(nome))
            linhas = entrada.read_bytes().splitlines(keepends=True)
            entrada.write_bytes(b"".join(linhas[:-1]))
            digest = hashlib.sha256(entrada.read_bytes()).hexdigest()
            fonte = legado.read_text(encoding="utf-8")
            legado.write_text(
                fonte.replace("8bd38ce75917d981431f4c2a9c09ba73648ff1151fd6ac717068637b682fe120", digest),
                encoding="utf-8",
            )
            resultado = self.executar(script)
        self.assertNotEqual(resultado.returncode, 0)
        self.assertIn("AssertionError", resultado.stderr)


if __name__ == "__main__":
    unittest.main()
