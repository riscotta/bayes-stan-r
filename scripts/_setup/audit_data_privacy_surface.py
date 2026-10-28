#!/usr/bin/env python3
"""Auditoria exploratória de superfícies de dados, sem exibir registros.

Não descompacta em disco, não lê linhas de dados e não certifica anonimização.
Executa somente inspeção de nomes de campos, tipos e metadados de arquivos.
"""
from collections import Counter
import csv
import io
from pathlib import Path
import re
import sys
import zipfile

ROOT = Path(__file__).resolve().parents[2]
RAW = ROOT / "data" / "raw"
MAX_HEADER = 32768
IDENTIFIERS = re.compile(
    r"(^|[_\W])(cpf|cnpj|cns|cartao_sus|cartao_nacional_saude|"
    r"nome_paciente|nome_pessoa|nome_completo|full_name|patient_name|"
    r"person_name|email|e_mail|telefone|phone_number|endereco|address|"
    r"data_nascimento|birth_date|identidade|rg_numero|documento_pessoal)"
    r"($|[_\W])", re.IGNORECASE
)
QUASI = re.compile(
    r"^(sexo|faixa|faixa_etaria|idade|municipio|bairro|distrito|"
    r"latitude|longitude|data|ano|ym)$", re.IGNORECASE
)


def inspect_header(raw: bytes) -> tuple[int, int] | None:
    if not raw:
        return None
    try:
        head = raw.decode("utf-8-sig")
    except UnicodeDecodeError:
        head = raw.decode("latin-1")
    # Apenas a primeira linha; valores de observações nunca são impressos.
    first = head.splitlines()[0] if head.splitlines() else ""
    if not first:
        return None
    delimiter = ";" if first.count(";") > first.count(",") else ","
    columns = next(csv.reader(io.StringIO(first), delimiter=delimiter), [])
    names = [c.strip().lower().replace(" ", "_") for c in columns]
    return sum(bool(IDENTIFIERS.search(n)) for n in names), sum(bool(QUASI.search(n)) for n in names)


def main() -> int:
    csvs = list(RAW.rglob("*.csv"))
    suspect_csvs = 0
    direct_columns = 0
    joint_quasi = 0
    processed = 0
    for file in csvs:
        try:
            with file.open("rb") as f:
                result = inspect_header(f.readline(MAX_HEADER))
        except OSError:
            continue
        if result is None:
            continue
        processed += 1
        direct, quasi = result
        direct_columns += direct
        suspect_csvs += bool(direct)
        joint_quasi += quasi >= 3
        if direct:
            print(f"::warning file={file.relative_to(ROOT)}::Cabecalho com {direct} nome(s) de campo(s) potencialmente identificaveis; revisar localmente, sem imprimir valores.")
        if quasi >= 3:
            print(f"::notice file={file.relative_to(ROOT)}::Tres ou mais quase-identificadores aparentes no cabecalho; avaliar risco de reidentificacao.")

    print("Escopo: cabecalhos CSV e metadados de arquivos ZIP/XLSX.")
    print(f"CSV inspecionados: {processed}; arquivos com possiveis nomes de identificadores diretos: {suspect_csvs}; campos sinalizados: {direct_columns}; cabecalhos com tres ou mais quase-identificadores: {joint_quasi}")

    for path in sorted(RAW.rglob("*")):
        if path.suffix.lower() not in (".zip", ".xlsx"):
            continue
        relpath = path.relative_to(ROOT)
        try:
            with zipfile.ZipFile(path) as z:
                items = [i for i in z.infolist() if not i.is_dir()]
                ext = Counter(Path(i.filename).suffix.lower() or "(sem extensao)" for i in items)
                total = sum(i.file_size for i in items)
                flagged_names = sum(bool(IDENTIFIERS.search(Path(i.filename).name)) for i in items)
                nested = 0
                nested_direct = 0
                for i in items:
                    if Path(i.filename).suffix.lower() not in (".csv", ".tsv", ".txt"):
                        continue
                    if i.file_size > 200_000_000:
                        continue
                    with z.open(i) as f:
                        sample = f.readline(MAX_HEADER)
                    result = inspect_header(sample)
                    if result is not None:
                        nested += 1
                        nested_direct += result[0]
                print(f"{relpath}: ZIP valido; entradas={len(items)}; bytes_descomprimidos={total}; tipos={dict(sorted(ext.items()))}; nomes_arquivo_sinalizados={flagged_names}; cabecalhos_texto_inspecionados={nested}; colunas_sinalizadas={nested_direct}")
                if path.suffix.lower() == ".xlsx":
                    print(f"{relpath}: planilha XML nao foi classificada semanticamente; revisao humana necessaria.")
                if flagged_names or nested_direct:
                    print(f"::warning file={relpath}::Verificar possiveis identificadores na estrutura do arquivo antes de qualquer redistribuicao.")
        except (OSError, zipfile.BadZipFile, RuntimeError) as exc:
            print(f"::warning file={relpath}::Nao foi possivel inspecionar o arquivo: {type(exc).__name__}")
    print("LIMITES: nenhum dado foi anonimizado; inspeção superficial, não avaliação LGPD ou direitos de terceiros.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
