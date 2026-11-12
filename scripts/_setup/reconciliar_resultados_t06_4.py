"""T06.4: conferência técnica isolada; nunca certifica N3/N4 ou validade científica.

Usa somente stdlib e caminhos locais autorizados. Não executa modelo nem grava outputs.
"""
import argparse
import csv
from decimal import Decimal, InvalidOperation
import hashlib
import json
from pathlib import Path, PurePosixPath
import re

SHA256 = re.compile(r"[0-9a-f]{64}\Z")
SHA_GIT = re.compile(r"[0-9a-f]{40}\Z")
NAME = re.compile(r"[a-z0-9_]+\Z")
RUN = re.compile(r"[a-zA-Z0-9_-]+\Z")
MODOS = {"auditoria_rapida", "reexecucao_modelo", "reproducao_desde_dados"}
TIPOS = {"bytes_exatos", "csv_deterministico", "csv_estocastico_resumo"}


class Bloqueio(ValueError):
    """Metadado, permissão ou cadeia de custódia insuficiente."""


class Divergencia(ValueError):
    """Diferença observada ou artefato adulterado."""


def exigir(condicao, mensagem):
    if not condicao:
        raise Bloqueio(mensagem)


def caminho(root, relativo):
    exigir(isinstance(relativo, str) and relativo and "\\" not in relativo,
           "caminho deve ser relativo POSIX")
    p = PurePosixPath(relativo)
    exigir(not p.is_absolute() and all(x not in {".", "..", ""} for x in p.parts),
           "caminho absoluto ou com traversal")
    base = Path(root).resolve(strict=True)
    alvo = (base / relativo).resolve(strict=True)
    exigir(alvo.is_relative_to(base) and alvo.is_file(), "caminho fora da raiz ou não arquivo")
    return alvo


def sha256_arquivo(path):
    h = hashlib.sha256()
    with path.open("rb") as arquivo:
        for bloco in iter(lambda: arquivo.read(1048576), b""):
            h.update(bloco)
    return h.hexdigest()


def decimal_estrito(valor):
    try:
        d = Decimal(str(valor))
    except (InvalidOperation, ValueError, TypeError):
        raise Bloqueio("valor ou tolerância não numérico") from None
    exigir(d.is_finite(), "valor ou tolerância não finito")
    return d


def tabela(path, chaves, colunas):
    with path.open("r", encoding="utf-8-sig", newline="") as arquivo:
        leitor = csv.DictReader(arquivo)
        campos = leitor.fieldnames
        exigir(campos is not None and len(campos) == len(set(campos)), "cabeçalho duplicado")
        exigir(set(campos) == set(chaves) | set(colunas), "colunas ausentes/extras")
        linhas = {}
        for linha in leitor:
            exigir(None not in linha and all(v is not None for v in linha.values()),
                   "CSV com célula/coluna irregular")
            chave = tuple(linha[c] for c in chaves)
            exigir(all(x != "" for x in chave), "chave vazia")
            exigir(chave not in linhas, "chave duplicada")
            linhas[chave] = linha
    return linhas


def tolerancias(configuracao, tipo):
    colunas = configuracao.get("colunas")
    exigir(isinstance(colunas, dict) and bool(colunas), "colunas numéricas não definidas")
    resultados = {}
    for nome, tolerancia in colunas.items():
        exigir(isinstance(nome, str) and nome, "métrica sem nome")
        exigir(isinstance(tolerancia, dict) and set(tolerancia) == {"abs", "rel"},
               "tolerância sem par abs/rel")
        a, r = decimal_estrito(tolerancia["abs"]), decimal_estrito(tolerancia["rel"])
        exigir(a >= 0 and r >= 0, "tolerância negativa")
        resultados[nome] = (a, r)
    nao_zero = any(a or r for a, r in resultados.values())
    aprovacao = configuracao.get("preaprovacao")
    if nao_zero or tipo == "csv_estocastico_resumo":
        exigir(isinstance(aprovacao, dict), "sem pré-aprovação de limiar")
        exigir(bool(aprovacao.get("id_protocolo")) and
               len(aprovacao.get("justificativa_matematica", "")) >= 40 and
               bool(aprovacao.get("responsavel_qa")) and
               bool(aprovacao.get("evidencia_calibracao")),
               "limiar sem justificativa, calibração ou responsável")
    return resultados


def conferir(registro, raiz):
    """Retorna estado técnico, sem promover N1/N2 em N3/N4.

    Bloqueio => nenhuma comparação autorizável; Divergencia => comparação falhou.
    """
    exigir(isinstance(registro, dict) and registro.get("schema") == "t06_4_execucao_v1",
           "schema desconhecido")
    estudo, identificador = registro.get("estudo"), registro.get("execucao_id")
    exigir(isinstance(estudo, str) and NAME.fullmatch(estudo) and
           isinstance(identificador, str) and RUN.fullmatch(identificador), "IDs inválidos")
    modo, tipo = registro.get("modo"), registro.get("tipo")
    exigir(modo in MODOS and tipo in TIPOS, "modo/tipo inválido")
    exigir(isinstance(registro.get("codigo_sha"), str) and
           SHA_GIT.fullmatch(registro["codigo_sha"]), "commit de código não rastreável")
    exigir(isinstance(registro.get("ambiente"), str) and registro["ambiente"].strip(),
           "ambiente ausente")
    exigir(isinstance(registro.get("comando"), str) and registro["comando"].strip(),
           "comando ausente")
    exigir("seed" in registro and registro["seed"] is not None,
           "seed ou declaração explícita de não aplicabilidade ausente")
    exigir(isinstance(registro.get("entrada_sha256"), str) and
           SHA256.fullmatch(registro["entrada_sha256"]), "hash da entrada ausente")
    exigir(registro.get("direitos") in {"NAO_HOMOLOGADOS", "HOMOLOGADOS"},
           "estado de direitos não declarado")
    if modo != "auditoria_rapida":
        exigir(registro["direitos"] == "HOMOLOGADOS", "P0: direitos não homologados")
    referencia, candidato = registro.get("referencia"), registro.get("candidato")
    exigir(isinstance(referencia, dict) and isinstance(candidato, dict),
           "referência/candidato incompletos")
    for item in (referencia, candidato):
        exigir(isinstance(item.get("sha256"), str) and SHA256.fullmatch(item["sha256"]),
               "checksum declarado ausente")
    prefixo = f"outputs/_reexecucoes_t06_4/{estudo}/{identificador}/"
    exigir(isinstance(candidato.get("path"), str) and
           candidato["path"].startswith(prefixo), "saída não isolada por estudo/execução")
    exigir(not str(referencia.get("path", "")).startswith("outputs/_reexecucoes_t06_4/"),
           "referência não pode ser uma saída temporária da execução")
    ref, cand = caminho(raiz, referencia.get("path")), caminho(raiz, candidato["path"])
    exigir(ref != cand, "referência e candidato são mesmo arquivo")
    if sha256_arquivo(ref) != referencia["sha256"]:
        raise Divergencia("checksum de referência diverge do congelado")
    if sha256_arquivo(cand) != candidato["sha256"]:
        raise Divergencia("checksum do candidato diverge do declarado")
    if tipo == "bytes_exatos":
        if referencia["sha256"] != candidato["sha256"]:
            raise Divergencia("conteúdo binário diferente")
        return {"estado": "CONFERENCIA_TECNICA", "escopo": "IDENTIDADE_BYTES_NAO_N3_N4"}
    config = registro.get("comparacao")
    exigir(isinstance(config, dict), "configuração CSV ausente")
    chaves = config.get("chaves")
    exigir(isinstance(chaves, list) and bool(chaves) and
           len(chaves) == len(set(chaves)) and all(isinstance(c, str) and c for c in chaves),
           "chaves de alinhamento inválidas")
    tol = tolerancias(config, tipo)
    exigir(not set(chaves) & set(tol), "chaves usadas como métricas")
    tabela_ref = tabela(ref, chaves, tol)
    tabela_nova = tabela(cand, chaves, tol)
    if tabela_ref.keys() != tabela_nova.keys():
        raise Divergencia("chaves de linha ausentes/extras; alinhamento proibido")
    for chave, valores in tabela_ref.items():
        for coluna, (a, r) in tol.items():
            original = decimal_estrito(valores[coluna])
            novo = decimal_estrito(tabela_nova[chave][coluna])
            if abs(novo - original) > a + r * abs(original):
                raise Divergencia(f"diferença excede limite em {coluna} / {chave}")
    if tipo == "csv_estocastico_resumo":
        exigir(bool(registro.get("oraculo_estimandos")) and
               bool(registro.get("diagnosticos_mcse_ess_rhat")),
               "faltam oráculo pré-especificado e diagnósticos para amostragem")
        return {"estado": "PENDENTE_QA", "escopo": "RESUMO_ESTOCASTICO_SEM_VALIDACAO_INFERENCIAL"}
    return {"estado": "CONFERENCIA_TECNICA", "escopo": "CSV_DESCRITIVO_NAO_N3_N4"}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--registro", required=True, type=Path)
    parser.add_argument("--raiz", required=True, type=Path)
    args = parser.parse_args()
    try:
        registro = json.loads(args.registro.read_text(encoding="utf-8"))
        resultado = conferir(registro, args.raiz)
    except (Bloqueio, Divergencia, FileNotFoundError, OSError, ValueError) as erro:
        status = "DIVERGENCIA" if isinstance(erro, Divergencia) else "BLOQUEADO"
        print(json.dumps({"estado": status, "causa": str(erro)}, ensure_ascii=False))
        return 2
    print(json.dumps(resultado, ensure_ascii=False))
    return 0 if resultado["estado"] == "CONFERENCIA_TECNICA" else 3


if __name__ == "__main__":
    raise SystemExit(main())
