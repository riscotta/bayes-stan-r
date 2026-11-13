"""T06.4: conferência técnica isolada; nunca certifica N3/N4 ou validade científica.

Usa somente stdlib e caminhos locais autorizados. Não executa modelo nem grava outputs.
"""
import argparse
import csv
from fractions import Fraction
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
MAX_CASAS_COEFICIENTE = 512
MAX_EXPOENTE_ABS = 1000
MAX_SEED = 2**64 - 1


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
    partes = relativo.split("/")
    exigir(all(x not in {".", "..", ""} for x in partes),
           "caminho com componentes proibidos")
    p = PurePosixPath(relativo)
    exigir(not p.is_absolute(), "caminho absoluto proibido")
    base = Path(root).resolve(strict=True)
    alvo_bruto = base.joinpath(*partes)
    # Symlinks no arquivo ou em qualquer ancestral relativo à raiz são proibidos.
    atual = base
    for parte in partes:
        atual = atual / parte
        exigir(not atual.is_symlink(), "link simbólico no caminho/ancestral")
    alvo = alvo_bruto.resolve(strict=True)
    exigir(alvo.is_relative_to(base) and alvo.is_file(), "caminho fora da raiz ou não arquivo")
    return alvo


def sha256_arquivo(path):
    h = hashlib.sha256()
    with path.open("rb") as arquivo:
        for bloco in iter(lambda: arquivo.read(1048576), b""):
            h.update(bloco)
    return h.hexdigest()


def git_blob_sha(path):
    """Identidade Git SHA-1 de blob, distinta do SHA-256 de bytes."""
    h = hashlib.sha1()
    h.update(f"blob {path.stat().st_size}\0".encode("ascii"))
    with path.open("rb") as arquivo:
        for bloco in iter(lambda: arquivo.read(1048576), b""):
            h.update(bloco)
    return h.hexdigest()


def conferir_ancora(registro, ref, catalogo, catalogo_sha256_confiavel):
    """Catálogo com digest fixado fora do registro; não autentica a fonte do pin."""
    exigir(isinstance(catalogo_sha256_confiavel, str) and
           SHA256.fullmatch(catalogo_sha256_confiavel),
           "âncora externa de catálogo ausente ou inválida")
    exigir(catalogo is not None, "catálogo de referência ausente")
    arq = Path(catalogo).resolve(strict=True)
    exigir(arq.is_file(), "catálogo não é arquivo")
    if sha256_arquivo(arq) != catalogo_sha256_confiavel:
        raise Divergencia("digest do catálogo diverge do pin externo confiável")
    try:
        dados = json.loads(arq.read_text(encoding="utf-8"))
    except (UnicodeError, json.JSONDecodeError):
        raise Bloqueio("catálogo malformado") from None
    exigir(isinstance(dados, dict) and dados.get("schema") == "t06_4_catalogo_v1" and
           isinstance(dados.get("git_commit"), str) and
           SHA_GIT.fullmatch(dados["git_commit"]), "catálogo sem proveniência de commit")
    itens = dados.get("referencias")
    exigir(isinstance(itens, list) and bool(itens), "referências de catálogo ausentes")
    for item in itens:
        exigir(isinstance(item, dict) and set(item) == {"path", "sha256", "git_blob_sha"} and
               isinstance(item["path"], str) and item["path"] and
               isinstance(item["sha256"], str) and SHA256.fullmatch(item["sha256"]) and
               isinstance(item["git_blob_sha"], str) and SHA_GIT.fullmatch(item["git_blob_sha"]),
               "entrada de catálogo inválida")
    correspondentes = [i for i in itens if i["path"] == registro["referencia"]["path"]]
    exigir(len(correspondentes) == 1, "referência sem entrada única em catálogo confiável")
    item = correspondentes[0]
    if item["sha256"] != registro["referencia"]["sha256"]:
        raise Divergencia("checksum do registro difere do catálogo fixado")
    if sha256_arquivo(ref) != item["sha256"]:
        raise Divergencia("bytes da referência diferem do catálogo fixado")
    if git_blob_sha(ref) != item["git_blob_sha"]:
        raise Divergencia("Git blob da referência difere do catálogo fixado")
    return dados["git_commit"]


def decimal_estrito(valor):
    try:
        d = Decimal(str(valor))
    except (InvalidOperation, ValueError, TypeError):
        raise Bloqueio("valor ou tolerância não numérico") from None
    exigir(d.is_finite(), "valor ou tolerância não finito")
    tupla = d.as_tuple()
    exigir(len(tupla.digits) <= MAX_CASAS_COEFICIENTE and
           abs(tupla.exponent) <= MAX_EXPOENTE_ABS,
           "valor/tolerância excede limites de precisão ou expoente")
    return d


def seed_valida(seed):
    """Aceita seed inteira de 64 bits ou declaração textual inequívoca de NA."""
    if isinstance(seed, bool):
        return False
    if isinstance(seed, int):
        return 0 <= seed <= MAX_SEED
    if isinstance(seed, str):
        return seed == "NAO_APLICAVEL" or (
            re.fullmatch(r"[0-9]{1,20}", seed) is not None and
            int(seed) <= MAX_SEED
        )
    return False


def excede_tolerancia(original, novo, absoluto, relativo):
    """Compara decimais finitos como racionais exatos; nunca arredonda o limiar."""
    ref = Fraction(original)
    cand = Fraction(novo)
    return abs(cand - ref) > Fraction(absoluto) + Fraction(relativo) * abs(ref)


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
        for v in tolerancia.values():
            exigir(isinstance(v, (str, int, float, Decimal)) and not isinstance(v, bool),
                   "tolerância deve ser número finito ou sua string decimal")
        a, r = decimal_estrito(tolerancia["abs"]), decimal_estrito(tolerancia["rel"])
        exigir(a >= 0 and r >= 0, "tolerância negativa")
        resultados[nome] = (a, r)
    nao_zero = any(a or r for a, r in resultados.values())
    aprovacao = configuracao.get("preaprovacao")
    if nao_zero or tipo == "csv_estocastico_resumo":
        exigir(isinstance(aprovacao, dict), "sem pré-aprovação de limiar")
        exigir(all(isinstance(aprovacao.get(k), str) and aprovacao[k].strip()
                   for k in ("id_protocolo", "justificativa_matematica",
                             "responsavel_qa", "evidencia_calibracao")) and
               len(aprovacao["justificativa_matematica"].strip()) >= 40,
               "limiar sem justificativa, calibração ou responsável")
    return resultados


def conferir(registro, raiz, *, catalogo=None, catalogo_sha256_confiavel=None):
    """Retorna estado técnico, sem promover N1/N2 em N3/N4.

    Bloqueio => nenhuma comparação autorizável; Divergencia => comparação falhou.
    """
    exigir(isinstance(registro, dict) and registro.get("schema") == "t06_4_execucao_v1",
           "schema desconhecido")
    estudo, identificador = registro.get("estudo"), registro.get("execucao_id")
    exigir(isinstance(estudo, str) and NAME.fullmatch(estudo) and
           isinstance(identificador, str) and RUN.fullmatch(identificador), "IDs inválidos")
    modo, tipo = registro.get("modo"), registro.get("tipo")
    exigir(isinstance(modo, str) and modo in MODOS and
           isinstance(tipo, str) and tipo in TIPOS, "modo/tipo inválido")
    exigir(isinstance(registro.get("codigo_sha"), str) and
           SHA_GIT.fullmatch(registro["codigo_sha"]), "commit de código não rastreável")
    exigir(isinstance(registro.get("ambiente"), str) and registro["ambiente"].strip(),
           "ambiente ausente")
    exigir(isinstance(registro.get("comando"), str) and registro["comando"].strip(),
           "comando ausente")
    exigir(seed_valida(registro.get("seed")),
           "seed inválida: use inteiro 0..2^64-1 ou NAO_APLICAVEL")
    exigir(isinstance(registro.get("entrada_sha256"), str) and
           SHA256.fullmatch(registro["entrada_sha256"]), "hash da entrada ausente")
    exigir(isinstance(registro.get("direitos"), str) and
           registro["direitos"] in {"NAO_HOMOLOGADOS", "HOMOLOGADOS"},
           "estado de direitos não declarado")
    if modo != "auditoria_rapida":
        # Issue #14 P0 aberta: o campo do registro não é prova de homologação.
        raise Bloqueio("P0: modos N3/N4 exigem homologação institucional externa verificada")
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
    base = Path(raiz).resolve(strict=True)
    isolamento = base / "outputs" / "_reexecucoes_t06_4" / estudo / identificador
    dominio_temporario = base / "outputs" / "_reexecucoes_t06_4"
    exigir(cand.is_relative_to(isolamento) and cand != isolamento,
           "candidato não está sob raiz canônica da execução")
    exigir(not ref.is_relative_to(dominio_temporario),
           "referência sob domínio temporário de reexecuções")
    exigir(ref != cand and not ref.samefile(cand),
           "referência e candidato são mesmo arquivo físico")
    conferir_ancora(registro, ref, catalogo, catalogo_sha256_confiavel)
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
           all(isinstance(c, str) and bool(c) for c in chaves) and
           len(chaves) == len(set(chaves)),
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
            if excede_tolerancia(original, novo, a, r):
                raise Divergencia(f"diferença excede limite em {coluna} / {chave}")
    if tipo == "csv_estocastico_resumo":
        exigir(bool(registro.get("oraculo_estimandos")) and
               bool(registro.get("diagnosticos_mcse_ess_rhat")),
               "faltam oráculo pré-especificado e diagnósticos para amostragem")
        return {"estado": "PENDENTE_QA", "escopo": "RESUMO_ESTOCASTICO_SEM_VALIDACAO_INFERENCIAL"}
    if any(a or r for a, r in tol.values()):
        # Declarações textuais de preaprovação não são autenticação externa.
        return {"estado": "PENDENTE_QA", "escopo": "TOLERANCIA_NAO_ZERO_SEM_APROVACAO_AUTENTICADA"}
    return {"estado": "CONFERENCIA_TECNICA", "escopo": "CSV_DESCRITIVO_NAO_N3_N4"}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--registro", required=True, type=Path)
    parser.add_argument("--raiz", required=True, type=Path)
    parser.add_argument("--catalogo", required=True, type=Path,
                        help="snapshot de referências versionado e previamente fixado")
    parser.add_argument("--catalogo-sha256-confiavel", required=True,
                        help="SHA-256 obtido fora do registro de execução, por canal confiável")
    args = parser.parse_args()
    try:
        # Preservar decimais JSON sem passar por float binário antes de Fraction.
        registro = json.loads(args.registro.read_text(encoding="utf-8"),
                              parse_float=Decimal)
        resultado = conferir(registro, args.raiz, catalogo=args.catalogo,
                            catalogo_sha256_confiavel=args.catalogo_sha256_confiavel)
    except (Bloqueio, Divergencia, FileNotFoundError, OSError,
            ValueError, InvalidOperation, csv.Error, RecursionError) as erro:
        status = "DIVERGENCIA" if isinstance(erro, Divergencia) else "BLOQUEADO"
        print(json.dumps({"estado": status, "causa": str(erro)}, ensure_ascii=False))
        return 2
    print(json.dumps(resultado, ensure_ascii=False))
    return 0 if resultado["estado"] == "CONFERENCIA_TECNICA" else 3


if __name__ == "__main__":
    raise SystemExit(main())
