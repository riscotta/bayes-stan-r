#!/usr/bin/env python3
"""T07.6: gate documental fail-closed para pares de saídas históricas.

Somente confere contrato/metadados versionados. NÃO executa estudos,
não lê dados brutos e não declara comparação numérica.
"""
from __future__ import annotations

import importlib.util
import json
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[1]
INVENTARIO = ROOT / "reports/t07_6/inventario_pares_T07_6.json"
BASE = "77930248d5fdad81b999bc5741bbb930f4f97858"
NOMES_SAIDA = ("stdout.txt", "stderr.txt", "report.json")


def verificar(dados: dict, estudos_runner: dict) -> dict:
    if not isinstance(dados, dict) or dados.get("schema") != "t07_6_inventario_v1":
        raise ValueError("Schema T07.6 ausente ou inválido")
    if dados.get("snapshot_main_sha") != BASE:
        raise ValueError("SHA-base do inventário divergiu")
    if dados.get("custodia_independente_referencia") != "NAO_COMPROVADA":
        raise ValueError("Custódia histórica declarada sem comprovação")
    if dados.get("equivalencia_historica_demonstrada") is not False:
        raise ValueError("Equivalência histórica indevidamente afirmada")
    estudos = dados.get("estudos")
    if not isinstance(estudos, list) or len(estudos) != 3:
        raise ValueError("Esperados exatamente três estudos")
    chaves = [e.get("estudo") for e in estudos if isinstance(e, dict)]
    if len(chaves) != 3 or set(chaves) != set(estudos_runner):
        raise ValueError("Conjunto de estudos não corresponde ao runner")
    total = 0
    for item in estudos:
        nome = item["estudo"]
        spec = estudos_runner[nome]
        if (item.get("script") != spec["script"]
                or item.get("entrada") != spec["input"]
                or item.get("execucao_permitida") is not spec["allowed"]):
            raise ValueError(f"Contrato original/adaptador divergiu: {nome}")
        saidas = item.get("artefatos")
        if not isinstance(saidas, list):
            raise ValueError(f"Saídas inválidas: {nome}")
        if nome == "pnadc_estudo_i":
            if [a.get("nome") for a in saidas if isinstance(a, dict)] != list(NOMES_SAIDA):
                raise ValueError("Conjunto ou ordem de saídas PNAD adulterados")
            if (item.get("classificacao_entrada") !=
                    "REFERENCIA_PUBLICA_DE_ENTRADA_NAO_OUTPUT_PAREADO"):
                raise ValueError("Entrada PNAD confundida com saída histórica")
            if not re.fullmatch(r"[0-9a-f]{40}", item.get("entrada_git_blob", "")):
                raise ValueError("Blob Git de entrada PNAD inválido")
            if not re.fullmatch(r"[0-9a-f]{64}", item.get("entrada_sha256_documentado", "")):
                raise ValueError("SHA-256 de entrada PNAD inválido")
            for a in saidas:
                esperado = f"outputs/pnadc_estudo_i/logs/<id>/{a['nome']}"
                if (a.get("candidato") != esperado
                        or a.get("referencia_historica") is not None
                        or a.get("status") != "NAO_COMPARAVEL"
                        or not a.get("criterio_previo")):
                    raise ValueError(f"Par de referência inexistente ou falso PASS: {a['nome']}")
            total += len(saidas)
        else:
            if saidas or item.get("status") != "BLOQUEADO_POR_CONTRATO":
                raise ValueError(f"Execução histórica proibida apresentada como saída: {nome}")
    criterios = dados.get("criterios_globais")
    if not isinstance(criterios, dict):
        raise ValueError("Critérios de comparação não definidos")
    if (criterios.get("status_quando_falta_referencia") != "NAO_COMPARAVEL"
            or not str(criterios.get("comparacao_efetiva_neste_ciclo", "")).startswith("NENHUMA")
            or criterios.get("tolerancia_numerica_deterministica") !=
            {"absoluta": "0", "relativa": "0",
             "excecao": "somente protocolo preaprovado e QA; ver T06.4"}):
        raise ValueError("Política de comparação modificada sem revisão")
    return {"status": "PARCIAL_NAO_COMPARAVEL",
            "estudos": len(estudos), "saidas_previstas": total,
            "pares_historicos_autenticados": 0,
            "comparacoes_historicas_efetivas": 0}


def carregar_runner(root: Path) -> dict:
    caminho = root / "adapters/t07_3/runner.py"
    spec = importlib.util.spec_from_file_location("runner_t07_3_inventario", caminho)
    if spec is None or spec.loader is None:
        raise ValueError("Runner T07.3 indisponível")
    modulo = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(modulo)
    return modulo.STUDIES


def main() -> None:
    dados = json.loads(INVENTARIO.read_text(encoding="utf-8"))
    print(json.dumps(verificar(dados, carregar_runner(ROOT)),
                     ensure_ascii=False, sort_keys=True))


if __name__ == "__main__":
    main()
