#!/usr/bin/env python3
"""Auditoria reexecutável de síntese E09 (não executa MCMC nem baixa SIDRA)."""
from __future__ import annotations
import csv
import hashlib
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
ARQUIVO = ROOT / 'data/raw/pnadc_estudo_i/resultados_auditados/contrastes_descritos_e09.csv'
SHA256 = '8bd38ce75917d981431f4c2a9c09ba73648ff1151fd6ac717068637b682fe120'

def auditar() -> None:
    assert ARQUIVO.is_file(), f'Entrada indisponível: {ARQUIVO}'
    data = ARQUIVO.read_bytes()
    assert hashlib.sha256(data).hexdigest() == SHA256, 'Arquivo diferente da síntese publicada'
    with ARQUIVO.open(encoding='utf-8', newline='') as f:
        registros = list(csv.DictReader(f, delimiter=';'))
    assert len(registros) == 26
    indicadores = defaultdict(set)
    contrastes = Counter()
    for r in registros:
        assert r['Eixo'] in 'ABCDE'
        assert r['Contraste'] in {'anual', 'recente'}
        assert r['PeriodoDestino'] == '202609'
        assert r['PeriodoOrigem'] == ('202509' if r['Contraste']=='anual' else '202606')
        assert abs(float(r['ValorDestino']) - float(r['ValorOrigem']) - float(r['Delta'])) < 1e-7
        indicadores[r['Eixo']].add(r['IndicadorId'])
        contrastes[r['Contraste']] += 1
    assert {eixo:len(ids) for eixo, ids in indicadores.items()} == {'A':2,'B':4,'C':4,'D':2,'E':1}
    assert contrastes == Counter({'anual':13,'recente':13})
    desocupacao = [r for r in registros if r['IndicadorId']=='desocupacao_pct']
    assert len(desocupacao)==2 and all(float(r['ValorDestino'])==5.2 for r in desocupacao)
    print('E10 / PNADC Estudo I — AUDITORIA DE SÍNTESE: APROVADA')
    print('13 indicadores | 5 eixos | 26 contrastes | dados congelados SHA-256')
    print('Atenção: não reproduz coleta, E04-E08, amostragem MCMC ou posterior conjunta.')

if __name__ == '__main__':
    auditar()
