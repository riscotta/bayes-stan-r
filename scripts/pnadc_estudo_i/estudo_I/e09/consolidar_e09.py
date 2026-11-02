"""E09: consolida os contrastes previamente computados em E08, sem nova inferência/MCMC."""
from pathlib import Path
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
E08 = ROOT / 'e08'
SAIDA = Path(__file__).resolve().parent / 'contrastes_e09_consolidado.csv'


def carregar(nome):
    return pd.read_csv(E08 / nome, sep=';', encoding='utf-8-sig')


cenarios = carregar('cenarios_contrastes_e082.csv')
assert len(cenarios) == 104
chave = ['Eixo', 'IndicadorId', 'Contraste']
assert not cenarios[chave + ['RhoHipotetico']].duplicated().any()
base = cenarios.loc[cenarios['RhoHipotetico'] == 0].copy()
limites = cenarios.groupby(chave, as_index=False).agg(
    ProbMinCenariosRho=('ProbSentido', 'min'),
    ProbMaxCenariosRho=('ProbSentido', 'max'),
)
base = base.merge(limites, on=chave, validate='one_to_one')
quadro = carregar('confronto_quadro_e084.csv')
quadro = quadro.rename(columns={
    'SituacaoOficial': 'SituacaoOficialIBGE',
    'DeltaPublicadoNoQuadro': 'DeltaOficialIBGE',
})
base = base.merge(quadro[chave + ['SituacaoOficialIBGE', 'DeltaOficialIBGE']],
                  on=chave, how='left', validate='one_to_one')
base['SituacaoOficialIBGE'] = base['SituacaoOficialIBGE'].fillna(
    'Não consta no Quadro Sintético')
base['UnidadeDelta'] = base['IndicadorId'].apply(
    lambda nome: 'p.p.' if nome.endswith('_pct') else
    ('Mil pessoas' if nome.endswith('_mil') else 'R$'))
colunas = ['Eixo', 'IndicadorId', 'Indicador', 'Contraste',
           'PeriodoOrigem', 'PeriodoDestino', 'ValorOrigem', 'ValorDestino',
           'Delta', 'UnidadeDelta', 'SituacaoOficialIBGE', 'DeltaOficialIBGE',
           'RhoHipotetico', 'SDDeltaCondicional', 'IC95Inf', 'IC95Sup',
           'ProbSentido', 'ProbMinCenariosRho', 'ProbMaxCenariosRho',
           'SentidoIndicador', 'Classificacao']
saida = base[colunas].sort_values(chave).reset_index(drop=True)
assert len(saida) == 26 and saida['IndicadorId'].nunique() == 13
assert saida.groupby('Eixo').size().to_dict() == {'A': 4, 'B': 8, 'C': 8, 'D': 4, 'E': 2}
assert not saida[['Delta', 'ProbSentido', 'IC95Inf', 'IC95Sup']].isna().any().any()
saida.to_csv(SAIDA, sep=';', index=False, encoding='utf-8-sig', float_format='%.9f')
print(f'E09: {len(saida)} contrastes, 13 indicadores, 5 eixos; {SAIDA}')
print('NOTA: rho=0 e limites rho são cenários hipotéticos; NÃO posterior conjunta.')
