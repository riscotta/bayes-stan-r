#!/usr/bin/env python3
"""E05 PNAD Contínua: exploração descritiva das 13 séries pré-especificadas.

Executar da raiz do pacote ou de qualquer diretório:
    python estudo_I/e05/explorar_e05.py
Não estima posteriores nem assume independência de trimestres móveis.
"""
from __future__ import annotations
import hashlib
import json
import platform
from pathlib import Path
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / 'estudo_I/e05/saida'
IN_LONG = ROOT / 'estudo_I/e04/saida/base_analitica_e04_longa.csv'
IN_WIDE = ROOT / 'estudo_I/e04/saida/base_analitica_e04_larga.csv'
MANIFEST = ROOT / 'MANIFESTO_SHA256.csv'
OUT.mkdir(parents=True, exist_ok=True)

def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with path.open('rb') as f:
        for chunk in iter(lambda: f.read(1 << 20), b''):
            h.update(chunk)
    return h.hexdigest()

manifest = pd.read_csv(MANIFEST, sep=';', encoding='utf-8-sig', dtype=str)
if set(manifest.columns) == {'arquivo','bytes','sha256'}:
    manifest = manifest.rename(columns={'arquivo':'Caminho','bytes':'Bytes','sha256':'SHA256'})
assert set(manifest.columns) == {'Caminho','Bytes','SHA256'}
# Transporte E06: os registros vivos podem ser editados, e as saídas E05
# podem ser regeneradas; só as fontes e entradas imutáveis exigem os hashes
# do manifesto deste pacote (independente do número total de membros).
LIVE = {'estudo_I/CONTRATO.md','estudo_I/ESTADO_ATUAL.md','estudo_I/DIARIO_EVIDENCIAS.md'}
REGENERABLE_PREFIX = 'estudo_I/e05/saida/'
assert not manifest.Caminho.duplicated().any()
# A verificação do manifesto completo é feita antes de qualquer reparo E10.
# Aqui só as fontes imutáveis (dados e norma) precisam bater com a entrega;
# o próprio script foi corrigido e saídas podem ser regeneradas.
immutable = manifest[manifest.Caminho.str.startswith(('dados/02_metadados/', 'dados/04_series_csv/'))]
assert len(immutable) > 0
for row in immutable.itertuples(index=False):
    path = ROOT / row.Caminho
    assert path.is_file() and path.stat().st_size == int(row.Bytes), f'Arquivo inválido: {row.Caminho}'
    assert sha256(path).lower() == row.SHA256.lower(), f'Hash divergente: {row.Caminho}'
assert all((ROOT/p).is_file() for p in LIVE)

long = pd.read_csv(IN_LONG, sep=';', encoding='utf-8-sig', dtype={'Periodo':str})
wide = pd.read_csv(IN_WIDE, sep=';', encoding='utf-8-sig', dtype={'Periodo':str})
assert long.shape == (2405,31), long.shape
assert wide.shape == (175,31), wide.shape
assert len(long.IndicadorId.unique()) == 14
assert long.loc[long.Papel == 'principal', 'IndicadorId'].nunique() == 13
assert wide.Periodo.is_unique
assert wide.Periodo.min() == '201203' and wide.Periodo.max() == '202609'
assert long.Valor.isna().sum() == 4
assert not long.duplicated(['IndicadorId','Periodo']).any()
assert set(long.loc[long.Valor.isna(),'Periodo']) == {'201510','201511'}
assert set(long.loc[long.Valor.isna(),'IndicadorId']) == {'subutilizacao_pct','subocupacao_horas_pct'}
assert long.loc[long.IndicadorId=='informalidade_pct', 'Periodo'].min() == '201512'
assert long.loc[long.IndicadorId=='informalidade_pct'].shape[0] == 130
assert not long[(long.Papel=='principal') & long.Valor.notna()].CVPercentual.isna().any()

wide_ix = wide.set_index('Periodo')
periods = ['202509','202606','202609']
assert set(periods).issubset(wide_ix.index)
series = long[['Eixo','IndicadorId','Indicador','Papel','UnidadeAnalitica']].drop_duplicates().sort_values(['Eixo','IndicadorId'])
records, statrecords = [], []
for info in series.itertuples(index=False):
    col = info.IndicadorId
    v = long[long.IndicadorId == col].sort_values('Periodo').copy()
    numeric = v.dropna(subset=['Valor'])
    cur = v.loc[v.Periodo=='202609'].iloc[0]
    a, b, c = [float(wide_ix.loc[per,col]) for per in periods]
    unit_delta = 'p.p.' if info.UnidadeAnalitica == '%' else info.UnidadeAnalitica
    records.append(dict(Eixo=info.Eixo, Papel=info.Papel, IndicadorId=col,
        Indicador=info.Indicador, Unidade=info.UnidadeAnalitica, PeriodoBaseAnual=periods[0],
        Valor202509=a, PeriodoBaseRecente=periods[1], Valor202606=b,
        PeriodoAtual=periods[2], Valor202609=c,
        DeltaAnual=c-a, DeltaRecente=c-b, UnidadeDelta=unit_delta,
        DeltaAnualRelativoPct=(100*(c/a-1) if (a!=0 and info.UnidadeAnalitica != '%') else None),
        CVAtualPercentual=float(cur.CVPercentual)))
    # Não combinar regimes de 2015 em mínimos, medianas e máximos interpretáveis.
    if col in ('subutilizacao_pct', 'subocupacao_horas_pct'):
        groups = [(reg, v[v.RegimeConceitual2015 == reg]) for reg in
                  ('ANTES_RUPTURA_2015', 'APOS_RUPTURA_2015')]
    else:
        groups = [('SERIE_PUBLICADA', v)]
    for regime, portion in groups:
        n = portion.dropna(subset=['Valor'])
        minrow = n.loc[n.Valor.idxmin()]
        maxrow = n.loc[n.Valor.idxmax()]
        statrecords.append(dict(Eixo=info.Eixo,Papel=info.Papel,IndicadorId=col,
            Indicador=info.Indicador, Regime=regime, Unidade=info.UnidadeAnalitica,
            PeriodosPublicados=len(portion), ObservacoesNumericas=len(n),
            NaoDivulgados=int(portion.Valor.isna().sum()), PrimeiroPeriodo=str(portion.Periodo.min()),
            UltimoPeriodo=str(portion.Periodo.max()), Minimo=float(minrow.Valor),
            PeriodoMinimo=str(minrow.Periodo), MedianaTemporal=float(n.Valor.median()),
            Maximo=float(maxrow.Valor),PeriodoMaximo=str(maxrow.Periodo),
            CVMedianaPercentual=float(n.CVPercentual.median())))

changes = pd.DataFrame.from_records(records)
stats = pd.DataFrame.from_records(statrecords)
assert len(changes) == 14 and len(stats) == 16
changes.to_csv(OUT/'contrastes_e05.csv',sep=';',encoding='utf-8-sig',index=False,float_format='%.6f')
stats.to_csv(OUT/'distribuicoes_temporais_e05.csv',sep=';',encoding='utf-8-sig',index=False,float_format='%.6f')
# Comparações contábeis de estoques publicados, não fluxos pessoa-a-pessoa.
pop2026 = float(wide_ix.loc['202609','populacao_14mais_mil'])
num2026 = float(wide_ix.loc['202609','desocupados_mil'])
den2026 = float(wide_ix.loc['202609','forca_trabalho_mil'])
assert round(100*num2026/den2026,1) == float(wide_ix.loc['202609','desocupacao_pct'])
assert round(100*float(wide_ix.loc['202609','ocupados_mil'])/pop2026,1) == float(wide_ix.loc['202609','nivel_ocupacao_pct'])
assert abs((wide_ix.loc['202609','ocupados_mil']-wide_ix.loc['202509','ocupados_mil'])-(wide_ix.loc['202609','forca_trabalho_mil']-wide_ix.loc['202509','forca_trabalho_mil']) + (wide_ix.loc['202609','desocupados_mil']-wide_ix.loc['202509','desocupados_mil'])) < 0.01

plt.rcParams.update({'font.size': 11, 'axes.grid': True, 'grid.alpha': 0.2,
                    'savefig.dpi': 170})
dates = pd.to_datetime(wide['Periodo'],format='%Y%m')

def canvas(title, ylabel):
    fig, ax = plt.subplots(figsize=(10.8,4.0))
    ax.set_title(title, loc='left', fontsize=14, pad=15)
    ax.set_ylabel(ylabel)
    ax.xaxis.set_major_locator(mdates.YearLocator(2))
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
    ax.spines[['top','right']].set_visible(False)
    ax.set_xlim(pd.Timestamp('2012-02-01'),pd.Timestamp('2027-02-01'))
    return fig, ax

def finalize(fig, ax, fn, caption, legend=True):
    if legend: ax.legend(loc='best',fontsize=9, frameon=False)
    fig.text(0.055,0.01,caption,fontsize=8.2,ha='left',alpha=.8)
    fig.tight_layout(rect=(0,0.05,1,1))
    fig.savefig(OUT/fn, bbox_inches='tight')
    plt.close(fig)

fig,ax=canvas('A — Taxa de desocupação','% da força de trabalho')
ax.plot(dates,wide.desocupacao_pct,label='Desocupação')
ax.scatter(dates.iloc[-1],wide.desocupacao_pct.iloc[-1],zorder=5)
finalize(fig,ax,'grafico_A_desocupacao.png','Fonte: IBGE/SIDRA, série agregada; trimestres móveis; setembro/2026 = 5,2%.')

fig,ax=canvas('B — Ocupação e participação','% da população de 14 anos ou mais')
ax.plot(dates,wide.nivel_ocupacao_pct,label='Nível de ocupação')
ax.plot(dates,wide.participacao_pct,label='Participação na força de trabalho')
finalize(fig,ax,'grafico_B_ocupacao_participacao.png','Fonte: IBGE/SIDRA. Indicadores com denominador populacional comum.')

for col, title, fn, ylabel in [
    ('subutilizacao_pct','C — Taxa composta de subutilização','grafico_C_subutilizacao.png','% da força de trabalho ampliada'),
    ('subocupacao_horas_pct','C — Subocupação por insuficiência de horas','grafico_C_subocupacao.png','% da população ocupada')]:
    fig,ax=canvas(title,ylabel)
    pre=(wide.Periodo <= '201509')
    post=(wide.Periodo >= '201512')
    ax.plot(dates[pre],wide.loc[pre,col],label='Regime anterior a out/2015')
    ax.plot(dates[post],wide.loc[post,col],label='Regime a partir de dez/2015')
    ax.axvspan(pd.Timestamp('2015-10-01'),pd.Timestamp('2015-11-30'),alpha=.15)
    finalize(fig,ax,fn,'Fonte: IBGE/SIDRA. Out–nov/2015 sem divulgação; ruptura conceitual, séries não emendadas.')

fig,ax=canvas('C — Percentual de desalento','% da força de trabalho ou desalentados')
ax.plot(dates,wide.desalento_pct,label='Desalento')
finalize(fig,ax,'grafico_C_desalento.png','Fonte: IBGE/SIDRA. Percentual e total de desalentados têm escalas distintas.')

fig,ax=canvas('D — Informalidade e contribuição previdenciária','% de ocupados (definições distintas)')
ax.plot(dates,wide.informalidade_pct,label='Informalidade (desde dez/2015)')
ax.plot(dates,wide.contrib_previdencia_pct,label='Contribuintes previdência')
finalize(fig,ax,'grafico_D_informalidade_previdencia.png','Fonte: IBGE/SIDRA. As proporções NÃO são complementares; não inferir informalidade pré-201512.')

fig,ax=canvas('E — Rendimento médio mensal real habitual','R$ na referência oficial da extração')
ax.plot(dates,wide.rendimento_real_habitual_rs,label='Rendimento médio real habitual')
finalize(fig,ax,'grafico_E_rendimento_real.png','Fonte: IBGE/SIDRA. Série real na mesma vintage oficial; não houve nova deflação.')

result = dict(etapa='E05', natureza='EXE no ambiente Python local; descrição, NÃO inferência',
            base_longa_sha256=sha256(IN_LONG),base_larga_sha256=sha256(IN_WIDE),
            manifest_sha256_itens_verificados=len(immutable), registros_vivos_modificaveis=len(LIVE), python=platform.python_version(),
            pandas=pd.__version__, matplotlib=matplotlib.__version__,
            num_indicadores_principais=13,num_apoio=1,num_periodos=175,
            num_linhas_base=2405,ausencias_numericas=4,
            indicadores_com_ausencias=['subutilizacao_pct','subocupacao_horas_pct'],
            periodos_com_ausencias=['201510','201511'], informalidade_primeiro='201512',
            periodos_contraste=['202509','202606','202609'],
            exemplo_taxa_desocupacao_recomposta=100*num2026/den2026,
            desocupacao_CV_202609_percentual=float(wide_ix.loc['202609','cv_desocupacao_pct']),
            desocupacao_SE_202609_pontos_percentuais=float(wide_ix.loc['202609','desocupacao_pct']*wide_ix.loc['202609','cv_desocupacao_pct']/100),
            limitações=['CV marginais não identificam covariâncias dos contrastes',
                      'Trimestres móveis e desenho rotativo induzem dependência',
                      'Ruptura de conceito de horas em 2015',
                      'Informalidade sem publicação anterior a dezembro/2015',
                      'Descritivos temporais não são distribuições entre indivíduos',
                      'Variações pontuais não são posteriores nem evidência causal'],
            figuras=[p.name for p in sorted(OUT.glob('grafico_*.png'))])
(OUT/'auditoria_e05.json').write_text(json.dumps(result,ensure_ascii=False,indent=2),encoding='utf-8')
print('E05 EXE OK:',len(immutable),'hashes de arquivos imutáveis,',len(changes),'indicadores,',len(wide),'períodos,',len(result['figuras']),'gráficos')
print('\nCONTRASTES PRINCIPAIS (est. IBGE; valores e deltas NÃO posteriores):')
print(changes[changes.Papel=='principal'][['Eixo','IndicadorId','Valor202509','Valor202606','Valor202609','DeltaAnual','DeltaRecente','UnidadeDelta']].to_string(index=False))
print('\nEXEMPLO TAXA RECOMPOSTA',result['exemplo_taxa_desocupacao_recomposta'],'CV (%)', result['desocupacao_CV_202609_percentual'])
