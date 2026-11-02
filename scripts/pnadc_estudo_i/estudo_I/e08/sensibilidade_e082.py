"""E08.2: Gaussian conditional scenario contrasts for 13 approved PNADC aggregated indicators.

No refitting, no new MCMC; uses E04 and the stored E07 posterior summaries solely
for an independent marginal consistency check. Scenario rho is NOT estimated.
"""
from pathlib import Path
import numpy as np
import pandas as pd
from scipy.stats import norm

ROOT = Path(__file__).resolve().parents[1]
PTH = ROOT/'e04'/'saida'/'base_analitica_e04_longa.csv'
CONTRASTES = ROOT/'e05'/'saida'/'contrastes_e05.csv'
E081 = ROOT/'e08'/'diagnosticos_e081.csv'
OUT = ROOT/'e08'/'cenarios_contrastes_e082.csv'
CHECK = ROOT/'e08'/'checagem_marginais_e082.csv'
LIMS = ROOT/'e08'/'limites_temporais_e082.csv'
LOG = ROOT/'e08'/'log_e082.txt'
DATES = {'anual': (202509, 202609), 'recente': (202606, 202609)}
RHOS = (-0.5, 0.0, 0.5, 0.9)
NEGATIVE = {'desocupacao_pct','desocupados_mil','desalentados_mil','desalento_pct',
            'subocupacao_horas_pct','subutilizacao_pct','informalidade_pct'}

df = pd.read_csv(PTH,sep=';',encoding='utf-8-sig')
df = df[(df.Papel=='principal') & (df.Periodo.isin((202509,202606,202609)))].copy()
if len(df)!=39 or df.IndicadorId.nunique()!=13:
    raise AssertionError(f'Expected 39 records, 13 indicators; got {len(df)} records')
assert df[['Eixo','IndicadorId','Periodo']].duplicated().sum()==0
assert df[['Valor','CVPercentual']].notna().all().all()
assert (df.CVPercentual>0).all()
df['s'] = df.Valor.abs()*df.CVPercentual/100
old = pd.read_csv(CONTRASTES,sep=';',encoding='utf-8-sig')
diag = pd.read_csv(E081,sep=';')
rows = []; check = []; limits = []
for (axis,ind),sub in df.groupby(['Eixo','IndicadorId'],sort=True):
    sub = sub.set_index('Periodo')
    e07 = diag.loc[diag.folder==f'{axis}_{ind}_202609']
    assert len(e07)==1,f'Unmatched E07: {axis}, {ind}'
    e07 = e07.iloc[0]
    assert abs(e07.y - sub.loc[202609,'Valor'])<1e-9
    assert abs(e07.s - sub.loc[202609,'s']) < 1e-8
    check.append(dict(Eixo=axis,IndicadorId=ind,ValorE04=sub.loc[202609,'Valor'],
                      SE_E04=sub.loc[202609,'s'],MediaE07=e07.theta_mean,
                      DP_E07=e07.theta_sd,
                      Diferenca_Media_Em_SE=(e07.theta_mean-sub.loc[202609,'Valor'])/sub.loc[202609,'s'],
                      Razao_DP_E07_E04=e07.theta_sd/sub.loc[202609,'s']))
    pdir = -1 if ind in NEGATIVE else 1
    for type_,(past,current) in DATES.items():
        y0,y1 = (float(sub.loc[t,'Valor']) for t in (past,current))
        s0,s1 = (float(sub.loc[t,'s']) for t in (past,current))
        delta = y1-y0
        ex = old[old.IndicadorId==ind]
        assert len(ex)==1
        expected = ex.iloc[0]['DeltaAnual' if type_=='anual' else 'DeltaRecente']
        assert abs(delta-expected)<1e-6,(ind,type_)
        for rho in RHOS:
            sd = np.sqrt(s0*s0+s1*s1-2*rho*s0*s1)
            prob_positive = float(norm.cdf(delta/sd))
            prob_direction = prob_positive if pdir==1 else 1-prob_positive
            rows.append(dict(Eixo=axis,IndicadorId=ind,Indicador=sub.iloc[0].Indicador,
                  Contraste=type_,PeriodoOrigem=past,PeriodoDestino=current,
                  ValorOrigem=y0,ValorDestino=y1,
                  CVOrigem=float(sub.loc[past,'CVPercentual']),
                  CVDestino=float(sub.loc[current,'CVPercentual']),
                  SEOrigem=s0,SEDestino=s1,Delta=delta,Unidade=sub.iloc[0].UnidadeAnalitica,
                  RhoHipotetico=rho,SDDeltaCondicional=sd,
                  IC95Inf=delta-1.959963984540054*sd,
                  IC95Sup=delta+1.959963984540054*sd,
                  ProbDeltaPositivo=prob_positive,
                  SentidoIndicador='menor' if pdir==-1 else 'maior',
                  ProbSentido=prob_direction,
                  Classificacao='CENARIO_CONDICIONAL'))
        # rho = +/-1 are limiting singular Gaussian cases: report only SD bounds,
        # not exact probabilities at a singular covariance matrix.
        sd_min = abs(s1-s0)
        sd_max = s1+s0
        # P in preferred direction decreases as SD increases when favorable delta>0.
        # Reverse if observed movement unfavorable.
        eps=1e-13
        ps=[float(norm.cdf(pdir*delta/s)) if s>eps else (float(pdir*delta>0) if delta!=0 else 0.5)
            for s in [sd_min, sd_max]]
        limits.append(dict(Eixo=axis,IndicadorId=ind,Contraste=type_,Delta=delta,
                MinDP=np.abs(s1-s0),MaxDP=s1+s0,
                InfProbSentidoAbre=round(min(ps),10),SupProbSentidoAbre=round(max(ps),10),
                Nota='bounds for limiting rho in (-1,1); not an empirical rho interval'))
res = pd.DataFrame(rows).sort_values(['Eixo','IndicadorId','Contraste','RhoHipotetico'])
ck = pd.DataFrame(check)
limits = pd.DataFrame(limits).sort_values(['Eixo','IndicadorId','Contraste'])
res.to_csv(OUT,index=False,sep=';',decimal='.',float_format='%.10f',encoding='utf-8-sig')
ck.to_csv(CHECK,index=False,sep=';',decimal='.',float_format='%.10f',encoding='utf-8-sig')
limits.to_csv(LIMS,index=False,sep=';',decimal='.',float_format='%.10f',encoding='utf-8-sig')
main = res.pivot(index=['Eixo','IndicadorId','Contraste','Delta'],columns='RhoHipotetico',values='ProbSentido').reset_index()
with LOG.open('w',encoding='utf-8') as f:
    f.write('E08.2; conditional Gaussian pairwise posterior scenario; NO new MCMC\n')
    f.write(f'Input records={len(df)}; indicators={df.IndicadorId.nunique()}; contrasts={len(limits)}; scenarios={len(res)}\n')
    f.write(f'Existing E07 posterior SD / E04 s min/max: {ck.Razao_DP_E07_E04.min():.5f}/{ck.Razao_DP_E07_E04.max():.5f}\n')
    f.write(f'Max |E07 mean minus E04 estimate| / s = {ck.Diferenca_Media_Em_SE.abs().max():.5f}\n')
    f.write('\nMain table: P(change toward nominated direction), NOT P(all indicators improve)\n')
    f.write(main.to_string(index=False,float_format=lambda x:f'{x:.4f}')+'\n')
print(LOG.read_text(encoding='utf-8'))
print('FILES: ',*[str(p) for p in [OUT,CHECK,LIMS,LOG]])
