#!/usr/bin/env python3
"""E08.3 — análise determinística de sensibilidade, sem Stan/MCMC novo.

Requer pandas, numpy, scipy. Usa apenas E04, E07 corrigido e E08.2 já existentes.
As priors alternativas e limites de arredondamento são HIPÓTESES, não dados IBGE.
"""
from pathlib import Path
import platform
import numpy as np
import pandas as pd
import scipy
from scipy.special import roots_hermitenorm, logsumexp, betaln
from scipy.stats import norm

ROOT=Path(__file__).resolve().parents[1]
E08=ROOT/'e08'
E04=ROOT/'e04'/'saida'/'base_analitica_e04_longa.csv'
E07=ROOT/'e07'/'resultados'/'completo_20261102_124430_17284'/'resumo_marginais.csv'
E082=E08/'cenarios_contrastes_e082.csv'
RHOS=(-0.5,0.0,0.5,0.9)
CONTRASTES={'anual':(202509,202609),'recente':(202606,202609)}
PRIORS={'base':(1.0,1.0,10000.0), 'dispersa':(0.5,0.5,20000.0),
        'moderada':(2.0,2.0,5000.0), 'adversarial':(20.0,20.0,1000.0)}
NEG={'desocupacao_pct','desocupados_mil','desalentados_mil','desalento_pct',
     'subocupacao_horas_pct','subutilizacao_pct','informalidade_pct'}

df=pd.read_csv(E04,sep=';',encoding='utf-8-sig')
df=df[df.Periodo.isin((202509,202606,202609))].copy()
assert len(df[(df.Papel=='principal')])==39
support=df[df.Papel=='apoio'].set_index('Periodo')
print('Support indicators',support[['IndicadorId','Valor']].to_string())
assert len(support)==3 and support.IndicadorId.nunique()==1
base=df[df.Papel=='principal'].copy()
assert not base[['Valor','CVPercentual']].isna().any().any()
assert (base.CVPercentual>0).all()
base['s']=base.Valor.abs()*base.CVPercentual/100
base['P14']=base.Periodo.map(support.Valor.to_dict())
assert base.P14.notna().all()
base['tipo']=np.where(base.IndicadorId.str.endswith('_pct'),'taxa',
                       np.where(base.IndicadorId.str.endswith('_mil'),'contagem','rendimento'))
base=base.set_index(['IndicadorId','Periodo']).sort_index()
old=pd.read_csv(E082,sep=';',encoding='utf-8-sig')
corrected=pd.read_csv(E07,sep=';')
assert len(corrected)==13

# GH smooth integration under independent normal reference factors.
z,w=roots_hermitenorm(101)
w=w/np.sqrt(2*np.pi)
logw=np.log(w)

def logprior(theta, tipo, p14, prior):
    a,b,scale=PRIORS[prior]
    if tipo=='taxa':
        frac=theta/100.0
        valid=(frac>0)&(frac<1)
    elif tipo=='contagem':
        frac=theta/p14
        valid=(frac>0)&(frac<1)
    else:
        valid=theta>0
    out=np.full(np.shape(theta),-np.inf,dtype=float)
    if tipo=='rendimento':
        out[valid]=-0.5*(theta[valid]/scale)**2-np.log(scale)
    else:
        q=frac[valid]
        out[valid]=(a-1)*np.log(q)+(b-1)*np.log1p(-q)-betaln(a,b)
    return out

def marginal(row,prior):
    yy=float(row.Valor); ss=float(row.s)
    th=yy+ss*z
    lp=logprior(th,row.tipo,float(row.P14),prior)
    lw=logw+lp
    lw-=logsumexp(lw)
    ww=np.exp(lw)
    mean=float(ww@th)
    sd=float(np.sqrt(ww@((th-mean)**2)))
    return mean,sd

def contrast_posterior(row0,row1,prior,rho):
    y0,s0=float(row0.Valor),float(row0.s)
    y1,s1=float(row1.Valor),float(row1.s)
    zz0=z[:,None]; zz1=z[None,:]
    th0=np.broadcast_to(y0+s0*zz0,(len(z),len(z)))
    th1=y1+s1*(rho*zz0+np.sqrt(1-rho*rho)*zz1)
    lw=(logw[:,None]+logw[None,:]
        +logprior(th0,row0.tipo,float(row0.P14),prior)
        +logprior(th1,row1.tipo,float(row1.P14),prior))
    lw-=logsumexp(lw)
    ww=np.exp(lw)
    delta=th1-th0
    mean=float(np.sum(ww*delta))
    sd=float(np.sqrt(np.sum(ww*(delta-mean)**2)))
    # Smooth-posterior delta is approximately normal at the resolution of these aggregates.
    # The Gaussian CDF is applied to its computed first two moments; this is NOT
    # a full posterior CDF under nonuniform priors.
    return mean,sd

marg=[]
for (ind,t),r in base.iterrows():
    for pr in PRIORS:
        mu,sd=marginal(r,pr)
        marg.append({'Eixo':r.Eixo,'IndicadorId':ind,'Periodo':t,'Prior':pr,
                     'Estimativa':r.Valor,'SE_Oficial_Aproximado':r.s,
                     'MediaPosteriorAnalitica':mu,'DPPosteriorAnalitico':sd,
                     'DeslocamentoDaEstimativa':mu-r.Valor,
                     'Classe':'CENARIO_CONDICIONAL_PRIOR'})
marg=pd.DataFrame(marg)
mm=marg[(marg.Periodo==202609)].copy()
m0=mm[mm.Prior=='base'].merge(corrected,on='IndicadorId',suffixes=('','_e07'))
assert len(m0)==13
m0['DifMediaEmMCSE']= (m0.MediaPosteriorAnalitica-m0['mean'])/(m0['sd']/np.sqrt(m0['ess_bulk']))
print('E07 validation mean max MCSE ratio',m0.DifMediaEmMCSE.abs().max())

contr=[]
for (ind,t),r1 in base.iterrows():
    if t!=202609: continue
    for typ,(t0,t1) in CONTRASTES.items():
        r0=base.loc[(ind,t0)]
        sign=-1 if ind in NEG else 1
        for rho in RHOS:
            baseline=old[(old.IndicadorId==ind)&(old.Contraste==typ)&np.isclose(old.RhoHipotetico,rho)].iloc[0]
            for pr in PRIORS:
                mu,sd=contrast_posterior(r0,r1,pr,rho)
                pp=float(norm.cdf(sign*mu/sd))
                contr.append({'Eixo':r1.Eixo,'IndicadorId':ind,'Contraste':typ,
                    'Prior':pr,'RhoHipotetico':rho,'DeltaObservado':r1.Valor-r0.Valor,
                    'MediaDelta':mu,'DPDelta':sd,'ProbSentido_NormMomentos':pp,
                    'ProbE082Base':baseline.ProbSentido,
                    'Aproximacao':'Posterior bivariada sob prior via GH; CDF delta aprox Normal momentos'})
contr=pd.DataFrame(contr)
assert len(contr)==26*len(RHOS)*len(PRIORS)

# Publication rounding as deterministic intervals, not a data-generating distribution.
# Endpoint scan 2^4: reported values 1 decimal for rates or integers for counts/rendimentos;
# all CV values one decimal of PERCENTAGE POINT of CV. Values in interval ±0.05/±0.5.
from itertools import product
rnd=[]
for ind in base.index.get_level_values(0).unique():
    r1=base.loc[(ind,202609)]
    for typ,(t0,t1) in CONTRASTES.items():
        r0=base.loc[(ind,t0)]
        y0,y1=(float(r.Valor) for r in (r0,r1))
        c0,c1=(float(r.CVPercentual) for r in (r0,r1))
        erry=0.05 if r1.tipo=='taxa' else 0.5
        errc=0.05
        direction=-1 if ind in NEG else 1
        for rho in RHOS:
            vals=[]; cvonly=[]; yonly=[]
            for e0,e1,f0,f1 in product((-1,1),repeat=4):
                yy0=y0+e0*erry; yy1=y1+e1*erry
                cc0=c0+f0*errc; cc1=c1+f1*errc
                assert cc0>0 and cc1>0
                def pf(v0,v1,cv0,cv1):
                    ss0=abs(v0)*cv0/100; ss1=abs(v1)*cv1/100
                    sd=np.sqrt(max(0,ss0**2+ss1**2-2*rho*ss0*ss1))
                    return float(norm.cdf(direction*(v1-v0)/sd))
                vals.append(pf(yy0,yy1,cc0,cc1))
                if e0==1 and e1==1:
                    cvonly.append(pf(y0,y1,cc0,cc1))
                if f0==1 and f1==1:
                    yonly.append(pf(yy0,yy1,c0,c1))
            nom=old[(old.IndicadorId==ind)&(old.Contraste==typ)&np.isclose(old.RhoHipotetico,rho)].iloc[0]
            rnd.append({'Eixo':r1.Eixo,'IndicadorId':ind,'Contraste':typ,'RhoHipotetico':rho,
                'Sentido':nom.SentidoIndicador,'DeltaPublicado':y1-y0,
                'MenorDeltaCompativel':y1-y0-2*erry,'MaiorDeltaCompativel':y1-y0+2*erry,
                'ProbNominal':nom.ProbSentido,
                'ProbMinExtremos':min(vals),'ProbMaxExtremos':max(vals),
                'ProbMinApenasValor':min(yonly),'ProbMaxApenasValor':max(yonly),
                'ProbMinApenasCV':min(cvonly),'ProbMaxApenasCV':max(cvonly),
                'Nota':'Envelope de 16 cantos, não bounds rigorosos interiores nem distribuição do arredondamento'})
rnd=pd.DataFrame(rnd)
assert len(rnd)==26*len(RHOS)

# Verify E082 baseline bivariate GH moments against analytic normal values
cc=contr[contr.Prior=='base'].merge(old,on=['Eixo','IndicadorId','Contraste','RhoHipotetico'])
print('E082 GH base max mean delta error',np.max(abs(cc.MediaDelta-cc.Delta)))
print('E082 GH base max SD ratio diff',np.max(abs(cc.DPDelta/cc.SDDeltaCondicional-1)))

for name, frame in [('priors_marginais_e083.csv',marg),
     ('sensibilidade_priors_contrastes_e083.csv',contr),
     ('sensibilidade_arredondamento_e083.csv',rnd)]:
    frame.to_csv(E08/name,index=False,sep=';',float_format='%.10g',encoding='utf-8-sig')

summary=[]
for ind in base.index.get_level_values(0).unique():
    sub=contr[(contr.IndicadorId==ind)&(contr.Contraste=='recente')&(contr.RhoHipotetico==0)]
    ran=rnd[(rnd.IndicadorId==ind)&(rnd.Contraste=='recente')&(rnd.RhoHipotetico==0)].iloc[0]
    b=sub[sub.Prior=='base'].iloc[0]
    subset=sub[sub.Prior!='adversarial']
    summary.append({'Eixo':b.Eixo,'IndicadorId':ind,'DeltaRecente':b.DeltaObservado,
        'ProbNominal':ran.ProbNominal,'ProbMinArredondamento':ran.ProbMinExtremos,
        'ProbMaxArredondamento':ran.ProbMaxExtremos,
        'AmplitudeArredondamento_pp':100*(ran.ProbMaxExtremos-ran.ProbMinExtremos),
        'ProbMinPriorsRegulares':subset.ProbSentido_NormMomentos.min(),
        'ProbMaxPriorsRegulares':subset.ProbSentido_NormMomentos.max(),
        'AmplitudePriorsRegulares_pp':100*(subset.ProbSentido_NormMomentos.max()-subset.ProbSentido_NormMomentos.min()),
        'ProbAdversarial':float(sub[sub.Prior=='adversarial'].ProbSentido_NormMomentos.iloc[0])})
summary=pd.DataFrame(summary).sort_values(['Eixo','IndicadorId'])
summary.to_csv(E08/'quadro_eixos_e083.csv',index=False,sep=';',float_format='%.10g',encoding='utf-8-sig')

log=[]
log.append(f'Python {platform.python_version()}, numpy {np.__version__}, pandas {pd.__version__}, scipy {scipy.__version__}')
log.append('E08.3: NOT new MCMC/Stan; GH deterministic posterior moments for priors; rounding endpoint scenarios.')
log.append('Priors rates and counts: base Beta(1,1), dispersed Beta(0.5,0.5), moderate Beta(2,2), adversarial Beta(20,20).')
log.append('Priors income: HalfNormal scales 10000, 20000, 5000, 1000 BRL, respectively.')
log.append('No assertion of prior validity for economically implausible adversarial case.')
log.append(f'Source rows: {len(base)}, marginal scenarios {len(marg)}, prior scenarios {len(contr)}, rounding scenarios {len(rnd)}')
log.append(f'GH reference max diff from E082 P rho baseline: {np.max(abs(cc.ProbSentido_NormMomentos-cc.ProbSentido)):.8f}')
log.append('Note rounding bounds are endpoint scans, not rigorous global optimization or probability models.')
log.append('NOTE prior P directions approximated by moment-fitted normal for delta (not exact tail integrals).')
log.append('\nRecent rho0 summary:\n'+summary.to_string(index=False,float_format=lambda x:f'{x:.5f}'))
(E08/'log_e083.txt').write_text('\n'.join(log)+'\n',encoding='utf-8')
print('\n'.join(log))
