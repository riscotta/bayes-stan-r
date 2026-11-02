#!/usr/bin/env python3
"""E08.4 — auditoria focal de identidades, contrastes e limites de identificabilidade.

Entradas preservadas E04, E05, E08.2 e E08.3. Nao roda Stan/MCMC.
Transcricao focal de linhas do Quadro Sintetico IBGE outubro/2026, p.1-2:
  dados/03_documentos/pnadc_202609_quadroSintetico.pdf
A transcricao e documental; os resultados numericos de sensibilidade continuam
CENARIOS CONDICIONAIS, sem recuperacao das covariancias de desenho.
"""
from pathlib import Path
import pandas as pd
import numpy as np
import hashlib
import json

ROOT = Path(__file__).resolve().parents[1]
E08 = ROOT / 'e08'
E04 = ROOT / 'e04' / 'saida' / 'base_analitica_e04_longa.csv'
E05 = ROOT / 'e05' / 'saida' / 'contrastes_e05.csv'
E082 = E08 / 'cenarios_contrastes_e082.csv'
E083P = E08 / 'sensibilidade_priors_contrastes_e083.csv'
E083R = E08 / 'sensibilidade_arredondamento_e083.csv'

sources=[E04,E05,E082,E083P,E083R]
sha={p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in sources}
a=pd.read_csv(E04,sep=';',encoding='utf-8-sig')
contr=pd.read_csv(E05,sep=';',encoding='utf-8-sig')
sc=pd.read_csv(E082,sep=';',encoding='utf-8-sig')
sp=pd.read_csv(E083P,sep=';',encoding='utf-8-sig')
sr=pd.read_csv(E083R,sep=';',encoding='utf-8-sig')
periods=[202509,202606,202609]
sub=a[a.Periodo.isin(periods)]
assert len(sub)==42 and len(sub[sub.Papel=='principal'])==39
assert sub[['Eixo','IndicadorId','Periodo']].duplicated().sum()==0
assert len(sc)==104 and len(sp)==416 and len(sr)==104
main=contr[(contr.Papel=='principal')].copy()
assert len(main)==13

# Contagens auxiliares lidas manualmente do Quadro Sintetico do IBGE, p. 2.
# Unidades: mil pessoas; transcritas sem alterar as bases E04/E05.
official_p2={
 202509: dict(potential=5224, subocupadas=4535, underused=15804,
               forca_ampliada=113702, forca_mais_desalentados=111115),
 202606: dict(potential=4723, subocupadas=4075, underused=14659,
               forca_ampliada=113641, forca_mais_desalentados=111207),
 202609: dict(potential=4930, subocupadas=4288, underused=14846,
               forca_ampliada=114196, forca_mais_desalentados=111725),
}

# Identidades para 3 periodos; tolerancia estritamente ligada a casas publicadas.
ident=[]
for period in periods:
    r=sub[sub.Periodo==period].set_index('IndicadorId').Valor
    official=official_p2[period]
    pop=float(r['populacao_14mais_mil'])
    lab=float(r['forca_trabalho_mil'])
    occ=float(r['ocupados_mil'])
    un=float(r['desocupados_mil'])
    des=float(r['desalentados_mil'])
    counts={
      'forca_igual_ocupados_mais_desocupados': (lab,occ+un,'mil pessoas',1.0),
      'forca_ampliada_igual_forca_mais_potencial': (official['forca_ampliada'],lab+official['potential'],'mil pessoas',1.0),
      'forca_mais_desalentados_igual_forca_mais_desalentados': (official['forca_mais_desalentados'],lab+des,'mil pessoas',1.0),
      'subutilizados_igual_desocupados_mais_subocupados_mais_potencial': (official['underused'],un+official['subocupadas']+official['potential'],'mil pessoas',2.0),
    }
    rates={
      'taxa_desocupacao': (r['desocupacao_pct'],100*un/lab,'p.p.',0.051),
      'nivel_ocupacao': (r['nivel_ocupacao_pct'],100*occ/pop,'p.p.',0.051),
      'taxa_participacao': (r['participacao_pct'],100*lab/pop,'p.p.',0.051),
      'percentual_desalento': (r['desalento_pct'],100*des/(lab+des),'p.p.',0.051),
      'taxa_subocupacao': (r['subocupacao_horas_pct'],100*official['subocupadas']/occ,'p.p.',0.051),
      'taxa_subutilizacao': (r['subutilizacao_pct'],100*official['underused']/official['forca_ampliada'],'p.p.',0.051),
    }
    for nome,(pub,rec,unidade,tol) in {**counts,**rates}.items():
        err=round(float(pub-rec),10)
        ident.append(dict(Periodo=period,Identidade=nome,ValorPublicado=pub,Reconstrucao=rec,
                          Diferenca=err,Unidade=unidade,Tolerancia=tol,Confere=(abs(err)<=tol)))
ident=pd.DataFrame(ident)
assert len(ident)==30 and ident.Confere.all(),ident[~ident.Confere].to_string()

# Releitura documental de duas colunas 'Diferenca' do Quadro Sintetico (p.1 e p.2).
# NA (D) = indicadores nao presentes como a mesma taxa neste Quadro, nao invalidez.
flags={
 'desocupacao_pct':(-0.4,-0.2,'caiu','caiu'),
 'desocupados_mil':(-417,-234,'caiu','caiu'),
 'forca_trabalho_mil':(788,347,'cresceu','cresceu'),
 'nivel_ocupacao_pct':(0.2,0.2,'estavel','cresceu'),
 'ocupados_mil':(1205,581,'cresceu','cresceu'),
 'participacao_pct':(0.0,0.1,'estavel','estavel'),
 'desalentados_mil':(-178,171,'caiu','cresceu'),
 'desalento_pct':(-0.2,0.1,'caiu','cresceu'),
 'subocupacao_horas_pct':(-0.3,0.2,'caiu','cresceu'),
 'subutilizacao_pct':(-0.9,0.1,'caiu','estavel'),
 'rendimento_real_habitual_rs':(131,37,'cresceu','estavel')
}
ref=[]
for _,r in main.iterrows():
  ind=r.IndicadorId
  if ind not in flags: continue
  pub_an,pub_re,st_an,st_re=flags[ind]
  for contraste,diff_pub,diff_rounded,st in [('anual',pub_an,float(r.DeltaAnual),st_an),
                                            ('recente',pub_re,float(r.DeltaRecente),st_re)]:
    ref.append(dict(Eixo=r.Eixo,IndicadorId=ind,Contraste=contraste,
                    DeltaNosValoresArredondados=diff_rounded,
                    DeltaPublicadoNoQuadro=diff_pub,
                    DivergenciaEntreDeltas=round(diff_pub-diff_rounded,8),
                    SituacaoOficial=st,
                    NaturezaSituacao='Teste oficial de significancia, NAO probabilidade bayesiana'))
ref=pd.DataFrame(ref)
assert len(ref)==22
assert (ref.DivergenciaEntreDeltas.abs()<=1.001).all(),ref.to_string()

# Cenário hipotético ±0,05% em P14: q mantido fixo = y/P14 no ponto publicado.
# Nao representa intervalo de incerteza do total projetado nem nova posterior.
cal=[]
for _,r in main[main.IndicadorId.str.endswith('_mil')].iterrows():
  for contraste,base in [('anual',202509),('recente',202606)]:
    yp=float(r.Valor202609)
    y0=float(r.Valor202509 if contraste=='anual' else r.Valor202606)
    nominal=yp-y0
    # q fixo, ambos totais da P14 perturbados em sentidos extremos
    k=.0005
    lo=nominal-k*(yp+y0)
    hi=nominal+k*(yp+y0)
    cal.append(dict(Eixo=r.Eixo,IndicadorId=r.IndicadorId,Contraste=contraste,
      DeltaNominalMilPessoas=nominal,MinDeltaCenarioMilPessoas=lo,MaxDeltaCenarioMilPessoas=hi,
      AmplitudeSemilarguraMilPessoas=k*(yp+y0),
      PreservaSinalNominal=bool(lo*hi>0),
      Nota='HIP: ±0,05% no total calibrado em cada periodo, q fixo. NAO e limite estatistico da P14.'))
cal=pd.DataFrame(cal)
assert len(cal)==8 and cal.PreservaSinalNominal.all()

# Matriz 13 indicadores x contrastes: probabilidades condicionais e envelope E08.3.
summary=[]
for _,r in main.iterrows():
  ind=r.IndicadorId; eixo=r.Eixo
  for contraste in ['anual','recente']:
    s=sc[(sc.IndicadorId==ind)&(sc.Contraste==contraste)]
    rr=sr[(sr.IndicadorId==ind)&(sr.Contraste==contraste)]
    pp=sp[(sp.IndicadorId==ind)&(sp.Contraste==contraste)&(sp.Prior.isin(['base','dispersa','moderada']))]
    assert len(s)==len(rr)==4 and len(pp)==12
    y0=float(r.Valor202509 if contraste=='anual' else r.Valor202606)
    y1=float(r.Valor202609)
    eps=.05 if ind.endswith('_pct') else .5
    low=(y1-y0)-2*eps
    high=(y1-y0)+2*eps
    st=flags.get(ind)
    s_off=(st[2] if contraste=='anual' else st[3]) if st is not None else 'nao consta'
    diff_off=(st[0] if contraste=='anual' else st[1]) if st is not None else np.nan
    amp_prior=max(float(group.ProbSentido_NormMomentos.max()-group.ProbSentido_NormMomentos.min()) for _,group in pp.groupby('RhoHipotetico'))
    summary.append(dict(Eixo=eixo,IndicadorId=ind,Indicador=r.Indicador,
          Contraste=contraste,DeltaValoresArredondados=y1-y0,
          DeltaMinCompativelArredondamento=low,DeltaMaxCompativelArredondamento=high,
          SinalArredondamentoEstrito=('positivo' if low>1e-8 else 'negativo' if high < -1e-8 else 'indeterminado_ate_zero'),
          DeltaQuadroOficial=diff_off,SituacaoQuadro=s_off,
          MinProbCenariosRho=rr.ProbNominal.min(),MaxProbCenariosRho=rr.ProbNominal.max(),
          MinProbCenariosRhoComArredondamento=rr.ProbMinExtremos.min(),
          MaxProbCenariosRhoComArredondamento=rr.ProbMaxExtremos.max(),
          AmplitudeProbPriorsRegulares_pp=100*amp_prior,
          Interpretacao='CENARIO_CONDICIONAL_SEM_COVARIANCIAS_IDENTIFICADAS'))
summary=pd.DataFrame(summary)
assert len(summary)==26

# 5 proxies por eixo, explicitamente NAO equivalentes ao contrato integral nem desfecho global.
proxy_ids=['desocupacao_pct','nivel_ocupacao_pct','subutilizacao_pct','informalidade_pct','rendimento_real_habitual_rs']
frechet=[]
for contraste in ['anual','recente']:
  for rho in [-.5,0.,.5,.9]:
    r=sc[(sc.IndicadorId.isin(proxy_ids))&(sc.Contraste==contraste)&(sc.RhoHipotetico==rho)]
    assert len(r)==5
    probs=r.ProbSentido.to_numpy()
    frechet.append(dict(Contraste=contraste,RhoTemporalHipotetico=rho,
     LimiteInferiorFrechet=max(0.,float(probs.sum())-4.),
     LimiteSuperiorFrechet=float(probs.min()),
     Nota='APENAS 5 eventos proxy, dados rho temporal hipotetico; dependencia entre indicadores nao identificada'))
frechet=pd.DataFrame(frechet)

for filename,frame in [('identidades_e084.csv',ident),('confronto_quadro_e084.csv',ref),
                        ('calibracao_hipotetica_e084.csv',cal),('sintese_eixos_e084.csv',summary),
                        ('limites_conjuntos_proxy_e084.csv',frechet)]:
    frame.to_csv(E08/filename,index=False,sep=';',encoding='utf-8-sig',float_format='%.9f')

log={
 'etapa':'E08.4', 'natureza':'EXE Python, DOC PDFs locais conferidos visualmente, HIP cenarios condicionais',
 'arquivos_entrada_SHA256':sha,
 'checks_identidades':len(ident),'checks_identidades_sucesso':int(ident.Confere.sum()),
 'contrastes_pdf_confrontados':len(ref),
 'contrastes_pdf_diferentes_das_subtracoes_arredondadas':int((ref.DivergenciaEntreDeltas.abs()>1e-7).sum()),
 'linhas_calibracao':len(cal),'sinais_preservados_hipotese_calibracao':int(cal.PreservaSinalNominal.sum()),
 'linhas_sintese':len(summary),'linhas_limites_frechet':len(frechet),
 'mcmc_novo':False,'refit_stan':False,
 'limitacoes':['Nao ha rho temporal ou covariancia entre indicadores identificada',
               'O Quadro Sintetico indica movimentos por testes de significancia, nao posterior',
               'P14 constante no modelo condicional nao implica populacao sem erro',
               'Nao ha posterior conjunta do evento de melhora em todas as dimensoes',
               'Transcricao documental manual de poucas linhas do Quadro Sintetico'],
}
(E08/'log_e084.json').write_text(json.dumps(log,ensure_ascii=False,indent=2),encoding='utf-8')
print('E08.4 OK')
print('Identidades',len(ident),'checadas;',ident.Confere.sum(),'passaram')
print('Contrastes quadro',len(ref),', discrepantes por arredondamento',log['contrastes_pdf_diferentes_das_subtracoes_arredondadas'])
print(ref[ref.DivergenciaEntreDeltas.abs()>1e-7][['IndicadorId','Contraste','DeltaNosValoresArredondados','DeltaPublicadoNoQuadro']].to_string(index=False))
print('Calibracao')
print(cal.to_string(index=False,columns=['IndicadorId','Contraste','DeltaNominalMilPessoas','MinDeltaCenarioMilPessoas','MaxDeltaCenarioMilPessoas']))
print('Limites Frechet para cinco proxies')
print(frechet.to_string(index=False))
