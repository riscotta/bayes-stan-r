from pathlib import Path
import pandas as pd
import numpy as np
import arviz as az

root=Path(__file__).resolve().parents[2]
results=root/'estudo_I'/'e07'/'resultados'/'completo_20261102_124430_17284'
base=pd.read_csv(root/'estudo_I'/'e04'/'saida'/'base_analitica_e04_longa.csv',sep=';',low_memory=False)
print('Base columns',base.columns.tolist())
print('Base shape',base.shape)
# Original ZIP integrity was checked separately before this analysis.
rows=[]
for folder in sorted(x for x in results.iterdir() if x.is_dir()):
    chains=sorted(folder.glob('chains_*.csv'))
    assert len(chains)==4,(folder,len(chains))
    arr=[pd.read_csv(p,comment='#') for p in chains]
    assert len(set(len(a) for a in arr))==1 and len(arr[0])==750
    diag=np.stack([a['theta'].to_numpy() for a in arr],axis=0)
    energy=np.stack([a['energy__'].to_numpy() for a in arr],axis=0)
    yrep=np.stack([a['y_rep'].to_numpy() for a in arr],axis=0)
    diverg=sum(int(a['divergent__'].sum()) for a in arr)
    max_depth=int(max(a['treedepth__'].max() for a in arr))
    depth_hits=sum(int((a['treedepth__']>=10).sum()) for a in arr)
    bfmi=az.bfmi(energy)
    rhat=float(az.rhat(diag,method='rank'))
    bulk=float(az.ess(diag,method='bulk'))
    tail=float(az.ess(diag,method='tail',prob=(0.05,0.95)))
    mcse=float(az.mcse(diag,method='mean'))
    input_=pd.read_csv(folder/'entrada.csv',sep=';')
    y=float(input_.iloc[0]['y'])
    sd_input=float(input_.iloc[0]['s'])
    # replicate coverage and tail probability (weak check for single y)
    ppc_lo,ppc_hi=np.quantile(yrep,[.025,.975]);ppc_cdf=float((yrep<=y).mean())
    rows.append(dict(folder=folder.name,theta_mean=float(diag.mean()),theta_sd=float(diag.std(ddof=1)),
                     rhat_rank=rhat,ess_bulk=bulk,ess_tail=tail,mcse_mean=mcse,
                     divergences=diverg,treedepth_max=max_depth,treedepth_hits_10=depth_hits,
                     ebfmi_min=float(bfmi.min()),ebfmi_max=float(bfmi.max()),
                     y=y,s=sd_input,ppc_q025=float(ppc_lo),ppc_q975=float(ppc_hi),ppc_cdf_obs=ppc_cdf))
df=pd.DataFrame(rows)
print('\nSYSTEM VERSIONS')
print('arviz',az.__version__,'pandas',pd.__version__,'numpy',np.__version__)
print('\nDIAGNOSTICS')
print(df[['folder','rhat_rank','ess_bulk','ess_tail','mcse_mean','divergences','treedepth_max','treedepth_hits_10','ebfmi_min','ppc_cdf_obs']].to_string(index=False,float_format=lambda x:f'{x:.5f}'))
print('\nAGGREGATE',dict(count=len(df),divergences=int(df.divergences.sum()),hits=int(df.treedepth_hits_10.sum()),min_bfmi=float(df.ebfmi_min.min()),max_rhat=float(df.rhat_rank.max()),min_bulk=float(df.ess_bulk.min()),max_mcse_over_sd=float((df.mcse_mean/df.theta_sd).max())))
out=root/'estudo_I'/'e08';out.mkdir(parents=True,exist_ok=True)
df.to_csv(out/'diagnosticos_e081.csv',sep=';',index=False)
print('Saved',out/'diagnosticos_e081.csv')
