#!/usr/bin/env python3
"""E04: série analítica nacional da PNADC a partir da coleta SIDRA já auditada.

Executar: python estudo_I/e04/construir_base_e04.py
Dependências: Python 3, pandas.
Nenhuma fonte externa é consultada; brutos não são modificados.
"""
from __future__ import annotations

import hashlib
import json
import platform
from pathlib import Path

import pandas as pd

RAIZ = Path(__file__).resolve().parents[2]
ENTRADA = RAIZ / 'dados/04_series_csv/pnadc_todas_variaveis_brasil_ate_202609.csv'
META = RAIZ / 'dados/02_metadados'
SAIDA = Path(__file__).resolve().parent / 'saida'
SAIDA.mkdir(parents=True, exist_ok=True)

# id | eixo | papel | nome | tabela | variavel | categoria | CV | numerador | denominador | alerta
# A população 14+ é suporte/denominador e NÃO um 14º eixo substantivo.
ESPECIFICACOES = [
 ('desocupacao_pct', 'A', 'principal', 'Taxa de desocupação', '6381', '4099', '', '4103', 'Pessoas desocupadas', 'Força de trabalho', 'Atenção à documentação de ajuste conceitual em 2016; efeito não estimado'),
 ('desocupados_mil', 'A', 'principal', 'Pessoas desocupadas', '6318', '1641', '32446', '4087', 'Pessoas desocupadas', 'Não se aplica: total expandido', 'Milhares de pessoas, não entrevistas'),
 ('nivel_ocupacao_pct', 'B', 'principal', 'Nível de ocupação', '6379', '4097', '', '4101', 'Pessoas ocupadas', 'População de 14 anos ou mais', 'Não confundir com taxa de participação'),
 ('participacao_pct', 'B', 'principal', 'Taxa de participação', '5944', '4096', '', '4100', 'Força de trabalho', 'População de 14 anos ou mais', 'Não confundir com taxa de desocupação'),
 ('ocupados_mil', 'B', 'principal', 'Pessoas ocupadas', '6318', '1641', '32387', '4087', 'Pessoas ocupadas', 'Não se aplica: total expandido', 'Total populacional expandido'),
 ('forca_trabalho_mil', 'B', 'principal', 'Força de trabalho', '6318', '1641', '32386', '4087', 'Ocupados + desocupados', 'Não se aplica: total expandido', 'Total populacional expandido'),
 ('populacao_14mais_mil', 'B', 'apoio', 'População de 14 anos ou mais', '6318', '1641', '32385', '4087', 'Pessoas de 14 anos ou mais', 'Não se aplica: total expandido', 'Denominador de nível de ocupação e participação'),
 ('subutilizacao_pct', 'C', 'principal', 'Taxa composta de subutilização', '6441', '4118', '', '4119', 'Desocupados + subocupados por horas + força potencial', 'Força de trabalho ampliada', 'Ruptura conceitual de horas no final de 2015; 201510/201511 não publicados'),
 ('subocupacao_horas_pct', 'C', 'principal', 'Taxa de subocupação por insuficiência de horas', '6785', '9819', '', '9820', 'Ocupados subocupados por insuficiência de horas', 'Pessoas ocupadas', 'Ruptura conceitual de horas no final de 2015; 201510/201511 não publicados'),
 ('desalentados_mil', 'C', 'principal', 'Pessoas desalentadas', '6438', '1641', '46254', '4087', 'Pessoas desalentadas', 'Não se aplica: total expandido', 'Subgrupo da força de trabalho potencial'),
 ('desalento_pct', 'C', 'principal', 'Percentual de desalento', '6807', '9869', '', '9870', 'Pessoas desalentadas', 'Força de trabalho + desalentados', 'Denominador diferente da população 14+ e da força de trabalho isolada'),
 ('informalidade_pct', 'D', 'principal', 'Taxa oficial de informalidade', '8513', '12466', '', '12467', 'Pessoas ocupadas em condições de informalidade', 'Pessoas ocupadas', 'Publicação mensal móvel somente desde 201512; não retroprojetar'),
 ('contrib_previdencia_pct', 'D', 'principal', 'Contribuição à previdência entre ocupados', '3919', '8463', '', '8464', 'Pessoas ocupadas contribuintes em qualquer trabalho', 'Pessoas ocupadas', 'Não equivale ao complemento da taxa oficial de informalidade'),
 ('rendimento_real_habitual_rs', 'E', 'principal', 'Rendimento médio mensal real habitual', '6390', '5933', '', '5941', 'Soma de rendimentos habituais de trabalhadores com rendimento, na definição da pesquisa', 'Pessoas ocupadas com rendimento de trabalho', 'IPCA e referência monetária da mesma extração/vintage; não redeflacionar'),
]


def sha256(caminho: Path) -> str:
    h = hashlib.sha256()
    with caminho.open('rb') as f:
        for bloco in iter(lambda: f.read(1024 * 1024), b''):
            h.update(bloco)
    return h.hexdigest()


def exigir(condicao: bool, mensagem: str) -> None:
    if not condicao:
        raise AssertionError(mensagem)


def numero_publicado(serie: pd.Series) -> pd.Series:
    # Apenas números decimais SIDRA; não interpretar A/Z/... como valores do fenômeno.
    ok = serie.str.fullmatch(r'[+-]?(?:\d+(?:\.\d*)?|\.\d+)')
    exigir(bool((ok | serie.eq('...')).all()),
           f'Encontrado código inesperado: {serie.loc[~(ok | serie.eq("..."))].unique().tolist()}')
    return pd.to_numeric(serie.where(ok), errors='coerce')


def executar() -> None:
    h_antes = sha256(ENTRADA)
    origem = pd.read_csv(ENTRADA, sep=';', encoding='utf-8-sig', dtype=str,
                         keep_default_na=False)
    campos = ['Tabela', 'Periodo', 'PeriodoDescricao', 'Localidade', 'VariavelId',
              'Variavel', 'CategoriasIds', 'Categorias', 'Unidade', 'ValorOriginal']
    exigir(origem.columns.tolist() == campos, 'Esquema de origem inesperado')
    exigir(len(origem) == 22130, 'Quantidade de linhas de origem mudou')
    chave = ['Tabela', 'Periodo', 'VariavelId', 'CategoriasIds']
    exigir(not origem.duplicated(chave).any(), 'Chave original duplicada')
    exigir(set(origem.Localidade) == {'Brasil'}, 'Localidade não esperada')
    periodos = sorted(origem.Periodo.unique().tolist())
    exigir(len(periodos) == 175 and periodos[0] == '201203' and periodos[-1] == '202609',
           'Cobertura da origem mudou')
    esperado = pd.period_range('2012-03', '2026-09', freq='M').strftime('%Y%m').tolist()
    exigir(periodos == esperado, 'Sequência temporal não contínua na origem')

    partes = []
    for (iid, eixo, papel, descricao, tab, var, cat, cv_var, numerador, denominador, nota) in ESPECIFICACOES:
        caminho_meta = META / f'tabela_{tab}_metadados.json'
        m = json.loads(caminho_meta.read_text(encoding='utf-8-sig'))
        exigir(int(m['id']) == int(tab) and m['periodicidade']['frequencia'] == 'trimestral móvel',
               f'Tabela ou periodicidade divergente: {tab}')
        ids_meta = {str(v['id']): v for v in m['variaveis']}
        exigir(var in ids_meta and cv_var in ids_meta, f'Variável sem metadados: {tab}/{var}')
        if cat:
            cats_meta = {str(cc['id']) for cl in m['classificacoes'] for cc in cl['categorias']}
            exigir(cat in cats_meta, f'Categoria ausente dos metadados: {tab}/{cat}')

        filtro = lambda vid: (origem.Tabela.eq(tab) & origem.VariavelId.eq(vid)
                             & origem.CategoriasIds.eq(cat))
        valores = origem.loc[filtro(var)].copy()
        cvs = origem.loc[filtro(cv_var), ['Periodo', 'Localidade', 'ValorOriginal']].copy()
        exigir(len(valores) == len(cvs), f'Valores/CVs desiguais: {iid}')
        unidade_meta = ids_meta[var]['unidade']
        exigir(valores.loc[valores.ValorOriginal.ne('...'), 'Unidade'].eq(unidade_meta).all()
               and valores.loc[valores.ValorOriginal.eq('...'), 'Unidade'].isin(['', unidade_meta]).all(),
               f'Unidade não corresponde aos metadados: {iid}')
        # SIDRA omite Unidade nos períodos '...'; preservar a original e acrescentar a unidade documental.
        valores['UnidadeAnalitica'] = unidade_meta
        exigir(valores.Periodo.is_unique and cvs.Periodo.is_unique, f'Período duplicado: {iid}')
        valores = valores.merge(cvs.rename(columns={'ValorOriginal':'CVOriginal'}),
                                on=['Periodo', 'Localidade'], how='left', validate='one_to_one',
                                indicator=True)
        exigir(valores._merge.eq('both').all(), f'CV não pareado: {iid}')
        valores = valores.drop(columns='_merge')
        valores['IndicadorId'] = iid
        valores['Eixo'] = eixo
        valores['Papel'] = papel
        valores['Indicador'] = descricao
        valores['CVVariavelId'] = cv_var
        valores['CVCategoriasIds'] = cat
        valores['CVUnidade'] = '%'
        valores['Valor'] = numero_publicado(valores['ValorOriginal'])
        valores['CVPercentual'] = numero_publicado(valores['CVOriginal'])
        exigir((valores.Valor.isna() == valores.CVPercentual.isna()).all(),
               f'Valores e CVs com lacunas incompatíveis: {iid}')
        valores['EstadoValor'] = valores['Valor'].notna().map({True: 'PUBLICADO_NUMERICO',
                                                              False:'NAO_DIVULGADO_OFICIAL'})
        valores['EstadoCV'] = valores['CVPercentual'].notna().map({True:'PUBLICADO_NUMERICO',
                                                                 False:'NAO_DIVULGADO_OFICIAL'})
        valores['Numerador'] = numerador
        valores['Denominador'] = denominador
        valores['NotaComparabilidade'] = nota
        valores['RegimeConceitual2015'] = 'NAO_APLICAVEL'
        if iid in {'subutilizacao_pct', 'subocupacao_horas_pct'}:
            valores['RegimeConceitual2015'] = 'APOS_RUPTURA_2015'
            valores.loc[valores.Periodo.le('201509'), 'RegimeConceitual2015'] = 'ANTES_RUPTURA_2015'
            valores.loc[valores.Periodo.isin(['201510', '201511']), 'RegimeConceitual2015'] = 'TRANSICAO_NAO_DIVULGADA'
        fim = pd.PeriodIndex(valores.Periodo.str[:4] + '-' + valores.Periodo.str[4:], freq='M')
        inicio = fim - 2
        valores['InicioJanela'] = inicio.strftime('%Y-%m')
        valores['FimJanela'] = fim.strftime('%Y-%m')
        valores['Periodicidade'] = 'trimestre móvel (três meses)'
        valores['Fonte'] = f'IBGE/SIDRA, tabela {tab}, variável {var}, categoria {cat or "sem categoria"}'
        partes.append(valores)

    dados = pd.concat(partes, ignore_index=True)
    ordem = ['Eixo','IndicadorId','Indicador','Papel','Periodo','PeriodoDescricao',
             'InicioJanela','FimJanela','Periodicidade','Localidade','Tabela',
             'VariavelId','CategoriasIds','Categorias','Variavel','Unidade','UnidadeAnalitica',
             'ValorOriginal','Valor','EstadoValor','CVVariavelId','CVCategoriasIds',
             'CVUnidade','CVOriginal','CVPercentual','EstadoCV','Numerador','Denominador',
             'RegimeConceitual2015','NotaComparabilidade','Fonte']
    dados = dados[ordem].sort_values(['Eixo','IndicadorId','Periodo']).reset_index(drop=True)
    exigir(not dados.duplicated(['IndicadorId','Periodo']).any(), 'Duplicidade na base analítica')
    exigir(dados.IndicadorId.nunique() == 14, 'Especificação de indicadores alterada')
    exigir(dados.EstadoValor.eq('NAO_DIVULGADO_OFICIAL').sum() == 4, 'Lacunas oficiais não preservadas')
    exigir(dados.loc[dados.EstadoValor.eq('NAO_DIVULGADO_OFICIAL'), 'Periodo'].value_counts().to_dict() ==
           {'201510':2, '201511':2}, 'Lacunas inesperadas')
    exigir(dados.loc[dados.IndicadorId.eq('informalidade_pct'),'Periodo'].min() == '201512',
           'Início da série de informalidade incorreto')
    exigir(dados.loc[dados.IndicadorId.eq('informalidade_pct')].shape[0] == 130,
           'Cobertura da informalidade incorreta')

    largo = dados.pivot(index='Periodo', columns='IndicadorId', values='Valor')
    cv_largo = dados.pivot(index='Periodo', columns='IndicadorId', values='CVPercentual')
    largo = largo.reindex(periodos)
    cv_largo = cv_largo.reindex(periodos)
    exigir(len(largo) == 175, 'Pivot descarta períodos')
    def s(nome: str) -> pd.Series:
        return largo[nome]

    contabil = s('forca_trabalho_mil') - s('ocupados_mil') - s('desocupados_mil')
    taxas = {
        'desocupacao_pct': 100*s('desocupados_mil')/s('forca_trabalho_mil'),
        'nivel_ocupacao_pct': 100*s('ocupados_mil')/s('populacao_14mais_mil'),
        'participacao_pct': 100*s('forca_trabalho_mil')/s('populacao_14mais_mil'),
        'desalento_pct': 100*s('desalentados_mil')/(s('forca_trabalho_mil')+s('desalentados_mil')),
    }
    diferencas = {k:(s(k)-v) for k,v in taxas.items()}
    exigir(contabil.abs().max() <= 1.0, 'Identidade força = ocupados + desocupados falhou')
    exigir(all(d.abs().max() <= 0.051 for d in diferencas.values()),
           'Identidade de taxas excedeu tolerância de arredondamento 0,051 p.p.')
    exige_202609 = {'desocupacao_pct':5.2, 'desocupados_mil':5628,
        'ocupados_mil':103638, 'forca_trabalho_mil':109266,
        'populacao_14mais_mil':175729, 'nivel_ocupacao_pct':59.0,
        'participacao_pct':62.2, 'subutilizacao_pct':13.0,
        'subocupacao_horas_pct':4.1, 'informalidade_pct':37.5,
        'rendimento_real_habitual_rs':3790}
    for k,v in exige_202609.items():
        exigir(float(largo.loc['202609', k]) == v, f'Referência 202609 divergente: {k}')

    resumo = dados.groupby(['Eixo','IndicadorId','Indicador','Papel','UnidadeAnalitica'],as_index=False).agg(
        Linhas=('Periodo','size'), Primeira=('Periodo','min'), Ultima=('Periodo','max'),
        Numericos=('Valor','count'), Ausencias=('Valor',lambda x:int(x.isna().sum())),
        CVsNumericos=('CVPercentual','count'))
    # O suporte não deve ser confundido com outro desfecho central.
    exigir(int(resumo.loc[resumo.Papel.eq('principal'),'Linhas'].sum()) == 2230,
           'Contagem de observações centrais incorreta')

    colunas_wide = sorted(largo.columns)
    larga_out = pd.DataFrame({'Periodo':periodos})
    larga_out['InicioJanela'] = pd.PeriodIndex([p[:4]+'-'+p[4:] for p in periodos],freq='M').strftime('%Y-%m')
    larga_out['InicioJanela'] = (pd.PeriodIndex(larga_out['InicioJanela'],freq='M') -2).strftime('%Y-%m')
    larga_out['FimJanela'] = pd.PeriodIndex([p[:4]+'-'+p[4:] for p in periodos],freq='M').strftime('%Y-%m')
    for col in colunas_wide:
        larga_out[col] = largo[col].to_numpy()
        larga_out['cv_'+col] = cv_largo[col].to_numpy()

    dados.to_csv(SAIDA/'base_analitica_e04_longa.csv',sep=';',index=False,encoding='utf-8-sig',na_rep='')
    larga_out.to_csv(SAIDA/'base_analitica_e04_larga.csv',sep=';',index=False,encoding='utf-8-sig',na_rep='')
    resumo.to_csv(SAIDA/'resumo_cobertura_e04.csv',sep=';',index=False,encoding='utf-8-sig')
    # Verificação de round-trip contra campos originais sem renomeações semânticas.
    reread = pd.read_csv(SAIDA/'base_analitica_e04_longa.csv',sep=';',dtype=str,encoding='utf-8-sig',keep_default_na=False)
    exigir(reread.ValorOriginal.tolist() == dados.ValorOriginal.tolist(), 'ValorOriginal alterado na escrita')
    exigir(reread.Unidade.tolist() == dados.Unidade.tolist(), 'Unidade alterada na escrita')
    h_depois = sha256(ENTRADA)
    exigir(h_antes == h_depois, 'Entrada modificada durante E04')

    audit = {
        'etapa':'E04', 'natureza':'EXE: execução Python local do conjunto transportado',
        'entrada':str(ENTRADA.relative_to(RAIZ)), 'sha256_entrada_antes_depois':h_antes,
        'ambiente_python': platform.python_version(), 'pandas': pd.__version__,
        'comando': 'python estudo_I/e04/construir_base_e04.py',
        'linhas_origem':len(origem), 'chaves_origem_unicas':int(origem[chave].drop_duplicates().shape[0]),
        'periodos_origem':len(periodos), 'primeiro_periodo':'201203', 'ultimo_periodo':'202609',
        'indicadores_principais':13, 'indicadores_apoio':1, 'linhas_base_longa':len(dados),
        'linhas_principais':int(dados.Papel.eq('principal').sum()),
        'linhas_apoio':int(dados.Papel.eq('apoio').sum()),
        'num_valores_numericos':int(dados.Valor.notna().sum()),
        'num_ausencias_oficiais':int(dados.Valor.isna().sum()),
        'periodos_ausencia':['201510','201511'],
        'informalidade_inicio':'201512', 'informalidade_linhas':130,
        'cv_percentual_pareado':'sim, um CV de origem por valor; sem covariâncias inferidas',
        'max_diferenca_contabil_mil_pessoas':float(contabil.abs().max()),
        'max_diferenca_taxas_em_pontos_percentuais': {k:float(d.abs().max()) for k,d in diferencas.items()},
        'tolerancias':{'contagem_mil_pessoas':1.0,'taxas_pontos_percentuais':0.051},
        'valores_202609': {k:float(largo.loc['202609',k]) for k in colunas_wide},
        'informalidade_201203_201511':'SEM LINHAS NA FONTE, não zero',
        'unidade_em_lacunas':'4 células com Unidade original vazia; UnidadeAnalitica obtida do metadado da variável',
        'limites':['Agregados trimestrais móveis sobrepostos e painel rotativo',
                   'CVs individuais marginais sem covariância conjunta',
                   'Quebra de conceito de horas em 2015 nas duas taxas sinalizadas',
                   'Rendimento em reais da vintage coletada, sem deflação adicional',
                   'Sem validação independente da API nesta etapa; conferência dos dados transportados'],
        'saidas':['estudo_I/e04/saida/base_analitica_e04_longa.csv',
                  'estudo_I/e04/saida/base_analitica_e04_larga.csv',
                  'estudo_I/e04/saida/resumo_cobertura_e04.csv',
                  'estudo_I/e04/saida/auditoria_e04.json'],
    }
    (SAIDA/'auditoria_e04.json').write_text(json.dumps(audit,indent=2,ensure_ascii=False)+'\n',encoding='utf-8')
    print('E04 EXECUTADA/VERIFICADA no ambiente Python da conversa')
    print('Origem:',len(origem),'linhas; SHA256:',h_antes)
    print('Base:',len(dados),'linhas;',len(colunas_wide),'indicadores (13 centrais + 1 apoio),',
          int(dados.Valor.isna().sum()),'lacunas de valor')
    print(resumo[['Eixo','IndicadorId','Linhas','Numericos','Ausencias','Primeira','Ultima']].to_string(index=False))
    print('Diferença máxima da identidade contábil:',contabil.abs().max(),'mil pessoas')
    print('Diferenças máximas das taxas (p.p.):',
          {k:round(float(v.abs().max()),6) for k,v in diferencas.items()})
    print('Saídas:',SAIDA)


if __name__ == '__main__':
    executar()
