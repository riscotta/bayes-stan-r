# Testes sobre agregados públicos de SP, sem reajustar modelos.
testthat::test_that("Indicadores e identidades da E09 são consistentes", {
 d<-file.path("data","raw","mortalidade_sp_0_74","resultados_auditados")
 annual<-read.csv(file.path(d,"E09_taxas_anuais_e_sensibilidade.csv"))
 comp<-read.csv(file.path(d,"E09_decomposicao_descritiva_simetrica.csv"))
 nb<-read.csv(file.path(d,"E09_validacao_NB2_e_cobertura.csv"))
 weights<-read.csv(file.path(d,"E09_pesos_2019_sexo_idade.csv"))
 age<-read.csv(file.path(d,"E09_contribuicoes_2019_2021_faixa_etaria.csv"))
 testthat::expect_identical(as.integer(annual$ano),2006:2023)
 testthat::expect_equal(sum(annual$obitos),778039)
 testthat::expect_equal(sum(annual$populacao),196070964)
 testthat::expect_equal(annual$taxa_bruta_100mil,
      1e5*annual$obitos/annual$populacao,tolerance=1e-7)
 testthat::expect_equal(sum(weights$peso_padrao_2019),1,tolerance=1e-8)
 testthat::expect_equal(sum(age$contribuicao_delta_padronizada_100mil),
      annual$taxa_padronizada_2019_100mil[annual$ano==2021]-
      annual$taxa_padronizada_2019_100mil[annual$ano==2019],tolerance=1e-7)
 testthat::expect_equal(comp$parcela_mudanca_taxas_100mil+
      comp$parcela_composicao_100mil,comp$diferenca_bruta_100mil,tolerance=1e-7)
 testthat::expect_equal(nb$obitos_R[nb$ano==2021],61166)
 testthat::expect_equal(nb$previstos[nb$ano==2021],42279.9283527,tolerance=1e-5)
 testthat::expect_equal(nb$cobertura95[nb$ano==2021],0.732986111111,tolerance=1e-8)
})
