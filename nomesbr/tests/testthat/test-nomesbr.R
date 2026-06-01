
if (requireNamespace("mockery", quietly = TRUE)) {
  library(mockery)
} else {
  message("mockery not available, some tests might be skipped or not run as intended.")
}

nomesteste <- c("PEDRO SANT ANA MOURAO FALECIDO","JJOSE D ACQUA","NAO CONSTA NADA"," TESTE ",
                "CORONEL JACINTO CUNHA",
                "MARIA DO SOCORRO SILVA DE OLIVEIRA")
d <- data.table(nome = nomesteste)
dclean <- limpar_nomes(d,'nome')

#Teste para função de limpeza de nomes marcar_problemas_e_limpar_nomes
test_that("limpar_nomes funciona corretamente",{
  DT_input <- d
  DT_cleaned <- dclean
  
  expect_equal(DT_cleaned[1,nome_clean],'PEDRO SANTANA MOURAO') #FALECIDO removido, removido espaco avaliado como apostrofo 
  expect_equal(DT_cleaned[2,nome_clean],'JOSE DACQUA') #repetição de D removida, removido espaco avaliado como apostrofo
  expect_true(is.na(DT_cleaned[3,nome_clean])) #NAO CONSTA NADA deve virar NA
  # expect_true(is.na(DT_cleaned[4,nome_clean])) #TESTE e espaços extras devem ser removidos, resultando em NA se vazio - removido por evidências de possibilidade de nome válido
  expect_true(all(c('falecido','cartorio','nome_clean') %in% names(DT_cleaned)))
})




#Teste para função de simplificação de nomes remove_PARTICULAS_AGNOMES

test_that("remove_PARTICULAS_AGNOMES funciona", {
  expect_equal(remove_PARTICULAS_AGNOMES("JOAO DA SILVA FILHO"),"JOAO SILVA")
  expect_equal(remove_PARTICULAS_AGNOMES("MARIA DE SOUSA"),"MARIA SOUSA")
})


#Teste para função de download de dado nomes_proprios_data
test_that("obter_dic_nomes_proprios_compostos funciona", {
  skip_on_cran()
  caminho_np <- obter_dic_nomes_proprios_compostos()
  expect_type(caminho_np,'character')
  expect_true(length(caminho_np)==1)
  expect_true(file.exists(caminho_np),info = "Arquivo nomes_proprios_compostos.rds inexiste, houve problema no download ou cache")
  expect_equal(as.character(tools::md5sum(caminho_np)),"7a09ca2d0d259b29979966fdf923fa4d",
               info = "Checksum do arquivo distinto do esperado.")
}
)


#Teste para função simplifica_PARTICULAS_AGNOMES_PATENTES
test_that('simplifica_PARTICULAS_AGNOMES_PATENTES funciona corretamente',{
  dspap <- data.table(nome_clean = c('JOAO DA SILVA FILHO','SARGENTO JOSE','DRA GLAUCIA','JOSE DE LA RUA'))
  dspap <- simplifica_PARTICULAS_AGNOMES_PATENTES(dspap)[,.(nome_simp,agnomes_titulos)]
  expect_equal(dspap,
          data.table(nome_simp = c('JOAO SILVA','JOSE','GLAUCIA','JOSE RUA'),
                     agnomes_titulos = c('FILHO','SARGENTO','DRA',NA)))
  })




test_that('tabular_problemas_em_nomes funciona corretamente',{
  tbd <- tabular_problemas_em_nomes(dclean,'nome')
  
  cond <- \(x) {
    if (x == 'final_missing'){
      (dclean$nada_nao == 1 | dclean$consta == 1) & is.na(dclean$nome_clean)
    } else {
      dclean[[x]] == 1
    }
  }
  
  condicoes <- c(
    "falecido",
    "cartorio",
    "espaco_TilAcentoApostrofe",
    "nome_P_M_S_N",
    "nada_nao",
    "consta", "nada_nao_consta",
    "nada_nao_consta2",
    "final_missing",
    "Xartigo", "sr_sra",
    "ignorado", 
    "dede_dada",
    "letra_repetida")
  expected_answer <- data.table(
    condition = condicoes,
    N_detected = sapply(condicoes,\(x) sum(cond(x),na.rm=TRUE),USE.NAMES = FALSE),
    N_made_NA = sapply(condicoes,\(x) sum(cond(x) & is.na(dclean$nome_clean),na.rm=TRUE),USE.NAMES = FALSE),
    N_replaced = sapply(condicoes,\(x) sum(cond(x) & !is.na(dclean$nome_clean) & dclean$nome_clean!=d$nome,na.rm=T),USE.NAMES = FALSE)
  )
    expect_equal(tbd,expected_answer)
}
  )

test_that('segmentar_nomes funciona corretamente',{
  
  segnomes <- segmentar_nomes(dclean,'nome_clean')
  
  expect_equal(
    segnomes$nome_clean_w12p,
    c("PEDRO SANTANA","JOSE DACQUA",
      NA,"TESTE","CORONEL JACINTO",
      "MARIA DO SOCORRO"))
  })


test_that('identificar_adicionar_nome_proprio funciona corretamente',{
  skip_on_cran()
  npd <- identificar_adicionar_nome_proprio(dclean,'nome_clean')
  
  expect_equal(
    npd$nome_clean1_v2,
    c("PEDRO","JOSE",
      NA,"","CORONEL",
      "MARIA DO SOCORRO"))
})



test_that('identificar_adicionar_nome_proprio funciona corretamente',{
  amostra_np2_data <- testthat::test_path("testdata", "amostra_np2_data_para_testes.RDS")
  mockery::stub(identificar_adicionar_nome_proprio, 'get_np2_data', function() amostra_np2_data)
  npd <- identificar_adicionar_nome_proprio(dclean,'nome_clean')
  
  expect_equal(
    npd$nome_clean1_v2,
    c("PEDRO","JOSE",
      NA,"","CORONEL",
      "MARIA DO SOCORRO"))
})