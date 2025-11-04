# context("checa-saida")  # Script "test-checa-saida.R"
library(testthat)        # load testthat package
library(sidra)       # load our package


# Test whether the output is a data frame
test_that("sidra(1705) returns a data frame", {
  output_table <- sidra(1705)
  teste_conexao <- call_ibge({httr::GET('https://servicodados.ibge.gov.br/api/v3/agregados',config = httr::timeout(4))})
  if(is.null(teste_conexao)) {
    expect_null(output_table,info = paste(teste_conexao,"OR","API was not reacheable,
                function should have returned NULL"))

  } else {
  expect_type(output_table, "list")
  succeed("API was reachable and a data.frame was returned.")
}})
