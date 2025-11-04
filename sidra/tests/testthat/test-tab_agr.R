
test_that(
  "function tab_agr returns what is expected",{
    teste_conexao <- call_ibge({httr::GET('https://servicodados.ibge.gov.br/api/v3/agregados',config = httr::timeout(4))})
    output_table <- sidra(1705)
    if(is.null(teste_conexao)) {
      expect_null(output_table,info = "API was not reacheable,
                function should have returned NULL")

    } else {
      expect_type(output_table, "list")
      succeed("API was reachable and a data.frame was returned.")
    }
  }
)
