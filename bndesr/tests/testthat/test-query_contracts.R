# run the full test only in my personal machine

if (Sys.getenv('USERNAME') == 'igorl') {

  df <- query_contracts(year = c(2002:2021))


  test_that("Check values from previous years", {
    expect_equal(nrow(df), 2124076)
    expect_equal(ncol(df), 35)
    expect_equal(sum(df$valor_contratacao_reais), 1533886450893)
    expect_equal(sum(df$valor_desembolso_reais), 1404777248159)
  })

}

