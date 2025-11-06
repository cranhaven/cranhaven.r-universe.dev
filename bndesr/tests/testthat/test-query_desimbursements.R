
# run the full test only in my personal machine

if (Sys.getenv('USERNAME') == 'igorl') {

  df <- query_desimbursements(year = c(1995:2021))

  # Data below from file https://www.bndes.gov.br/wps/wcm/connect/site/c447152b-f540-4b54-a30e-e6d96d375637/DESEMBOLSOS+DO+SISTEMA+BNDES.xlsx?MOD=AJPERES&amp;CACHEID=ROOTWORKSPACE.Z18_7QGCHA41LORVA0AHO1SIO51085-c447152b-f540-4b54-a30e-e6d96d375637-ohz2OC6

  consolidated_values_1995_2021 <- rev(c(64301894837.3, 64921466643.8, 55313843672.9,
                                     69303220810.0, 70750785420.9, 88256500663.2, 135942045428.4,
                                     187836868950.3, 190419035117.3, 155992269821.0,
                                     138873436656.7, 168422747024.7, 136356355110.0, 90877907888.2,
                                     64891795274.5, 51318015291.3, 46980237258.8, 39833897351.2,
                                     33533588775.1, 37419271861.5, 25216524876.8, 23045830658.1,
                                     18051513819.3, 18990882466.4, 17894060667.0, 9672615267.3,
                                     7097870478.2))

  year <- df %>%
    dplyr::group_by(ano) %>%
    dplyr::summarise(valor = sum(desembolsos_reais))


  test_that("Check values from previous years", {
    expect_equal(nrow(df), 3331174)
    expect_equal(ncol(df), 16)
    expect_equal(sum(df$desembolsos_reais), 2011514482090)

    # check total with the data consolidated yearly from BNDES from 1995 to 2021
    # round to remove the cents (all needs that every element of the vector be TRUE)
    expect_equal(all(round(year$valor, 0) == round(consolidated_values_1995_2021, 0)), TRUE)

  })

}


