# get_idb_data() 1year

    Code
      head(RcensusPkg::get_idb_data(dataset = "1year", years = c(2023, 2024),
      countries = c("BW", "NO")))
    Output
              GEO_ID     NAME   GENC    POP     YR    AGE    SEX
              <char>   <char> <char> <char> <char> <char> <char>
      1: W140000WOBW Botswana     BW  47164   2023      1      0
      2: W140000WOBW Botswana     BW  23872   2023      1      1
      3: W140000WOBW Botswana     BW  23292   2023      1      2
      4: W140000WOBW Botswana     BW  47133   2023      2      0
      5: W140000WOBW Botswana     BW  23852   2023      2      1
      6: W140000WOBW Botswana     BW  23281   2023      2      2

# get_idb_data() 5year

    Code
      head(RcensusPkg::get_idb_data(dataset = "5year", years = 2023, group = TRUE,
        countries = "US", wide_to_long = TRUE))
    Output
              GEO_ID          NAME  variable     value
              <char>        <char>    <fctr>    <char>
      1: W140000WOUS United States      GENC        US
      2: W140000WOUS United States       POP 334914895
      3: W140000WOUS United States  AREA_KM2   9151200
      4: W140000WOUS United States ASFR15_19      <NA>
      5: W140000WOUS United States ASFR20_24      <NA>
      6: W140000WOUS United States ASFR25_29      <NA>

