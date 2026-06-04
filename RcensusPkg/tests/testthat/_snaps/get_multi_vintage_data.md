# get_multi_vintage_data()

    Code
      deschutes_fips <- usmap::fips("OR", "Deschutes")
      state <- substr(deschutes_fips, 1, 2)
      county <- substr(deschutes_fips, 3, 5)
      head(RcensusPkg::get_multi_vintage_data(dataset = "acs/acs1", vintage_v = 2005:
        2019, vars = c("B25077_001E", "B25077_001M"), region = paste0("county:",
        county), regionin = paste0("state:", state)))
    Output
         B25077_001E B25077_001M  state county vintage
              <char>      <char> <char> <char>   <int>
      1:      236100       13444     41    017    2005
      2:      336600       11101     41    017    2006
      3:      356700       16765     41    017    2007
      4:      331600       17104     41    017    2008
      5:      284300       12652     41    017    2009
      6:      260700       18197     41    017    2010

