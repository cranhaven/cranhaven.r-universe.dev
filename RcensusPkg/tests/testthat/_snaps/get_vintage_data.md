# get_vintage_data() vars

    Code
      head(RcensusPkg::get_vintage_data(dataset = "acs/acs5", vars = c("B01001A_017E",
        "B01001A_001E"), vintage = 2021, region = "state:*"))
    Output
               NAME B01001A_017E B01001A_001E  state  GEOID
             <char>       <char>       <char> <char> <char>
      1:    Alabama      1694858      3338590     01     01
      2:     Alaska       214313       458520     02     02
      3:    Arizona      2501115      4985729     04     04
      4:   Arkansas      1125451      2227020     05     05
      5: California     10235004     20553732     06     06
      6:   Colorado      2227858      4507448     08     08

# get_vintage_data() group

    Code
      holmes_ohio_fips <- usmap::fips(state = "Ohio", county = "Holmes")
      ohio_fips <- substr(holmes_ohio_fips, 1, 2)
      holmes_fips <- substr(holmes_ohio_fips, 3, 5)
      head(RcensusPkg::get_vintage_data(dataset = "dec/dhc", vintage = 2020, group = "H12I",
        wide_to_long = TRUE, region = paste0("county:", holmes_fips), regionin = paste0(
          "state:", ohio_fips), ))
    Output
                        NAME  GEOID   variable  value
                      <char> <char>     <fctr> <char>
      1: Holmes County, Ohio  39075  H12I_001N  13099
      2: Holmes County, Ohio  39075 H12I_001NA   <NA>
      3: Holmes County, Ohio  39075  H12I_002N   9881
      4: Holmes County, Ohio  39075 H12I_002NA   <NA>
      5: Holmes County, Ohio  39075  H12I_003N   1487
      6: Holmes County, Ohio  39075 H12I_003NA   <NA>

