# get_category_strings() category names

    Code
      RcensusPkg::get_category_strings(get_names = TRUE)
    Output
      [1] "sex"      "agegroup" "race"    

# get_category_strings() race

    Code
      RcensusPkg::get_category_strings(name = "race")
    Output
            val                                                         race_label
          <int>                                                             <char>
       1:     0                                                          All races
       2:     1                                                        White alone
       3:     2                                                        Black alone
       4:     3                            American Indian and Alaska Native alone
       5:     4                                                        Asian alone
       6:     5                   Native Hawaiian and Other Pacific Islander alone
       7:     6                                                  Two or more races
       8:     7                                      White alone or in combination
       9:     8                                      Black alone or in combination
      10:     9          American Indian and Alaska Native alone or in combination
      11:    10                                      Asian alone or in combination
      12:    11 Native Hawaiian and Other Pacific Islander alone or in combination

