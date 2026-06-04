# get_variable_names()

    Code
      variables_dt <- RcensusPkg::get_variable_names(dataset = "acs/acs1/profile",
        vintage = 2019, filter_label_str = "educational attainment")
      variables_dt[23:33, .(name, label, dataset)]
    Output
                 name
               <char>
       1:  DP02_0060E
       2: DP02_0060PE
       3:  DP02_0061E
       4: DP02_0061PE
       5:  DP02_0062E
       6: DP02_0062PE
       7:  DP02_0063E
       8: DP02_0063PE
       9:  DP02_0064E
      10: DP02_0064PE
      11:  DP02_0065E
                                                                                                                label
                                                                                                               <char>
       1:                         Estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Less than 9th grade
       2:                          Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Less than 9th grade
       3:               Estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!9th to 12th grade, no diploma
       4:                Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!9th to 12th grade, no diploma
       5: Estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!High school graduate (includes equivalency)
       6:  Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!High school graduate (includes equivalency)
       7:                     Estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Some college, no degree
       8:                      Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Some college, no degree
       9:                          Estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Associate's degree
      10:                           Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Associate's degree
      11:                           Estimate!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree
                   dataset
                    <char>
       1: acs/acs1/profile
       2: acs/acs1/profile
       3: acs/acs1/profile
       4: acs/acs1/profile
       5: acs/acs1/profile
       6: acs/acs1/profile
       7: acs/acs1/profile
       8: acs/acs1/profile
       9: acs/acs1/profile
      10: acs/acs1/profile
      11: acs/acs1/profile

# get_variable_names() category

    Code
      variables_dt <- RcensusPkg::get_variable_names(category = "acs1", vintage = 2023,
        filter_label_str = "computers")
      variables_dt[44:49, .(name, label, dataset)]
    Output
                name
              <char>
      1:  DP02_0152E
      2: DP02_0152PE
      3:  DP02_0153E
      4: DP02_0153PE
      5:  DP02_0154E
      6: DP02_0154PE
                                                                                                  label
                                                                                                 <char>
      1:                                         Estimate!!COMPUTERS AND INTERNET USE!!Total households
      2:                                          Percent!!COMPUTERS AND INTERNET USE!!Total households
      3:                        Estimate!!COMPUTERS AND INTERNET USE!!Total households!!With a computer
      4:                         Percent!!COMPUTERS AND INTERNET USE!!Total households!!With a computer
      5: Estimate!!COMPUTERS AND INTERNET USE!!Total households!!With a broadband Internet subscription
      6:  Percent!!COMPUTERS AND INTERNET USE!!Total households!!With a broadband Internet subscription
                  dataset
                   <char>
      1: acs/acs1/profile
      2: acs/acs1/profile
      3: acs/acs1/profile
      4: acs/acs1/profile
      5: acs/acs1/profile
      6: acs/acs1/profile

