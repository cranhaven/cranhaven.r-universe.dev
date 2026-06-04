# wide_to_long()

    Code
      B19001_1yr_wide_dt <- RcensusPkg::get_vintage_data(dataset = "acs/acs1",
        vintage = 2016, group = "B19001", region = "state")
      head(RcensusPkg::wide_to_long(dt = B19001_1yr_wide_dt, id_v = c("NAME", "GEOID")))
    Output
                  NAME  GEOID    variable estimate
                <char> <char>      <fctr>   <char>
      1:   Mississippi     28 B19001_001E  1091245
      2:      Missouri     29 B19001_001E  2372190
      3:       Montana     30 B19001_001E   416125
      4:      Nebraska     31 B19001_001E   747562
      5:        Nevada     32 B19001_001E  1055158
      6: New Hampshire     33 B19001_001E   520643

