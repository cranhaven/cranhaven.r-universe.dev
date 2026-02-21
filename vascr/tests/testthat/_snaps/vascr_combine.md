# Can combine dataframes

    Code
      data2
    Output
      # A tibble: 92,430 x 10
          Time Unit  Value Well  Sample       Frequency Experiment Instrument Excluded
         <dbl> <chr> <dbl> <chr> <chr>            <dbl> <fct>      <chr>      <chr>   
       1     5 Alpha  2.69 A01   35,000_cell~         0 1 : 1 : E~ ECIS       no      
       2     5 Alpha  2.89 A02   35,000_cell~         0 1 : 1 : E~ ECIS       no      
       3     5 Alpha  2.71 A03   35,000_cell~         0 1 : 1 : E~ ECIS       no      
       4     5 Alpha  2.52 B01   30,000_cell~         0 1 : 1 : E~ ECIS       no      
       5     5 Alpha  2.69 B02   30,000_cell~         0 1 : 1 : E~ ECIS       no      
       6     5 Alpha  2.34 B03   30,000_cell~         0 1 : 1 : E~ ECIS       no      
       7     5 Alpha  2.15 C01   25,000_cell~         0 1 : 1 : E~ ECIS       no      
       8     5 Alpha  2.35 C02   25,000_cell~         0 1 : 1 : E~ ECIS       no      
       9     5 Alpha  2.4  C03   25,000_cell~         0 1 : 1 : E~ ECIS       no      
      10     5 Alpha  1.85 D01   20,000_cell~         0 1 : 1 : E~ ECIS       no      
      # i 92,420 more rows
      # i 1 more variable: SampleID <int>

---

    Code
      data3
    Output
      # A tibble: 139,230 x 10
          Time Unit  Value Well  Sample       Frequency Experiment Instrument Excluded
         <dbl> <chr> <dbl> <chr> <chr>            <dbl> <fct>      <chr>      <chr>   
       1     5 Alpha  2.69 A01   35,000_cell~         0 1 : 1 : E~ ECIS       no      
       2     5 Alpha  2.89 A02   35,000_cell~         0 1 : 1 : E~ ECIS       no      
       3     5 Alpha  2.71 A03   35,000_cell~         0 1 : 1 : E~ ECIS       no      
       4     5 Alpha  2.52 B01   30,000_cell~         0 1 : 1 : E~ ECIS       no      
       5     5 Alpha  2.69 B02   30,000_cell~         0 1 : 1 : E~ ECIS       no      
       6     5 Alpha  2.34 B03   30,000_cell~         0 1 : 1 : E~ ECIS       no      
       7     5 Alpha  2.15 C01   25,000_cell~         0 1 : 1 : E~ ECIS       no      
       8     5 Alpha  2.35 C02   25,000_cell~         0 1 : 1 : E~ ECIS       no      
       9     5 Alpha  2.4  C03   25,000_cell~         0 1 : 1 : E~ ECIS       no      
      10     5 Alpha  1.85 D01   20,000_cell~         0 1 : 1 : E~ ECIS       no      
      # i 139,220 more rows
      # i 1 more variable: SampleID <int>

---

    Code
      vascr_combine(experiment1r.df, experiment2r.df, experiment3.df)
    Message
      ! Datasets have different non-identical timebases. Please resample one or more of these datasets before running this function again or graphs may not be properly generated.
      ! Datasets have different non-identical timebases. Please resample one or more of these datasets before running this function again or graphs may not be properly generated.
    Output
      # A tibble: 64,575 x 10
          Time Unit  Value Well  Sample       Frequency Experiment Instrument Excluded
         <dbl> <chr> <dbl> <chr> <chr>            <dbl> <fct>      <chr>      <chr>   
       1   5   Alpha  2.69 A01   35,000_cell~         0 1 : 1 : E~ ECIS       no      
       2  26.1 Alpha  5.31 A01   35,000_cell~         0 1 : 1 : E~ ECIS       no      
       3  47.2 Alpha  4.8  A01   35,000_cell~         0 1 : 1 : E~ ECIS       no      
       4  68.3 Alpha  3.94 A01   35,000_cell~         0 1 : 1 : E~ ECIS       no      
       5  89.4 Alpha  3.71 A01   35,000_cell~         0 1 : 1 : E~ ECIS       no      
       6 111.  Alpha  3.49 A01   35,000_cell~         0 1 : 1 : E~ ECIS       no      
       7 132.  Alpha  3.29 A01   35,000_cell~         0 1 : 1 : E~ ECIS       no      
       8 153.  Alpha  3.13 A01   35,000_cell~         0 1 : 1 : E~ ECIS       no      
       9 174.  Alpha  2.93 A01   35,000_cell~         0 1 : 1 : E~ ECIS       no      
      10 195   Alpha  2.71 A01   35,000_cell~         0 1 : 1 : E~ ECIS       no      
      # i 64,565 more rows
      # i 1 more variable: SampleID <int>

---

    Code
      vascr_combine(experiment1r.df, experiment2r.df, experiment3.df) %>%
        vascr_check_resampled()
    Message
      ! Datasets have different non-identical timebases. Please resample one or more of these datasets before running this function again or graphs may not be properly generated.
      ! Datasets have different non-identical timebases. Please resample one or more of these datasets before running this function again or graphs may not be properly generated.
    Output
      [1] FALSE

---

    Code
      vascr_combine(experiment1r.df, experiment2r.df, experiment3.df, resample = TRUE)
    Output
      # A tibble: 17,850 x 10
         Unit  Well  Sample    Frequency Experiment Instrument Excluded SampleID Value
         <chr> <chr> <chr>         <dbl> <fct>      <chr>      <chr>       <int> <dbl>
       1 Alpha A01   35,000_c~         0 1 : 1 : E~ ECIS       no              1  2.69
       2 Alpha A01   35,000_c~         0 1 : 1 : E~ ECIS       no              1  4.58
       3 Alpha A01   35,000_c~         0 1 : 1 : E~ ECIS       no              1  3.60
       4 Alpha A01   35,000_c~         0 1 : 1 : E~ ECIS       no              1  3.17
       5 Alpha A01   35,000_c~         0 1 : 1 : E~ ECIS       no              1  2.71
       6 Alpha A02   35,000_c~         0 1 : 1 : E~ ECIS       no              1  2.89
       7 Alpha A02   35,000_c~         0 1 : 1 : E~ ECIS       no              1  4.40
       8 Alpha A02   35,000_c~         0 1 : 1 : E~ ECIS       no              1  3.64
       9 Alpha A02   35,000_c~         0 1 : 1 : E~ ECIS       no              1  3.18
      10 Alpha A02   35,000_c~         0 1 : 1 : E~ ECIS       no              1  2.9 
      # i 17,840 more rows
      # i 1 more variable: Time <dbl>

