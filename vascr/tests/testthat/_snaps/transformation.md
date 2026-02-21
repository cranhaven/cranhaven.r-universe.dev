# Can summarise

    Code
      vascr_summarise(rbgrowth.df, level = "summary")
    Output
      # A tibble: 312 x 14
          Time Unit  Frequency Sample      Instrument      sd totaln     n   min   max
         <dbl> <fct>     <dbl> <chr>       <chr>        <dbl>  <int> <int> <dbl> <dbl>
       1     5 Rb            0 0_cells + ~ ECIS       NA           3     1     0  0   
       2     5 Rb            0 10,000_cel~ ECIS        0           9     3     0  0   
       3     5 Rb            0 15,000_cel~ ECIS        0           9     3     0  0   
       4     5 Rb            0 20,000_cel~ ECIS        0           9     3     0  0   
       5     5 Rb            0 25,000_cel~ ECIS        0           9     3     0  0   
       6     5 Rb            0 30,000_cel~ ECIS        0           9     3     0  0   
       7     5 Rb            0 35,000_cel~ ECIS        0           9     3     0  0   
       8     5 Rb            0 5,000_cell~ ECIS        0.0337      9     3     0  0.06
       9    10 Rb            0 0_cells + ~ ECIS       NA           3     1     0  0   
      10    10 Rb            0 10,000_cel~ ECIS        0           9     3     0  0   
      # i 302 more rows
      # i 4 more variables: Well <chr>, Value <dbl>, Experiment <chr>, sem <dbl>

---

    Code
      vascr_summarise(rbgrowth.df, level = "experiments")
    Output
      # A tibble: 858 x 15
          Time Unit  Frequency Sample    Experiment Instrument SampleID Excluded    sd
         <dbl> <fct>     <dbl> <chr>     <fct>      <chr>         <int> <chr>    <dbl>
       1     5 Rb            0 0_cells ~ 3 : Exper~ ECIS              8 no           0
       2     5 Rb            0 10,000_c~ 1 : Exper~ ECIS              6 no           0
       3     5 Rb            0 10,000_c~ 2 : Exper~ ECIS              6 no           0
       4     5 Rb            0 10,000_c~ 3 : Exper~ ECIS              6 no           0
       5     5 Rb            0 15,000_c~ 1 : Exper~ ECIS              5 no           0
       6     5 Rb            0 15,000_c~ 2 : Exper~ ECIS              5 no           0
       7     5 Rb            0 15,000_c~ 3 : Exper~ ECIS              5 no           0
       8     5 Rb            0 20,000_c~ 1 : Exper~ ECIS              4 no           0
       9     5 Rb            0 20,000_c~ 2 : Exper~ ECIS              4 no           0
      10     5 Rb            0 20,000_c~ 3 : Exper~ ECIS              4 no           0
      # i 848 more rows
      # i 6 more variables: n <int>, min <dbl>, max <dbl>, Well <chr>, Value <dbl>,
      #   sem <dbl>

---

    Code
      vascr_summarise(rbgrowth.df, level = "experiments") %>% vascr_summarise(level = "summary")
    Output
      # A tibble: 312 x 14
          Time Unit  Frequency Sample      Instrument      sd totaln     n   min   max
         <dbl> <fct>     <dbl> <chr>       <chr>        <dbl>  <int> <int> <dbl> <dbl>
       1     5 Rb            0 0_cells + ~ ECIS       NA           3     1     0  0   
       2     5 Rb            0 10,000_cel~ ECIS        0           9     3     0  0   
       3     5 Rb            0 15,000_cel~ ECIS        0           9     3     0  0   
       4     5 Rb            0 20,000_cel~ ECIS        0           9     3     0  0   
       5     5 Rb            0 25,000_cel~ ECIS        0           9     3     0  0   
       6     5 Rb            0 30,000_cel~ ECIS        0           9     3     0  0   
       7     5 Rb            0 35,000_cel~ ECIS        0           9     3     0  0   
       8     5 Rb            0 5,000_cell~ ECIS        0.0337      9     3     0  0.06
       9    10 Rb            0 0_cells + ~ ECIS       NA           3     1     0  0   
      10    10 Rb            0 10,000_cel~ ECIS        0           9     3     0  0   
      # i 302 more rows
      # i 4 more variables: Well <chr>, Value <dbl>, Experiment <chr>, sem <dbl>

---

    Code
      vascr_summarise(rbgrowth.df, level = "wells")
    Output
      # A tibble: 2,574 x 12
          Time Unit  Well  Value Sample    Frequency Experiment cells line  Instrument
         <dbl> <fct> <chr> <dbl> <chr>         <dbl> <fct>      <chr> <chr> <chr>     
       1     5 Rb    A01       0 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       2     5 Rb    A02       0 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       3     5 Rb    A03       0 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       4     5 Rb    B01       0 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       5     5 Rb    B02       0 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       6     5 Rb    B03       0 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       7     5 Rb    C01       0 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
       8     5 Rb    C02       0 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
       9     5 Rb    C03       0 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
      10     5 Rb    D01       0 20,000_c~         0 1 : Exper~ 20000 HCME~ ECIS      
      # i 2,564 more rows
      # i 2 more variables: SampleID <int>, Excluded <chr>

---

    Code
      vascr_summarise(rbgrowth.df, level = "median_deviation")
    Output
      # A tibble: 2,574 x 15
      # Groups:   Time, Experiment, SampleID, Excluded [858]
          Time Unit  Well  Value Sample    Frequency Experiment cells line  Instrument
         <dbl> <fct> <chr> <dbl> <chr>         <dbl> <fct>      <chr> <chr> <chr>     
       1     5 Rb    A01       0 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       2     5 Rb    A02       0 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       3     5 Rb    A03       0 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       4     5 Rb    B01       0 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       5     5 Rb    B02       0 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       6     5 Rb    B03       0 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       7     5 Rb    C01       0 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
       8     5 Rb    C02       0 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
       9     5 Rb    C03       0 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
      10     5 Rb    D01       0 20,000_c~         0 1 : Exper~ 20000 HCME~ ECIS      
      # i 2,564 more rows
      # i 5 more variables: SampleID <int>, Excluded <chr>, Median_Deviation <dbl>,
      #   Median_Value <dbl>, MAD <dbl>

---

    Requested data is less summarised than the data input, try again

---

    Code
      vascr_summarise_summary(vascr_summarise(rbgrowth.df, "summary"))
    Output
      # A tibble: 312 x 14
          Time Unit  Frequency Sample      Instrument      sd totaln     n   min   max
         <dbl> <fct>     <dbl> <chr>       <chr>        <dbl>  <int> <int> <dbl> <dbl>
       1     5 Rb            0 0_cells + ~ ECIS       NA           3     1     0  0   
       2     5 Rb            0 10,000_cel~ ECIS        0           9     3     0  0   
       3     5 Rb            0 15,000_cel~ ECIS        0           9     3     0  0   
       4     5 Rb            0 20,000_cel~ ECIS        0           9     3     0  0   
       5     5 Rb            0 25,000_cel~ ECIS        0           9     3     0  0   
       6     5 Rb            0 30,000_cel~ ECIS        0           9     3     0  0   
       7     5 Rb            0 35,000_cel~ ECIS        0           9     3     0  0   
       8     5 Rb            0 5,000_cell~ ECIS        0.0337      9     3     0  0.06
       9    10 Rb            0 0_cells + ~ ECIS       NA           3     1     0  0   
      10    10 Rb            0 10,000_cel~ ECIS        0           9     3     0  0   
      # i 302 more rows
      # i 4 more variables: Well <chr>, Value <dbl>, Experiment <chr>, sem <dbl>

---

    Code
      w16 = system.file("extdata/instruments/ecis_16_testplate.abp", package = "vascr")
      d16 = vascr_import("ECIS", raw = w16, experiment = "W16")
    Condition
      Warning:
      Expected 2 pieces. Additional pieces discarded in 2 rows [1, 2].

---

    Code
      vascr_check_resampled(d16)
    Output
      [1] FALSE

---

    Code
      vascr_summarise(d16 %>% vascr_subset(unit = "R", frequency = 4000), "summary")
    Output
      # A tibble: 2 x 14
           Time Unit  Frequency Sample Instrument    sd totaln     n   min   max Well 
          <dbl> <fct>     <dbl> <chr>  <chr>      <dbl>  <int> <int> <dbl> <dbl> <chr>
      1 6.88e-4 R          4000 NA     ECIS          NA      8     1 4504. 4504. A01,~
      2 6.73e-2 R          4000 NA     ECIS          NA      8     1 4503. 4503. A01,~
      # i 3 more variables: Value <dbl>, Experiment <chr>, sem <dbl>

# Can summarise deviation

    Code
      vascr_summarise(growth.df %>% vascr_subset(unit = "R", frequency = "4000"),
      level = "median_deviation")
    Output
      # A tibble: 2,808 x 15
      # Groups:   Time, Experiment, SampleID, Excluded [936]
          Time Unit  Well  Value Sample    Frequency Experiment cells line  Instrument
         <dbl> <fct> <chr> <dbl> <chr>         <dbl> <fct>      <chr> <chr> <chr>     
       1     5 R     A01    302. 35,000_c~      4000 1 : Exper~ 35000 HCME~ ECIS      
       2    10 R     A01    342. 35,000_c~      4000 1 : Exper~ 35000 HCME~ ECIS      
       3    15 R     A01    405. 35,000_c~      4000 1 : Exper~ 35000 HCME~ ECIS      
       4    20 R     A01    469. 35,000_c~      4000 1 : Exper~ 35000 HCME~ ECIS      
       5    25 R     A01    514. 35,000_c~      4000 1 : Exper~ 35000 HCME~ ECIS      
       6    30 R     A01    535. 35,000_c~      4000 1 : Exper~ 35000 HCME~ ECIS      
       7    35 R     A01    572. 35,000_c~      4000 1 : Exper~ 35000 HCME~ ECIS      
       8    40 R     A01    620. 35,000_c~      4000 1 : Exper~ 35000 HCME~ ECIS      
       9    45 R     A01    664. 35,000_c~      4000 1 : Exper~ 35000 HCME~ ECIS      
      10    50 R     A01    690. 35,000_c~      4000 1 : Exper~ 35000 HCME~ ECIS      
      # i 2,798 more rows
      # i 5 more variables: SampleID <int>, Excluded <chr>, Median_Deviation <dbl>,
      #   Median_Value <dbl>, MAD <dbl>

# Can normalise

    Code
      vascr_normalise(data.df = rgrowth.df, 100)
    Output
      # A tibble: 1,440 x 10
          Time Unit   Value Well  Sample      Frequency Experiment Instrument SampleID
         <dbl> <fct>  <dbl> <chr> <chr>           <dbl> <fct>      <chr>         <int>
       1     5 R     -298.  A01   35,000_cel~      4000 1 : Exper~ ECIS              1
       2    10 R     -257.  A01   35,000_cel~      4000 1 : Exper~ ECIS              1
       3    15 R     -194.  A01   35,000_cel~      4000 1 : Exper~ ECIS              1
       4    20 R     -130.  A01   35,000_cel~      4000 1 : Exper~ ECIS              1
       5    25 R      -84.8 A01   35,000_cel~      4000 1 : Exper~ ECIS              1
       6    30 R      -63.9 A01   35,000_cel~      4000 1 : Exper~ ECIS              1
       7    35 R      -27.0 A01   35,000_cel~      4000 1 : Exper~ ECIS              1
       8    40 R       21.3 A01   35,000_cel~      4000 1 : Exper~ ECIS              1
       9    45 R       65.3 A01   35,000_cel~      4000 1 : Exper~ ECIS              1
      10    50 R       90.6 A01   35,000_cel~      4000 1 : Exper~ ECIS              1
      # i 1,430 more rows
      # i 1 more variable: Excluded <chr>

---

    Code
      vascr_normalise(rgrowth.df, 100, divide = TRUE)
    Output
      # A tibble: 1,440 x 10
          Time Unit  Value Well  Sample       Frequency Experiment Instrument SampleID
         <dbl> <fct> <dbl> <chr> <chr>            <dbl> <fct>      <chr>         <int>
       1     5 R     0.503 A01   35,000_cell~      4000 1 : Exper~ ECIS              1
       2    10 R     0.570 A01   35,000_cell~      4000 1 : Exper~ ECIS              1
       3    15 R     0.677 A01   35,000_cell~      4000 1 : Exper~ ECIS              1
       4    20 R     0.783 A01   35,000_cell~      4000 1 : Exper~ ECIS              1
       5    25 R     0.858 A01   35,000_cell~      4000 1 : Exper~ ECIS              1
       6    30 R     0.893 A01   35,000_cell~      4000 1 : Exper~ ECIS              1
       7    35 R     0.955 A01   35,000_cell~      4000 1 : Exper~ ECIS              1
       8    40 R     1.04  A01   35,000_cell~      4000 1 : Exper~ ECIS              1
       9    45 R     1.11  A01   35,000_cell~      4000 1 : Exper~ ECIS              1
      10    50 R     1.15  A01   35,000_cell~      4000 1 : Exper~ ECIS              1
      # i 1,430 more rows
      # i 1 more variable: Excluded <chr>

---

    Code
      vascr_normalise(rgrowth.df, 100)
    Output
      # A tibble: 2,808 x 10
          Time Unit   Value Well  Sample      Frequency Experiment Instrument SampleID
         <dbl> <fct>  <dbl> <chr> <chr>           <dbl> <fct>      <chr>         <int>
       1     5 R     -298.  A01   35,000_cel~      4000 1 : Exper~ ECIS              1
       2    10 R     -257.  A01   35,000_cel~      4000 1 : Exper~ ECIS              1
       3    15 R     -194.  A01   35,000_cel~      4000 1 : Exper~ ECIS              1
       4    20 R     -130.  A01   35,000_cel~      4000 1 : Exper~ ECIS              1
       5    25 R      -84.8 A01   35,000_cel~      4000 1 : Exper~ ECIS              1
       6    30 R      -63.9 A01   35,000_cel~      4000 1 : Exper~ ECIS              1
       7    35 R      -27.0 A01   35,000_cel~      4000 1 : Exper~ ECIS              1
       8    40 R       21.3 A01   35,000_cel~      4000 1 : Exper~ ECIS              1
       9    45 R       65.3 A01   35,000_cel~      4000 1 : Exper~ ECIS              1
      10    50 R       90.6 A01   35,000_cel~      4000 1 : Exper~ ECIS              1
      # i 2,798 more rows
      # i 1 more variable: Excluded <chr>

---

    Code
      vascr_normalise(growth.df, NULL)
    Output
      # A tibble: 146,370 x 12
          Time Unit  Well  Value Sample    Frequency Experiment cells line  Instrument
         <dbl> <chr> <chr> <dbl> <chr>         <dbl> <fct>      <chr> <chr> <chr>     
       1     0 Alpha A01      NA 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       2     0 Alpha A02      NA 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       3     0 Alpha A03      NA 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       4     0 Alpha B01      NA 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       5     0 Alpha B02      NA 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       6     0 Alpha B03      NA 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       7     0 Alpha C01      NA 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
       8     0 Alpha C02      NA 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
       9     0 Alpha C03      NA 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
      10     0 Alpha D01      NA 20,000_c~         0 1 : Exper~ 20000 HCME~ ECIS      
      # i 146,360 more rows
      # i 2 more variables: SampleID <int>, Excluded <chr>

# Can subsample

    Code
      vascr_subsample(growth.df, 10)
    Output
      # A tibble: 17,850 x 12
          Time Unit  Well  Value Sample    Frequency Experiment cells line  Instrument
         <dbl> <chr> <chr> <dbl> <chr>         <dbl> <fct>      <chr> <chr> <chr>     
       1     0 Alpha A01      NA 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       2     0 Alpha A02      NA 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       3     0 Alpha A03      NA 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       4     0 Alpha B01      NA 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       5     0 Alpha B02      NA 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       6     0 Alpha B03      NA 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       7     0 Alpha C01      NA 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
       8     0 Alpha C02      NA 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
       9     0 Alpha C03      NA 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
      10     0 Alpha D01      NA 20,000_c~         0 1 : Exper~ 20000 HCME~ ECIS      
      # i 17,840 more rows
      # i 2 more variables: SampleID <int>, Excluded <chr>

---

    Code
      vascr_subsample(growth.df, Inf)
    Output
      # A tibble: 146,370 x 12
          Time Unit  Well  Value Sample    Frequency Experiment cells line  Instrument
         <dbl> <chr> <chr> <dbl> <chr>         <dbl> <fct>      <chr> <chr> <chr>     
       1     0 Alpha A01      NA 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       2     0 Alpha A02      NA 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       3     0 Alpha A03      NA 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       4     0 Alpha B01      NA 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       5     0 Alpha B02      NA 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       6     0 Alpha B03      NA 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       7     0 Alpha C01      NA 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
       8     0 Alpha C02      NA 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
       9     0 Alpha C03      NA 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
      10     0 Alpha D01      NA 20,000_c~         0 1 : Exper~ 20000 HCME~ ECIS      
      # i 146,360 more rows
      # i 2 more variables: SampleID <int>, Excluded <chr>

---

    Code
      vascr_subsample(growth.df %>% vascr_subset(time = 10), 10)
    Output
      # A tibble: 3,570 x 12
          Time Unit  Well  Value Sample    Frequency Experiment cells line  Instrument
         <dbl> <chr> <chr> <dbl> <chr>         <dbl> <fct>      <chr> <chr> <chr>     
       1    10 Alpha A01    3.34 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       2    10 Alpha A02    3.77 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       3    10 Alpha A03    3.25 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       4    10 Alpha B01    3.15 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       5    10 Alpha B02    3.4  30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       6    10 Alpha B03    2.89 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       7    10 Alpha C01    2.71 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
       8    10 Alpha C02    2.88 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
       9    10 Alpha C03    2.83 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
      10    10 Alpha D01    2.27 20,000_c~         0 1 : Exper~ 20000 HCME~ ECIS      
      # i 3,560 more rows
      # i 2 more variables: SampleID <int>, Excluded <chr>

---

    Code
      vascr_resample_time(growth.df, t_start = 5, t_end = 20, rate = 5)
    Output
      # A tibble: 14,280 x 12
         Unit  Well  Sample       Frequency Experiment cells line  Instrument SampleID
         <chr> <chr> <chr>            <dbl> <fct>      <chr> <chr> <chr>         <int>
       1 Alpha A01   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
       2 Alpha A01   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
       3 Alpha A01   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
       4 Alpha A01   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
       5 Alpha A02   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
       6 Alpha A02   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
       7 Alpha A02   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
       8 Alpha A02   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
       9 Alpha A03   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
      10 Alpha A03   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
      # i 14,270 more rows
      # i 3 more variables: Excluded <chr>, Value <dbl>, Time <dbl>

# Can interpolate time

    Code
      vascr_interpolate_time(growth.df %>% vascr_subset(unit = "Rb"))
    Output
      # A tibble: 2,574 x 12
         Unit  Well  Sample       Frequency Experiment cells line  Instrument SampleID
         <fct> <chr> <chr>            <dbl> <fct>      <chr> <chr> <chr>         <int>
       1 Rb    A01   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
       2 Rb    A01   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
       3 Rb    A01   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
       4 Rb    A01   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
       5 Rb    A01   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
       6 Rb    A01   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
       7 Rb    A01   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
       8 Rb    A01   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
       9 Rb    A01   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
      10 Rb    A01   35,000_cell~         0 1 : Exper~ 35000 HCME~ ECIS              1
      # i 2,564 more rows
      # i 3 more variables: Excluded <chr>, Value <dbl>, Time <dbl>

---

    vascr_interpolate_time only supports one unit and frequency at a time

# Can interpolate time unresampled dataset

    Code
      vascr_resample_time(growth_unresampled.df %>% vascr_subset(unit = "R",
        frequency = 4000))
    Output
      # A tibble: 8,100 x 10
         Unit  Well  Frequency Instrument Experiment Sample SampleID Excluded Value
         <fct> <chr>     <dbl> <chr>      <fct>      <chr>     <dbl> <lgl>    <dbl>
       1 R     A01        4000 ECIS       "1 : "     A01           1 FALSE     276.
       2 R     A01        4000 ECIS       "1 : "     A01           1 FALSE     276.
       3 R     A01        4000 ECIS       "1 : "     A01           1 FALSE     277.
       4 R     A01        4000 ECIS       "1 : "     A01           1 FALSE     278.
       5 R     A01        4000 ECIS       "1 : "     A01           1 FALSE     279.
       6 R     A01        4000 ECIS       "1 : "     A01           1 FALSE     280.
       7 R     A01        4000 ECIS       "1 : "     A01           1 FALSE     281.
       8 R     A01        4000 ECIS       "1 : "     A01           1 FALSE     281.
       9 R     A01        4000 ECIS       "1 : "     A01           1 FALSE     282.
      10 R     A01        4000 ECIS       "1 : "     A01           1 FALSE     283.
      # i 8,090 more rows
      # i 1 more variable: Time <dbl>

# vascr_force_resampled

    Code
      vascr_force_resampled(growth.df)
    Output
      # A tibble: 146,370 x 12
          Time Unit  Well  Value Sample    Frequency Experiment cells line  Instrument
         <dbl> <chr> <chr> <dbl> <chr>         <dbl> <fct>      <chr> <chr> <chr>     
       1     0 Alpha A01      NA 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       2     0 Alpha A02      NA 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       3     0 Alpha A03      NA 35,000_c~         0 1 : Exper~ 35000 HCME~ ECIS      
       4     0 Alpha B01      NA 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       5     0 Alpha B02      NA 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       6     0 Alpha B03      NA 30,000_c~         0 1 : Exper~ 30000 HCME~ ECIS      
       7     0 Alpha C01      NA 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
       8     0 Alpha C02      NA 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
       9     0 Alpha C03      NA 25,000_c~         0 1 : Exper~ 25000 HCME~ ECIS      
      10     0 Alpha D01      NA 20,000_c~         0 1 : Exper~ 20000 HCME~ ECIS      
      # i 146,360 more rows
      # i 2 more variables: SampleID <int>, Excluded <chr>

---

    Code
      vascr_force_resampled(growth_unresampled.df)
    Message
      ! Data is not resampled, resampling to allow further analytics
    Output
      # A tibble: 48,600 x 10
         Unit  Well  Frequency Instrument Experiment Sample SampleID Excluded Value
         <fct> <chr>     <dbl> <chr>      <fct>      <chr>     <dbl> <chr>    <dbl>
       1 R     A01        1000 ECIS       "1 : "     A01           1 no        383.
       2 R     A01        1000 ECIS       "1 : "     A01           1 no        383.
       3 R     A01        1000 ECIS       "1 : "     A01           1 no        385.
       4 R     A01        1000 ECIS       "1 : "     A01           1 no        386.
       5 R     A01        1000 ECIS       "1 : "     A01           1 no        387.
       6 R     A01        1000 ECIS       "1 : "     A01           1 no        388.
       7 R     A01        1000 ECIS       "1 : "     A01           1 no        389.
       8 R     A01        1000 ECIS       "1 : "     A01           1 no        389.
       9 R     A01        1000 ECIS       "1 : "     A01           1 no        390.
      10 R     A01        1000 ECIS       "1 : "     A01           1 no        391.
      # i 48,590 more rows
      # i 1 more variable: Time <dbl>

# vascr time samples counts correctly

    Code
      vascr_find_count_timepoints(growth.df)
    Output
      [1] 41

# vascr AUC works

    Code
      vascr_auc(growth.df %>% vascr_subset(experiment = 1, well = "A01", unit = "R",
        frequency = 4000))
    Output
      [1] 101819.7

# remove metadata

    Code
      growth.df %>% vascr_remove_metadata()
    Output
      # A tibble: 146,370 x 10
          Time Unit  Value Well  Sample       Frequency Experiment Instrument SampleID
         <dbl> <chr> <dbl> <chr> <chr>            <dbl> <fct>      <chr>         <int>
       1     0 Alpha    NA A01   35,000_cell~         0 1 : Exper~ ECIS              1
       2     0 Alpha    NA A02   35,000_cell~         0 1 : Exper~ ECIS              1
       3     0 Alpha    NA A03   35,000_cell~         0 1 : Exper~ ECIS              1
       4     0 Alpha    NA B01   30,000_cell~         0 1 : Exper~ ECIS              2
       5     0 Alpha    NA B02   30,000_cell~         0 1 : Exper~ ECIS              2
       6     0 Alpha    NA B03   30,000_cell~         0 1 : Exper~ ECIS              2
       7     0 Alpha    NA C01   25,000_cell~         0 1 : Exper~ ECIS              3
       8     0 Alpha    NA C02   25,000_cell~         0 1 : Exper~ ECIS              3
       9     0 Alpha    NA C03   25,000_cell~         0 1 : Exper~ ECIS              3
      10     0 Alpha    NA D01   20,000_cell~         0 1 : Exper~ ECIS              4
      # i 146,360 more rows
      # i 1 more variable: Excluded <chr>

---

    Code
      vascr_summarise(growth.df %>% vascr_subset(unit = "R", frequency = 4000),
      "experiments") %>% vascr_remove_metadata()
    Message
      ! You are removing some summary statistics. These are not re-generatable using vascr_explode alone, and must be regenerated with vascr_summarise.
    Output
      # A tibble: 936 x 10
          Time Unit  Value Well        Sample Frequency Experiment Instrument SampleID
         <dbl> <fct> <dbl> <chr>       <chr>      <dbl> <fct>      <chr>         <int>
       1     5 R      241. H01,H02,H03 0_cel~      4000 1 : Exper~ ECIS              8
       2     5 R      247. H04,H05,H06 0_cel~      4000 2 : Exper~ ECIS              8
       3     5 R      247. H07,H08,H09 0_cel~      4000 3 : Exper~ ECIS              8
       4     5 R      256. F01,F02,F03 10,00~      4000 1 : Exper~ ECIS              6
       5     5 R      267. F04,F05,F06 10,00~      4000 2 : Exper~ ECIS              6
       6     5 R      265. F07,F08,F09 10,00~      4000 3 : Exper~ ECIS              6
       7     5 R      263. E01,E02,E03 15,00~      4000 1 : Exper~ ECIS              5
       8     5 R      280. E04,E05,E06 15,00~      4000 2 : Exper~ ECIS              5
       9     5 R      273. E07,E08,E09 15,00~      4000 3 : Exper~ ECIS              5
      10     5 R      272. D01,D02,D03 20,00~      4000 1 : Exper~ ECIS              4
      # i 926 more rows
      # i 1 more variable: Excluded <chr>

