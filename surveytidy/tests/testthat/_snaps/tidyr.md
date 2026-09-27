# drop_na() empty-domain warning snapshot

    Code
      drop_na(d, y1)
    Condition
      Warning:
      ! `drop_na()` resulted in an empty domain (0 in-domain rows).
      i All rows have `NA` in at least one of the selected columns.
      v Check the column selection or inspect the data for pervasive missingness.
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_taylor> (Taylor series linearization)
      Sample size: 100
      Domain: 0 of 100 rows
      
    Output
      # A tibble: 100 x 9
         psu   strata      fpc    wt    y1      y2    y3 group ..surveycore_domain..
         <chr> <chr>     <dbl> <dbl> <dbl>   <dbl> <int> <chr> <lgl>                
       1 psu_1 stratum_1   742  14.3    NA -1.05       0 C     FALSE                
       2 psu_1 stratum_1   742  21.8    NA -0.646      0 A     FALSE                
       3 psu_1 stratum_1   742  14.4    NA -0.185      1 C     FALSE                
       4 psu_1 stratum_1   742  18.9    NA -1.20       1 C     FALSE                
       5 psu_1 stratum_1   742  23.0    NA  2.04       0 A     FALSE                
       6 psu_1 stratum_1   742  11.0    NA  0.108      0 C     FALSE                
       7 psu_2 stratum_1   742  13.8    NA -0.0841     0 A     FALSE                
       8 psu_2 stratum_1   742  14.2    NA  0.496      0 C     FALSE                
       9 psu_2 stratum_1   742  16.5    NA  0.0374     1 B     FALSE                
      10 psu_2 stratum_1   742  13.7    NA -0.132      0 C     FALSE                
      # i 90 more rows

