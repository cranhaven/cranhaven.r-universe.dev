# print.survey_taylor default output matches snapshot

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_taylor> (Taylor series linearization)
      Sample size: 50
      
    Output
      # A tibble: 50 x 8
         psu   strata      fpc    wt    y1      y2    y3 group
         <chr> <chr>     <dbl> <dbl> <dbl>   <dbl> <int> <chr>
       1 psu_1 stratum_1   378  14.3  50.9 -0.122      1 A    
       2 psu_1 stratum_1   378  21.8  20.1  0.188      0 B    
       3 psu_1 stratum_1   378  14.4  52.8  0.119      1 B    
       4 psu_2 stratum_1   378  18.9  46.3 -0.0251     0 A    
       5 psu_2 stratum_1   378  23.0  51.9  0.108      0 B    
       6 psu_2 stratum_1   378  11.0  55.8 -0.485      0 C    
       7 psu_2 stratum_1   378  13.8  64.0 -0.504      0 A    
       8 psu_2 stratum_1   378  14.2  42.7 -1.66       0 B    
       9 psu_3 stratum_1   378  16.5  63.0 -0.382      1 C    
      10 psu_3 stratum_1   378  13.7  53.4 -0.513      1 A    
      # i 40 more rows

# print.survey_taylor full=TRUE output matches snapshot

    Code
      print(d, full = TRUE)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_taylor> (Taylor series linearization)
      Sample size: 50
      Weighted N: 625
      
      
      -- Design specification --
      
      * IDs: psu (10 PSUs)
      * Strata: strata (2 strata)
      * Weights: wt
      * Weights provided as: sampling weights
      * FPC: fpc
      * Nesting: TRUE
      
      Design variables: psu, wt, strata, and fpc
      
      
      -- Weight distribution --
      
      * Range: 6.04 – 22.97
      * Mean: 12.5
      * CV: 0.323
      
      
      -- Metadata --
      
      0 variable(s) labeled
      
    Output
      # A tibble: 50 x 8
         psu   strata      fpc    wt    y1      y2    y3 group
         <chr> <chr>     <dbl> <dbl> <dbl>   <dbl> <int> <chr>
       1 psu_1 stratum_1   378  14.3  50.9 -0.122      1 A    
       2 psu_1 stratum_1   378  21.8  20.1  0.188      0 B    
       3 psu_1 stratum_1   378  14.4  52.8  0.119      1 B    
       4 psu_2 stratum_1   378  18.9  46.3 -0.0251     0 A    
       5 psu_2 stratum_1   378  23.0  51.9  0.108      0 B    
       6 psu_2 stratum_1   378  11.0  55.8 -0.485      0 C    
       7 psu_2 stratum_1   378  13.8  64.0 -0.504      0 A    
       8 psu_2 stratum_1   378  14.2  42.7 -1.66       0 B    
       9 psu_3 stratum_1   378  16.5  63.0 -0.382      1 C    
      10 psu_3 stratum_1   378  13.7  53.4 -0.513      1 A    
      # i 40 more rows

# print.survey_replicate default output matches snapshot

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_replicate> (BRR, 5 replicates)
      Sample size: 50
      
    Output
      # A tibble: 50 x 13
         psu   strata      fpc    wt    y1      y2    y3 group repwt_1 repwt_2 repwt_3
         <chr> <chr>     <dbl> <dbl> <dbl>   <dbl> <int> <chr>   <dbl>   <dbl>   <dbl>
       1 psu_1 stratum_1   378  14.3  50.9 -0.122      1 A        14.4    13.2   13.3 
       2 psu_1 stratum_1   378  21.8  20.1  0.188      0 B        19.0    22.6   20.3 
       3 psu_1 stratum_1   378  14.4  52.8  0.119      1 B        11.6    12.6   15.2 
       4 psu_2 stratum_1   378  18.9  46.3 -0.0251     0 A        17.5    19.0   19.0 
       5 psu_2 stratum_1   378  23.0  51.9  0.108      0 B        22.9    21.2   24.3 
       6 psu_2 stratum_1   378  11.0  55.8 -0.485      0 C        12.7    13.1    9.77
       7 psu_2 stratum_1   378  13.8  64.0 -0.504      0 A        15.3    12.5   15.1 
       8 psu_2 stratum_1   378  14.2  42.7 -1.66       0 B        14.0    15.2   14.9 
       9 psu_3 stratum_1   378  16.5  63.0 -0.382      1 C        18.0    15.3   15.9 
      10 psu_3 stratum_1   378  13.7  53.4 -0.513      1 A        11.8    13.1   15.8 
      # i 40 more rows
      # i 2 more variables: repwt_4 <dbl>, repwt_5 <dbl>

# print.survey_replicate full=TRUE output matches snapshot

    Code
      print(d, full = TRUE)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_replicate> (BRR, 5 replicates)
      Sample size: 50
      Weighted N: 625
      
      
      -- Design specification --
      
      * Weights: wt
      * Replicates: 5 BRR replicate weight column(s)
      * Scale: 0.2
      * MSE: TRUE
      * FPC: not specified
      
      
      -- Weight distribution --
      
      * Range: 6.04 – 22.97
      * Mean: 12.5
      * CV: 0.323
      
      
      -- Metadata --
      
      0 variable(s) labeled
      
    Output
      # A tibble: 50 x 13
         psu   strata      fpc    wt    y1      y2    y3 group repwt_1 repwt_2 repwt_3
         <chr> <chr>     <dbl> <dbl> <dbl>   <dbl> <int> <chr>   <dbl>   <dbl>   <dbl>
       1 psu_1 stratum_1   378  14.3  50.9 -0.122      1 A        14.4    13.2   13.3 
       2 psu_1 stratum_1   378  21.8  20.1  0.188      0 B        19.0    22.6   20.3 
       3 psu_1 stratum_1   378  14.4  52.8  0.119      1 B        11.6    12.6   15.2 
       4 psu_2 stratum_1   378  18.9  46.3 -0.0251     0 A        17.5    19.0   19.0 
       5 psu_2 stratum_1   378  23.0  51.9  0.108      0 B        22.9    21.2   24.3 
       6 psu_2 stratum_1   378  11.0  55.8 -0.485      0 C        12.7    13.1    9.77
       7 psu_2 stratum_1   378  13.8  64.0 -0.504      0 A        15.3    12.5   15.1 
       8 psu_2 stratum_1   378  14.2  42.7 -1.66       0 B        14.0    15.2   14.9 
       9 psu_3 stratum_1   378  16.5  63.0 -0.382      1 C        18.0    15.3   15.9 
      10 psu_3 stratum_1   378  13.7  53.4 -0.513      1 A        11.8    13.1   15.8 
      # i 40 more rows
      # i 2 more variables: repwt_4 <dbl>, repwt_5 <dbl>

# print.survey_twophase default output matches snapshot

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_twophase> (method: full)
      Phase 1 sample size: 60
      Phase 2 sample size: 29
      
    Output
      # A tibble: 60 x 11
         psu   strata      fpc    wt    y1         y2    y3 group subset phase1_prob
         <chr> <chr>     <dbl> <dbl> <dbl>      <dbl> <int> <chr> <lgl>        <dbl>
       1 psu_1 stratum_1   451  14.3  60.4 -2.02          0 B     TRUE        0.0687
       2 psu_1 stratum_1   451  21.8  59.2 -1.22          0 B     FALSE       0.0687
       3 psu_1 stratum_1   451  14.4  57.2  0.180         1 B     FALSE       0.0687
       4 psu_1 stratum_1   451  18.9  39.6  0.568         1 C     TRUE        0.0687
       5 psu_2 stratum_1   451  23.0  49.1 -0.493         1 A     FALSE       0.0687
       6 psu_2 stratum_1   451  11.0  56.2  0.0000629     0 C     TRUE        0.0687
       7 psu_2 stratum_1   451  13.8  40.5  1.12          0 A     TRUE        0.0687
       8 psu_2 stratum_1   451  14.2  44.6  1.44          0 C     FALSE       0.0687
       9 psu_2 stratum_1   451  16.5  55.8 -1.10          0 A     TRUE        0.0687
      10 psu_2 stratum_1   451  13.7  57.7 -0.117         1 B     TRUE        0.0687
      # i 50 more rows
      # i 1 more variable: phase2_prob <dbl>

# print.survey_twophase full=TRUE output matches snapshot

    Code
      print(d, full = TRUE)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_twophase> (method: full)
      Phase 1 sample size: 60
      Phase 2 sample size: 29
      
      
      -- Phase 1 design --
      
      * IDs: psu (10 PSUs)
      * Strata: strata (2 strata)
      * Weights: wt
      
      
      -- Phase 2 design --
      
      * Subset: subset
      
      
      -- Metadata --
      
      0 variable(s) labeled
      
    Output
      # A tibble: 60 x 11
         psu   strata      fpc    wt    y1         y2    y3 group subset phase1_prob
         <chr> <chr>     <dbl> <dbl> <dbl>      <dbl> <int> <chr> <lgl>        <dbl>
       1 psu_1 stratum_1   451  14.3  60.4 -2.02          0 B     TRUE        0.0687
       2 psu_1 stratum_1   451  21.8  59.2 -1.22          0 B     FALSE       0.0687
       3 psu_1 stratum_1   451  14.4  57.2  0.180         1 B     FALSE       0.0687
       4 psu_1 stratum_1   451  18.9  39.6  0.568         1 C     TRUE        0.0687
       5 psu_2 stratum_1   451  23.0  49.1 -0.493         1 A     FALSE       0.0687
       6 psu_2 stratum_1   451  11.0  56.2  0.0000629     0 C     TRUE        0.0687
       7 psu_2 stratum_1   451  13.8  40.5  1.12          0 A     TRUE        0.0687
       8 psu_2 stratum_1   451  14.2  44.6  1.44          0 C     FALSE       0.0687
       9 psu_2 stratum_1   451  16.5  55.8 -1.10          0 A     TRUE        0.0687
      10 psu_2 stratum_1   451  13.7  57.7 -0.117         1 B     TRUE        0.0687
      # i 50 more rows
      # i 1 more variable: phase2_prob <dbl>

# summary.survey_taylor output matches snapshot

    Code
      summary(d)
    Message
      
      -- Survey Design Summary -------------------------------------------------------
      Type: Taylor series linearization
      Sample size: 50
      Weighted N: 625
      
      
      -- Design --
      
      IDs: psu (10 PSUs)
      Strata: strata (2 strata)
      Weights: wt
      * Range: 6.04 – 22.97
      * Mean: 12.5
      * CV: 0.323
      FPC: fpc
      
      Metadata: 0 of 8 variable(s) labeled

# summary.survey_replicate output matches snapshot

    Code
      summary(d)
    Message
      
      -- Survey Design Summary -------------------------------------------------------
      Type: replicate weights (BRR, 5 replicates)
      Sample size: 50
      Weighted N: 625
      
      
      -- Design --
      
      Weights: wt
      * Range: 6.04 – 22.97
      * Mean: 12.5
      * CV: 0.323
      Scale: 0.2
      MSE: TRUE
      
      Metadata: 0 of 13 variable(s) labeled

# summary.survey_twophase output matches snapshot

    Code
      summary(d)
    Message
      
      -- Survey Design Summary -------------------------------------------------------
      Type: two-phase sampling (method: full)
      Phase 1 sample size: 60
      Phase 2 sample size: 29
      
      
      -- Phase 1 design --
      
      IDs: psu (10 PSUs)
      Strata: strata (2 strata)
      Weights: wt
      Weighted N: 747
      
      
      -- Phase 2 design --
      
      Subset: subset
      
      Metadata: 0 of 11 variable(s) labeled

# print.survey_taylor() shows domain line when domain column is present

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_taylor> (Taylor series linearization)
      Sample size: 50
      Domain: 50 of 50 rows
      
    Output
      # A tibble: 50 x 9
         psu   strata      fpc    wt    y1      y2    y3 group ..surveycore_domain..
         <chr> <chr>     <dbl> <dbl> <dbl>   <dbl> <int> <chr> <lgl>                
       1 psu_1 stratum_1   378  14.3  50.9 -0.122      1 A     TRUE                 
       2 psu_1 stratum_1   378  21.8  20.1  0.188      0 B     TRUE                 
       3 psu_1 stratum_1   378  14.4  52.8  0.119      1 B     TRUE                 
       4 psu_2 stratum_1   378  18.9  46.3 -0.0251     0 A     TRUE                 
       5 psu_2 stratum_1   378  23.0  51.9  0.108      0 B     TRUE                 
       6 psu_2 stratum_1   378  11.0  55.8 -0.485      0 C     TRUE                 
       7 psu_2 stratum_1   378  13.8  64.0 -0.504      0 A     TRUE                 
       8 psu_2 stratum_1   378  14.2  42.7 -1.66       0 B     TRUE                 
       9 psu_3 stratum_1   378  16.5  63.0 -0.382      1 C     TRUE                 
      10 psu_3 stratum_1   378  13.7  53.4 -0.513      1 A     TRUE                 
      # i 40 more rows

# print.survey_taylor() domain count excludes NAs

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_taylor> (Taylor series linearization)
      Sample size: 50
      Domain: 17 of 50 rows
      
    Output
      # A tibble: 50 x 9
         psu   strata      fpc    wt    y1      y2    y3 group ..surveycore_domain..
         <chr> <chr>     <dbl> <dbl> <dbl>   <dbl> <int> <chr> <lgl>                
       1 psu_1 stratum_1   378  14.3  50.9 -0.122      1 A     TRUE                 
       2 psu_1 stratum_1   378  21.8  20.1  0.188      0 B     FALSE                
       3 psu_1 stratum_1   378  14.4  52.8  0.119      1 B     NA                   
       4 psu_2 stratum_1   378  18.9  46.3 -0.0251     0 A     TRUE                 
       5 psu_2 stratum_1   378  23.0  51.9  0.108      0 B     FALSE                
       6 psu_2 stratum_1   378  11.0  55.8 -0.485      0 C     NA                   
       7 psu_2 stratum_1   378  13.8  64.0 -0.504      0 A     TRUE                 
       8 psu_2 stratum_1   378  14.2  42.7 -1.66       0 B     FALSE                
       9 psu_3 stratum_1   378  16.5  63.0 -0.382      1 C     NA                   
      10 psu_3 stratum_1   378  13.7  53.4 -0.513      1 A     TRUE                 
      # i 40 more rows

# print.survey_taylor() domain line appears before groups line

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_taylor> (Taylor series linearization)
      Sample size: 50
      Domain: 50 of 50 rows
      Groups: strata
      
    Output
      # A tibble: 50 x 9
         psu   strata      fpc    wt    y1      y2    y3 group ..surveycore_domain..
         <chr> <chr>     <dbl> <dbl> <dbl>   <dbl> <int> <chr> <lgl>                
       1 psu_1 stratum_1   378  14.3  50.9 -0.122      1 A     TRUE                 
       2 psu_1 stratum_1   378  21.8  20.1  0.188      0 B     TRUE                 
       3 psu_1 stratum_1   378  14.4  52.8  0.119      1 B     TRUE                 
       4 psu_2 stratum_1   378  18.9  46.3 -0.0251     0 A     TRUE                 
       5 psu_2 stratum_1   378  23.0  51.9  0.108      0 B     TRUE                 
       6 psu_2 stratum_1   378  11.0  55.8 -0.485      0 C     TRUE                 
       7 psu_2 stratum_1   378  13.8  64.0 -0.504      0 A     TRUE                 
       8 psu_2 stratum_1   378  14.2  42.7 -1.66       0 B     TRUE                 
       9 psu_3 stratum_1   378  16.5  63.0 -0.382      1 C     TRUE                 
      10 psu_3 stratum_1   378  13.7  53.4 -0.513      1 A     TRUE                 
      # i 40 more rows

# print.survey_replicate() shows domain line when domain column is present

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_replicate> (BRR, 5 replicates)
      Sample size: 50
      Domain: 50 of 50 rows
      
    Output
      # A tibble: 50 x 14
         psu   strata      fpc    wt    y1      y2    y3 group repwt_1 repwt_2 repwt_3
         <chr> <chr>     <dbl> <dbl> <dbl>   <dbl> <int> <chr>   <dbl>   <dbl>   <dbl>
       1 psu_1 stratum_1   378  14.3  50.9 -0.122      1 A        14.4    13.2   13.3 
       2 psu_1 stratum_1   378  21.8  20.1  0.188      0 B        19.0    22.6   20.3 
       3 psu_1 stratum_1   378  14.4  52.8  0.119      1 B        11.6    12.6   15.2 
       4 psu_2 stratum_1   378  18.9  46.3 -0.0251     0 A        17.5    19.0   19.0 
       5 psu_2 stratum_1   378  23.0  51.9  0.108      0 B        22.9    21.2   24.3 
       6 psu_2 stratum_1   378  11.0  55.8 -0.485      0 C        12.7    13.1    9.77
       7 psu_2 stratum_1   378  13.8  64.0 -0.504      0 A        15.3    12.5   15.1 
       8 psu_2 stratum_1   378  14.2  42.7 -1.66       0 B        14.0    15.2   14.9 
       9 psu_3 stratum_1   378  16.5  63.0 -0.382      1 C        18.0    15.3   15.9 
      10 psu_3 stratum_1   378  13.7  53.4 -0.513      1 A        11.8    13.1   15.8 
      # i 40 more rows
      # i 3 more variables: repwt_4 <dbl>, repwt_5 <dbl>, ..surveycore_domain.. <lgl>

# print.survey_twophase() shows domain line when domain column is present

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_twophase> (method: full)
      Phase 1 sample size: 60
      Phase 2 sample size: 29
      Domain: 29 of 29 Phase 2 rows
      
    Output
      # A tibble: 60 x 12
         psu   strata      fpc    wt    y1         y2    y3 group subset phase1_prob
         <chr> <chr>     <dbl> <dbl> <dbl>      <dbl> <int> <chr> <lgl>        <dbl>
       1 psu_1 stratum_1   451  14.3  60.4 -2.02          0 B     TRUE        0.0687
       2 psu_1 stratum_1   451  21.8  59.2 -1.22          0 B     FALSE       0.0687
       3 psu_1 stratum_1   451  14.4  57.2  0.180         1 B     FALSE       0.0687
       4 psu_1 stratum_1   451  18.9  39.6  0.568         1 C     TRUE        0.0687
       5 psu_2 stratum_1   451  23.0  49.1 -0.493         1 A     FALSE       0.0687
       6 psu_2 stratum_1   451  11.0  56.2  0.0000629     0 C     TRUE        0.0687
       7 psu_2 stratum_1   451  13.8  40.5  1.12          0 A     TRUE        0.0687
       8 psu_2 stratum_1   451  14.2  44.6  1.44          0 C     FALSE       0.0687
       9 psu_2 stratum_1   451  16.5  55.8 -1.10          0 A     TRUE        0.0687
      10 psu_2 stratum_1   451  13.7  57.7 -0.117         1 B     TRUE        0.0687
      # i 50 more rows
      # i 2 more variables: phase2_prob <dbl>, ..surveycore_domain.. <lgl>

# print.survey_nonprob() default output

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_nonprob> (non-probability) [experimental]
      * Variance: SRS approximation (no bootstrap replicate weights)
      Sample size: 30
      
    Output
      # A tibble: 30 x 9
         psu   strata      fpc    wt    y1      y2    y3 group cal_wt
         <chr> <chr>     <dbl> <dbl> <dbl>   <dbl> <int> <chr>  <dbl>
       1 psu_1 stratum_1   145 14.3   55.0 -0.727      1 A       13.7
       2 psu_1 stratum_1   145 12.9   32.8  1.30       1 B       13.6
       3 psu_1 stratum_1   145 17.8   42.2  0.336      1 A       17.5
       4 psu_2 stratum_1   145 12.9   41.5  1.04       0 A       13.9
       5 psu_2 stratum_1   145 19.7   25.9  0.921      1 A       21.5
       6 psu_2 stratum_1   145 13.0   50.4  0.721      1 C       11.8
       7 psu_2 stratum_1   145 17.1   52.1 -1.04       1 B       17.2
       8 psu_2 stratum_1   145 20.8   46.4 -0.0902     1 B       22.5
       9 psu_3 stratum_1   145  9.98  57.6  0.624      0 B       10.1
      10 psu_3 stratum_1   145 12.5   42.7 -0.954      1 A       12.4
      # i 20 more rows

# print.survey_nonprob() shows domain line when domain column is present

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_nonprob> (non-probability) [experimental]
      * Variance: SRS approximation (no bootstrap replicate weights)
      Sample size: 30
      Domain: 30 of 30 rows
      
    Output
      # A tibble: 30 x 10
         psu   strata      fpc    wt    y1      y2    y3 group cal_wt
         <chr> <chr>     <dbl> <dbl> <dbl>   <dbl> <int> <chr>  <dbl>
       1 psu_1 stratum_1   145 14.3   55.0 -0.727      1 A       13.7
       2 psu_1 stratum_1   145 12.9   32.8  1.30       1 B       13.6
       3 psu_1 stratum_1   145 17.8   42.2  0.336      1 A       17.5
       4 psu_2 stratum_1   145 12.9   41.5  1.04       0 A       13.9
       5 psu_2 stratum_1   145 19.7   25.9  0.921      1 A       21.5
       6 psu_2 stratum_1   145 13.0   50.4  0.721      1 C       11.8
       7 psu_2 stratum_1   145 17.1   52.1 -1.04       1 B       17.2
       8 psu_2 stratum_1   145 20.8   46.4 -0.0902     1 B       22.5
       9 psu_3 stratum_1   145  9.98  57.6  0.624      0 B       10.1
      10 psu_3 stratum_1   145 12.5   42.7 -0.954      1 A       12.4
      # i 20 more rows
      # i 1 more variable: ..surveycore_domain.. <lgl>

# print.survey_taylor() shows domain line when zero rows are in domain

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_taylor> (Taylor series linearization)
      Sample size: 50
      Domain: 0 of 50 rows
      
    Output
      # A tibble: 50 x 9
         psu   strata      fpc    wt    y1      y2    y3 group ..surveycore_domain..
         <chr> <chr>     <dbl> <dbl> <dbl>   <dbl> <int> <chr> <lgl>                
       1 psu_1 stratum_1   378  14.3  50.9 -0.122      1 A     FALSE                
       2 psu_1 stratum_1   378  21.8  20.1  0.188      0 B     FALSE                
       3 psu_1 stratum_1   378  14.4  52.8  0.119      1 B     FALSE                
       4 psu_2 stratum_1   378  18.9  46.3 -0.0251     0 A     FALSE                
       5 psu_2 stratum_1   378  23.0  51.9  0.108      0 B     FALSE                
       6 psu_2 stratum_1   378  11.0  55.8 -0.485      0 C     FALSE                
       7 psu_2 stratum_1   378  13.8  64.0 -0.504      0 A     FALSE                
       8 psu_2 stratum_1   378  14.2  42.7 -1.66       0 B     FALSE                
       9 psu_3 stratum_1   378  16.5  63.0 -0.382      1 C     FALSE                
      10 psu_3 stratum_1   378  13.7  53.4 -0.513      1 A     FALSE                
      # i 40 more rows

# print.survey_taylor shows per-stage FPC for 2-stage design

    Code
      print(sc, design_info = TRUE)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_taylor> (Taylor series linearization)
      Sample size: 200
      
      
      -- Design specification --
      
      * IDs: psu and ssu (20 PSUs)
      * Strata: strata (5 strata)
      * Weights: wt
      * Weights provided as: sampling weights
      * FPC (stage 1): fpc
      * FPC (stage 2): fpc2
      * Nesting: FALSE
      
      Design variables: psu, ssu, wt, strata, fpc, and fpc2
      
    Output
      # A tibble: 200 x 10
         psu   strata      fpc    wt    y1      y2    y3 group ssu       fpc2
         <chr> <chr>     <dbl> <dbl> <dbl>   <dbl> <int> <chr> <chr>    <int>
       1 psu_1 stratum_1   365  9.09  61.7  0.606      0 A     psu_1_s1    10
       2 psu_1 stratum_1   365 10.4   35.4  1.34       0 B     psu_1_s2    10
       3 psu_1 stratum_1   365  8.25  51.0  0.767      0 A     psu_1_s3    10
       4 psu_1 stratum_1   365 10.8   58.5  0.194      0 B     psu_1_s4    10
       5 psu_1 stratum_1   365  7.70  33.8  1.14       1 C     psu_1_s5    10
       6 psu_1 stratum_1   365  9.43  64.1  0.0139     1 B     psu_1_s1    10
       7 psu_1 stratum_1   365 10.6   44.6 -1.11       0 C     psu_1_s2    10
       8 psu_1 stratum_1   365 10.1   52.8 -0.0252     0 B     psu_1_s3    10
       9 psu_1 stratum_1   365 11.6   48.1 -0.164      0 B     psu_1_s4    10
      10 psu_1 stratum_1   365  9.75  65.8  0.370      0 C     psu_1_s5    10
      # i 190 more rows

# print.survey_collection snapshot: small 3-survey collection

    Code
      print(coll)
    Message
      A <survey_collection> with 3 surveys:
      id: ".survey"
      if_missing_var: "error"
      "2017-18": survey_taylor, 1,200 rows, 8 variables
      "2019-20": survey_taylor, 1,500 rows, 8 variables
      "2021-22": survey_taylor, 2,000 rows, 8 variables

# print.survey_collection snapshot: length-25 abbreviation

    Code
      print(coll)
    Message
      A <survey_collection> with 25 surveys:
      id: ".survey"
      if_missing_var: "error"
      "wave_01": survey_taylor, 40 rows, 8 variables
      "wave_02": survey_taylor, 40 rows, 8 variables
      "wave_03": survey_taylor, 40 rows, 8 variables
      "wave_04": survey_taylor, 40 rows, 8 variables
      "wave_05": survey_taylor, 40 rows, 8 variables
      "wave_06": survey_taylor, 40 rows, 8 variables
      "wave_07": survey_taylor, 40 rows, 8 variables
      "wave_08": survey_taylor, 40 rows, 8 variables
      "wave_09": survey_taylor, 40 rows, 8 variables
      "wave_10": survey_taylor, 40 rows, 8 variables
      ... and 12 more
      "wave_23": survey_taylor, 40 rows, 8 variables
      "wave_24": survey_taylor, 40 rows, 8 variables
      "wave_25": survey_taylor, 40 rows, 8 variables

# print.survey_collection snapshot: length-1 pluralisation

    Code
      print(coll)
    Message
      A <survey_collection> with 1 survey:
      id: ".survey"
      if_missing_var: "error"
      "only": survey_taylor, 50 rows, 8 variables

# print.survey_nonprob() shows BOOTSTRAP class line with repweights

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_nonprob> (non-probability, BOOTSTRAP, 10 replicates) [experimental]
      Sample size: 100
      
    Output
      # A tibble: 100 x 18
         psu   strata      fpc    wt    y1      y2    y3 group repwt_1 repwt_2 repwt_3
         <chr> <chr>     <dbl> <dbl> <dbl>   <dbl> <int> <chr>   <dbl>   <dbl>   <dbl>
       1 psu_1 stratum_1   298 13.2   61.2 -0.466      0 B      39.0    11.3      5.73
       2 psu_2 stratum_1   298 16.5   64.4 -0.269      0 C      21.4     5.87    27.5 
       3 psu_2 stratum_1   298 10.6   39.0 -0.391      0 A      49.0     0.334    4.36
       4 psu_3 stratum_1   298 12.7   48.8  1.35       1 B       1.73   34.2     14.0 
       5 psu_4 stratum_1   298 12.6   62.0 -0.0228     0 A       2.13   16.7      8.91
       6 psu_4 stratum_1   298  9.19  45.3  0.244      0 B       0.766   2.32     3.94
       7 psu_4 stratum_1   298 15.0   49.5 -0.942      1 A      34.6    60.8     25.4 
       8 psu_5 stratum_1   298 15.5   49.1 -0.729      0 B       8.21    5.74     5.39
       9 psu_5 stratum_1   298 13.9   41.1  0.998      1 C       7.15   44.6     25.4 
      10 psu_5 stratum_1   298 17.3   45.6  1.26       1 B       1.61    0.395    8.04
      # i 90 more rows
      # i 7 more variables: repwt_4 <dbl>, repwt_5 <dbl>, repwt_6 <dbl>,
      #   repwt_7 <dbl>, repwt_8 <dbl>, repwt_9 <dbl>, repwt_10 <dbl>

# print.survey_nonprob() shows SRS approximation note without repweights

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_nonprob> (non-probability) [experimental]
      * Variance: SRS approximation (no bootstrap replicate weights)
      Sample size: 50
      
    Output
      # A tibble: 50 x 2
              y    wt
          <dbl> <dbl>
       1 -0.415 1.38 
       2  1.98  0.808
       3  0.460 0.715
       4 -1.13  1.45 
       5 -1.78  2.36 
       6 -0.393 1.09 
       7 -0.767 1.41 
       8 -0.380 1.82 
       9  0.370 1.33 
      10 -0.625 0.631
      # i 40 more rows

# print.survey_nonprob() with repweights and design_info = TRUE shows replicate bullets

    Code
      print(d, design_info = TRUE)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_nonprob> (non-probability, BOOTSTRAP, 10 replicates) [experimental]
      Sample size: 100
      
      
      -- Design specification --
      
      * Weights: wt
      * Replicate weights: 10 columns (repwt_1, repwt_2, repwt_3, repwt_4, repwt_5,
        repwt_6, repwt_7, repwt_8, repwt_9, repwt_10)
      * Type: "bootstrap"
      * Scale: 0.1
      * Calibration provenance: none stored
      
      Design variables: wt
      
    Output
      # A tibble: 100 x 18
         psu   strata      fpc    wt    y1      y2    y3 group repwt_1 repwt_2 repwt_3
         <chr> <chr>     <dbl> <dbl> <dbl>   <dbl> <int> <chr>   <dbl>   <dbl>   <dbl>
       1 psu_1 stratum_1   298 13.2   61.2 -0.466      0 B      39.0    11.3      5.73
       2 psu_2 stratum_1   298 16.5   64.4 -0.269      0 C      21.4     5.87    27.5 
       3 psu_2 stratum_1   298 10.6   39.0 -0.391      0 A      49.0     0.334    4.36
       4 psu_3 stratum_1   298 12.7   48.8  1.35       1 B       1.73   34.2     14.0 
       5 psu_4 stratum_1   298 12.6   62.0 -0.0228     0 A       2.13   16.7      8.91
       6 psu_4 stratum_1   298  9.19  45.3  0.244      0 B       0.766   2.32     3.94
       7 psu_4 stratum_1   298 15.0   49.5 -0.942      1 A      34.6    60.8     25.4 
       8 psu_5 stratum_1   298 15.5   49.1 -0.729      0 B       8.21    5.74     5.39
       9 psu_5 stratum_1   298 13.9   41.1  0.998      1 C       7.15   44.6     25.4 
      10 psu_5 stratum_1   298 17.3   45.6  1.26       1 B       1.61    0.395    8.04
      # i 90 more rows
      # i 7 more variables: repwt_4 <dbl>, repwt_5 <dbl>, repwt_6 <dbl>,
      #   repwt_7 <dbl>, repwt_8 <dbl>, repwt_9 <dbl>, repwt_10 <dbl>

# print(survey_nonprob): JK1 snapshot

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_nonprob> (non-probability, JK1, 4 replicates) [experimental]
      Sample size: 10
      
    Output
      # A tibble: 10 x 6
             y    wt    r1    r2    r3    r4
         <int> <dbl> <dbl> <dbl> <dbl> <dbl>
       1     1     1     1     1     1     1
       2     2     1     1     1     1     1
       3     3     1     1     1     1     1
       4     4     1     1     1     1     1
       5     5     1     1     1     1     1
       6     6     1     1     1     1     1
       7     7     1     1     1     1     1
       8     8     1     1     1     1     1
       9     9     1     1     1     1     1
      10    10     1     1     1     1     1

# print(survey_nonprob): JK2 snapshot

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_nonprob> (non-probability, JK2, 4 replicates) [experimental]
      Sample size: 10
      
    Output
      # A tibble: 10 x 6
             y    wt    r1    r2    r3    r4
         <int> <dbl> <dbl> <dbl> <dbl> <dbl>
       1     1     1     1     1     1     1
       2     2     1     1     1     1     1
       3     3     1     1     1     1     1
       4     4     1     1     1     1     1
       5     5     1     1     1     1     1
       6     6     1     1     1     1     1
       7     7     1     1     1     1     1
       8     8     1     1     1     1     1
       9     9     1     1     1     1     1
      10    10     1     1     1     1     1

# print(survey_nonprob): JKn snapshot

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_nonprob> (non-probability, JKN, 4 replicates) [experimental]
      Sample size: 10
      
    Output
      # A tibble: 10 x 6
             y    wt    r1    r2    r3    r4
         <int> <dbl> <dbl> <dbl> <dbl> <dbl>
       1     1     1     1     1     1     1
       2     2     1     1     1     1     1
       3     3     1     1     1     1     1
       4     4     1     1     1     1     1
       5     5     1     1     1     1     1
       6     6     1     1     1     1     1
       7     7     1     1     1     1     1
       8     8     1     1     1     1     1
       9     9     1     1     1     1     1
      10    10     1     1     1     1     1

# print(survey_nonprob): bootstrap snapshot [regression guard]

    Code
      print(d)
    Message
      
      -- Survey Design ---------------------------------------------------------------
      <survey_nonprob> (non-probability, BOOTSTRAP, 4 replicates) [experimental]
      Sample size: 10
      
    Output
      # A tibble: 10 x 6
             y    wt    r1    r2    r3    r4
         <int> <dbl> <dbl> <dbl> <dbl> <dbl>
       1     1     1     1     1     1     1
       2     2     1     1     1     1     1
       3     3     1     1     1     1     1
       4     4     1     1     1     1     1
       5     5     1     1     1     1     1
       6     6     1     1     1     1     1
       7     7     1     1     1     1     1
       8     8     1     1     1     1     1
       9     9     1     1     1     1     1
      10    10     1     1     1     1     1

# summary(survey_nonprob): JK1 snapshot

    Code
      summary(d)
    Message
      
      -- Survey Design Summary -------------------------------------------------------
      Type: non-probability, JK1 replicates [experimental]
      Sample size: 10
      Weighted N: 10
      
      
      -- Design --
      
      Weights: wt
      * Range: 1 – 1
      * Mean: 1
      * CV: 0
      Calibration provenance: none stored
      
      Metadata: 0 of 6 variable(s) labeled

# summary(survey_nonprob): JK2 snapshot

    Code
      summary(d)
    Message
      
      -- Survey Design Summary -------------------------------------------------------
      Type: non-probability, JK2 replicates [experimental]
      Sample size: 10
      Weighted N: 10
      
      
      -- Design --
      
      Weights: wt
      * Range: 1 – 1
      * Mean: 1
      * CV: 0
      Calibration provenance: none stored
      
      Metadata: 0 of 6 variable(s) labeled

# summary(survey_nonprob): JKn snapshot

    Code
      summary(d)
    Message
      
      -- Survey Design Summary -------------------------------------------------------
      Type: non-probability, JKn replicates [experimental]
      Sample size: 10
      Weighted N: 10
      
      
      -- Design --
      
      Weights: wt
      * Range: 1 – 1
      * Mean: 1
      * CV: 0
      Calibration provenance: none stored
      
      Metadata: 0 of 6 variable(s) labeled

# summary(survey_nonprob): bootstrap snapshot [regression guard]

    Code
      summary(d)
    Message
      
      -- Survey Design Summary -------------------------------------------------------
      Type: non-probability, bootstrap replicates [experimental]
      Sample size: 10
      Weighted N: 10
      
      
      -- Design --
      
      Weights: wt
      * Range: 1 – 1
      * Mean: 1
      * CV: 0
      Calibration provenance: none stored
      
      Metadata: 0 of 6 variable(s) labeled

