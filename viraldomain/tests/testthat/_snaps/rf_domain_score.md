# `rf_domain_score()` works

    Code
      rf_domain_score(featured_col, train_data, rf_hyperparameters, test_data,
        threshold_value)
    Condition
      Warning:
      ! 2 columns were requested but there was 1 predictor in the data.
      i 1 predictor will be used.
    Output
      # A tibble: 53 x 3
         .pred distance distance_pctl
         <dbl>    <dbl>         <dbl>
       1  485.    0.424          9.08
       2  368.    1.32          69.9 
       3  674.    1.01          60.7 
       4  611.    0.356          4.37
       5  603.    1.33          70.2 
       6  402.    0.452         18.7 
       7  564.    1.11          66.1 
       8  442.    0.321          3.49
       9  243.    0.573         25.3 
      10  368.    0.660         38.6 
      # i 43 more rows

