# `mars_domain_score()` works

    Code
      mars_domain_score(featured_col, train_data, mars_hyperparameters, test_data,
        threshold_value)
    Output
      # A tibble: 53 x 3
         .pred distance distance_pctl
         <dbl>    <dbl>         <dbl>
       1  460.    0.424          9.08
       2  460.    1.32          69.9 
       3  460.    1.01          60.7 
       4  460.    0.356          4.37
       5  460.    1.33          70.2 
       6  461.    0.452         18.7 
       7  460.    1.11          66.1 
       8  460.    0.321          3.49
       9  460.    0.573         25.3 
      10  460.    0.660         38.6 
      # i 43 more rows

