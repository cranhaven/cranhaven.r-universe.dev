# `nn_domain_score()` works

    Code
      nn_domain_score(featured_col, train_data, nn_hyperparameters, test_data,
        threshold_value)
    Output
      # A tibble: 53 x 3
         .pred distance distance_pctl
         <dbl>    <dbl>         <dbl>
       1  480.    0.424          9.08
       2  480.    1.32          69.9 
       3  480.    1.01          60.7 
       4  480.    0.356          4.37
       5  480.    1.33          70.2 
       6  480.    0.452         18.7 
       7  474.    1.11          66.1 
       8  480.    0.321          3.49
       9  170.    0.573         25.3 
      10  480.    0.660         38.6 
      # i 43 more rows

