# `knn_domain_score()` works

    Code
      knn_domain_score(featured_col, train_data, knn_hyperparameters, test_data,
        threshold_value)
    Output
      # A tibble: 53 x 3
         .pred distance distance_pctl
         <dbl>    <dbl>         <dbl>
       1  528.    0.424          9.08
       2  405.    1.32          69.9 
       3  545.    1.01          60.7 
       4  599.    0.356          4.37
       5  585.    1.33          70.2 
       6  371.    0.452         18.7 
       7  483.    1.11          66.1 
       8  405.    0.321          3.49
       9  291.    0.573         25.3 
      10  405.    0.660         38.6 
      # i 43 more rows

