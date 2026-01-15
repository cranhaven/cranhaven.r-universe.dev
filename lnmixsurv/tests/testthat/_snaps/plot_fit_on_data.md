# plot_fit_on_data works for Bayesian model with covariates

    Code
      plot_fit_on_data(mod, data_model, interval = "credible", level = 0.95, type = "survival")$
        preds
    Output
      # A tibble: 10,000 x 12
          time n.risk n.event n.censor estimate std.error conf.high conf.low strata
         <dbl>  <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>    <dbl> <chr> 
       1  3.09   6034       0        1     1     0                1     1    x=0   
       2  3.17   6033       0        1     1     0                1     1    x=0   
       3  3.19   6032       0        1     1     0                1     1    x=0   
       4  3.65   6031       0        1     1     0                1     1    x=0   
       5  4.01   6030       0        1     1     0                1     1    x=0   
       6  4.10   6029       0        1     1     0                1     1    x=0   
       7  4.27   6028       0        1     1     0                1     1    x=0   
       8  4.27   6027       1        0     1.00  0.000166         1     1.00 x=0   
       9  4.57   6026       0        1     1.00  0.000166         1     1.00 x=0   
      10  4.86   6025       0        1     1.00  0.000166         1     1.00 x=0   
      # i 9,990 more rows
      # i 3 more variables: .pred_survival <dbl>, .pred_lower <dbl>,
      #   .pred_upper <dbl>

---

    Code
      plot_fit_on_data(mod, data_model, interval = "credible", level = 0.95, type = "hazard")$
        preds
    Output
      # A tibble: 10,000 x 13
          time n.risk n.event n.censor estimate std.error conf.high conf.low strata
         <dbl>  <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>    <dbl> <chr> 
       1  3.09   6034       0        1     1     0                1     1    x=0   
       2  3.17   6033       0        1     1     0                1     1    x=0   
       3  3.19   6032       0        1     1     0                1     1    x=0   
       4  3.65   6031       0        1     1     0                1     1    x=0   
       5  4.01   6030       0        1     1     0                1     1    x=0   
       6  4.10   6029       0        1     1     0                1     1    x=0   
       7  4.27   6028       0        1     1     0                1     1    x=0   
       8  4.27   6027       1        0     1.00  0.000166         1     1.00 x=0   
       9  4.57   6026       0        1     1.00  0.000166         1     1.00 x=0   
      10  4.86   6025       0        1     1.00  0.000166         1     1.00 x=0   
      # i 9,990 more rows
      # i 4 more variables: hazard_estimate <dbl>, .pred_hazard <dbl>,
      #   .pred_lower <dbl>, .pred_upper <dbl>

---

    Code
      plot_fit_on_data(mod, data_model, type = "survival")$preds
    Output
      # A tibble: 10,000 x 10
          time n.risk n.event n.censor estimate std.error conf.high conf.low strata
         <dbl>  <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>    <dbl> <chr> 
       1  3.09   6034       0        1     1     0                1     1    x=0   
       2  3.17   6033       0        1     1     0                1     1    x=0   
       3  3.19   6032       0        1     1     0                1     1    x=0   
       4  3.65   6031       0        1     1     0                1     1    x=0   
       5  4.01   6030       0        1     1     0                1     1    x=0   
       6  4.10   6029       0        1     1     0                1     1    x=0   
       7  4.27   6028       0        1     1     0                1     1    x=0   
       8  4.27   6027       1        0     1.00  0.000166         1     1.00 x=0   
       9  4.57   6026       0        1     1.00  0.000166         1     1.00 x=0   
      10  4.86   6025       0        1     1.00  0.000166         1     1.00 x=0   
      # i 9,990 more rows
      # i 1 more variable: .pred_survival <dbl>

---

    Code
      plot_fit_on_data(mod, data_model, type = "hazard")$preds
    Output
      # A tibble: 10,000 x 11
          time n.risk n.event n.censor estimate std.error conf.high conf.low strata
         <dbl>  <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>    <dbl> <chr> 
       1  3.09   6034       0        1     1     0                1     1    x=0   
       2  3.17   6033       0        1     1     0                1     1    x=0   
       3  3.19   6032       0        1     1     0                1     1    x=0   
       4  3.65   6031       0        1     1     0                1     1    x=0   
       5  4.01   6030       0        1     1     0                1     1    x=0   
       6  4.10   6029       0        1     1     0                1     1    x=0   
       7  4.27   6028       0        1     1     0                1     1    x=0   
       8  4.27   6027       1        0     1.00  0.000166         1     1.00 x=0   
       9  4.57   6026       0        1     1.00  0.000166         1     1.00 x=0   
      10  4.86   6025       0        1     1.00  0.000166         1     1.00 x=0   
      # i 9,990 more rows
      # i 2 more variables: hazard_estimate <dbl>, .pred_hazard <dbl>

# plot_fit_on_data works for Bayesian model with intercept only

    Code
      plot_fit_on_data(mod, data_model, interval = "credible", level = 0.95, type = "survival")$
        preds
    Output
      # A tibble: 9,999 x 11
          time n.risk n.event n.censor estimate std.error conf.high conf.low
         <dbl>  <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>    <dbl>
       1  3.09  10000       0        1     1     0                1     1   
       2  3.17   9999       0        1     1     0                1     1   
       3  3.19   9998       0        1     1     0                1     1   
       4  3.65   9997       0        1     1     0                1     1   
       5  4.01   9996       0        1     1     0                1     1   
       6  4.10   9995       0        1     1     0                1     1   
       7  4.27   9994       0        1     1     0                1     1   
       8  4.27   9993       1        0     1.00  0.000100         1     1.00
       9  4.57   9992       0        1     1.00  0.000100         1     1.00
      10  4.86   9991       0        1     1.00  0.000100         1     1.00
      # i 9,989 more rows
      # i 3 more variables: .pred_survival <dbl>, .pred_lower <dbl>,
      #   .pred_upper <dbl>

---

    Code
      plot_fit_on_data(mod, data_model, interval = "credible", level = 0.95, type = "hazard")$
        preds
    Output
      # A tibble: 9,999 x 12
          time n.risk n.event n.censor estimate std.error conf.high conf.low
         <dbl>  <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>    <dbl>
       1  3.09  10000       0        1     1     0                1     1   
       2  3.17   9999       0        1     1     0                1     1   
       3  3.19   9998       0        1     1     0                1     1   
       4  3.65   9997       0        1     1     0                1     1   
       5  4.01   9996       0        1     1     0                1     1   
       6  4.10   9995       0        1     1     0                1     1   
       7  4.27   9994       0        1     1     0                1     1   
       8  4.27   9993       1        0     1.00  0.000100         1     1.00
       9  4.57   9992       0        1     1.00  0.000100         1     1.00
      10  4.86   9991       0        1     1.00  0.000100         1     1.00
      # i 9,989 more rows
      # i 4 more variables: hazard_estimate <dbl>, .pred_hazard <dbl>,
      #   .pred_lower <dbl>, .pred_upper <dbl>

---

    Code
      plot_fit_on_data(mod, data_model, type = "survival")$preds
    Output
      # A tibble: 9,999 x 9
          time n.risk n.event n.censor estimate std.error conf.high conf.low
         <dbl>  <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>    <dbl>
       1  3.09  10000       0        1     1     0                1     1   
       2  3.17   9999       0        1     1     0                1     1   
       3  3.19   9998       0        1     1     0                1     1   
       4  3.65   9997       0        1     1     0                1     1   
       5  4.01   9996       0        1     1     0                1     1   
       6  4.10   9995       0        1     1     0                1     1   
       7  4.27   9994       0        1     1     0                1     1   
       8  4.27   9993       1        0     1.00  0.000100         1     1.00
       9  4.57   9992       0        1     1.00  0.000100         1     1.00
      10  4.86   9991       0        1     1.00  0.000100         1     1.00
      # i 9,989 more rows
      # i 1 more variable: .pred_survival <dbl>

---

    Code
      plot_fit_on_data(mod, data_model, type = "hazard")$preds
    Output
      # A tibble: 9,999 x 10
          time n.risk n.event n.censor estimate std.error conf.high conf.low
         <dbl>  <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>    <dbl>
       1  3.09  10000       0        1     1     0                1     1   
       2  3.17   9999       0        1     1     0                1     1   
       3  3.19   9998       0        1     1     0                1     1   
       4  3.65   9997       0        1     1     0                1     1   
       5  4.01   9996       0        1     1     0                1     1   
       6  4.10   9995       0        1     1     0                1     1   
       7  4.27   9994       0        1     1     0                1     1   
       8  4.27   9993       1        0     1.00  0.000100         1     1.00
       9  4.57   9992       0        1     1.00  0.000100         1     1.00
      10  4.86   9991       0        1     1.00  0.000100         1     1.00
      # i 9,989 more rows
      # i 2 more variables: hazard_estimate <dbl>, .pred_hazard <dbl>

# plot_fit_on_data works for EM model with covariates

    Code
      plot_fit_on_data(mod, data_model, type = "survival")$preds
    Output
      # A tibble: 10,000 x 10
          time n.risk n.event n.censor estimate std.error conf.high conf.low strata
         <dbl>  <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>    <dbl> <chr> 
       1  3.09   6034       0        1     1     0                1     1    x=0   
       2  3.17   6033       0        1     1     0                1     1    x=0   
       3  3.19   6032       0        1     1     0                1     1    x=0   
       4  3.65   6031       0        1     1     0                1     1    x=0   
       5  4.01   6030       0        1     1     0                1     1    x=0   
       6  4.10   6029       0        1     1     0                1     1    x=0   
       7  4.27   6028       0        1     1     0                1     1    x=0   
       8  4.27   6027       1        0     1.00  0.000166         1     1.00 x=0   
       9  4.57   6026       0        1     1.00  0.000166         1     1.00 x=0   
      10  4.86   6025       0        1     1.00  0.000166         1     1.00 x=0   
      # i 9,990 more rows
      # i 1 more variable: .pred_survival <dbl>

---

    Code
      plot_fit_on_data(mod, data_model, type = "hazard")$preds
    Output
      # A tibble: 10,000 x 11
          time n.risk n.event n.censor estimate std.error conf.high conf.low strata
         <dbl>  <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>    <dbl> <chr> 
       1  3.09   6034       0        1     1     0                1     1    x=0   
       2  3.17   6033       0        1     1     0                1     1    x=0   
       3  3.19   6032       0        1     1     0                1     1    x=0   
       4  3.65   6031       0        1     1     0                1     1    x=0   
       5  4.01   6030       0        1     1     0                1     1    x=0   
       6  4.10   6029       0        1     1     0                1     1    x=0   
       7  4.27   6028       0        1     1     0                1     1    x=0   
       8  4.27   6027       1        0     1.00  0.000166         1     1.00 x=0   
       9  4.57   6026       0        1     1.00  0.000166         1     1.00 x=0   
      10  4.86   6025       0        1     1.00  0.000166         1     1.00 x=0   
      # i 9,990 more rows
      # i 2 more variables: hazard_estimate <dbl>, .pred_hazard <dbl>

# plot_fit_on_data works for EM model with intercept only

    Code
      plot_fit_on_data(mod, data_model, type = "survival")$preds
    Output
      # A tibble: 9,999 x 9
          time n.risk n.event n.censor estimate std.error conf.high conf.low
         <dbl>  <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>    <dbl>
       1  3.09  10000       0        1     1     0                1     1   
       2  3.17   9999       0        1     1     0                1     1   
       3  3.19   9998       0        1     1     0                1     1   
       4  3.65   9997       0        1     1     0                1     1   
       5  4.01   9996       0        1     1     0                1     1   
       6  4.10   9995       0        1     1     0                1     1   
       7  4.27   9994       0        1     1     0                1     1   
       8  4.27   9993       1        0     1.00  0.000100         1     1.00
       9  4.57   9992       0        1     1.00  0.000100         1     1.00
      10  4.86   9991       0        1     1.00  0.000100         1     1.00
      # i 9,989 more rows
      # i 1 more variable: .pred_survival <dbl>

---

    Code
      plot_fit_on_data(mod, data_model, type = "hazard")$preds
    Output
      # A tibble: 9,999 x 10
          time n.risk n.event n.censor estimate std.error conf.high conf.low
         <dbl>  <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>    <dbl>
       1  3.09  10000       0        1     1     0                1     1   
       2  3.17   9999       0        1     1     0                1     1   
       3  3.19   9998       0        1     1     0                1     1   
       4  3.65   9997       0        1     1     0                1     1   
       5  4.01   9996       0        1     1     0                1     1   
       6  4.10   9995       0        1     1     0                1     1   
       7  4.27   9994       0        1     1     0                1     1   
       8  4.27   9993       1        0     1.00  0.000100         1     1.00
       9  4.57   9992       0        1     1.00  0.000100         1     1.00
      10  4.86   9991       0        1     1.00  0.000100         1     1.00
      # i 9,989 more rows
      # i 2 more variables: hazard_estimate <dbl>, .pred_hazard <dbl>

