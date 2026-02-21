# resample stretching works

    Code
      vascr_summarise_cc_stretch_shift_stats(data.df, reference = 3)
    Output
      # A tibble: 5 x 12
        name  title          p   mean      sd nsample ncontrol Sample.x Sample.y refs 
        <chr> <chr>      <dbl>  <dbl>   <dbl>   <int>    <int> <chr>    <chr>    <chr>
      1 cc    35,000_~ 2.16e-1  0.967 0.0231        3        3 35,000_~ 35,000_~ 0.99~
      2 cc    35,000_~ 3.03e-1  0.985 0.00649       3        3 35,000_~ 25,000_~ 0.99~
      3 cc    25,000_~ 1   e+0  0.991 0.00448       3        3 25,000_~ 25,000_~ 0.99~
      4 cc    0_cells~ 2.00e-7 -0.988 0.00990       3        3 0_cells~ 25,000_~ 0.99~
      5 cc    0_cells~ 1.94e-1  0.996 0.00325       3        3 0_cells~ 0_cells~ 0.99~
      # i 2 more variables: padj <dbl>, stars <noquote>

