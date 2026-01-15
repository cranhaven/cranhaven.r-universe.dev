# empirical hazards are joined as expected

    Code
      km_haz_joined
    Output
      # A tibble: 206 x 10
          time n.risk n.event n.censor estimate std.error conf.high conf.low strata
         <dbl>  <dbl>   <dbl>    <dbl>    <dbl>     <dbl>     <dbl>    <dbl> <chr> 
       1    11    138       3        0    0.978    0.0127     1        0.954 sex=1 
       2    12    135       1        0    0.971    0.0147     0.999    0.943 sex=1 
       3    13    134       2        0    0.957    0.0181     0.991    0.923 sex=1 
       4    15    132       1        0    0.949    0.0197     0.987    0.913 sex=1 
       5    26    131       1        0    0.942    0.0211     0.982    0.904 sex=1 
       6    30    130       1        0    0.935    0.0225     0.977    0.894 sex=1 
       7    31    129       1        0    0.928    0.0238     0.972    0.885 sex=1 
       8    53    128       2        0    0.913    0.0263     0.961    0.867 sex=1 
       9    54    126       1        0    0.906    0.0275     0.956    0.858 sex=1 
      10    59    125       1        0    0.899    0.0286     0.950    0.850 sex=1 
      # i 196 more rows
      # i 1 more variable: hazard_estimate <dbl>

