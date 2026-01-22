# dimension reduction works

    Code
      tmp
    Output
      Index pipeline: 
      
      Steps: 
    Message
      dimension_reduction: `aggregate_linear()` -> index_new
    Output
      
      Data: 
      # A tibble: 146 x 23
         country      region index  rank economic_participati~1 educational_attainment
         <chr>        <chr>  <dbl> <dbl>                  <dbl>                  <dbl>
       1 Afghanistan  South~ 0.405   146                  0.189                  0.482
       2 Chad         Sub-S~ 0.57    145                  0.538                  0.637
       3 Algeria      Middl~ 0.573   144                  0.317                  0.951
       4 Iran (Islam~ Middl~ 0.575   143                  0.344                  0.96 
       5 Pakistan     South~ 0.575   142                  0.362                  0.825
       6 Mali         Sub-S~ 0.605   141                  0.489                  0.779
       7 Congo, Demo~ Sub-S~ 0.612   140                  0.676                  0.683
       8 Oman         Middl~ 0.614   139                  0.488                  0.957
       9 Benin        Sub-S~ 0.616   138                  0.53                   0.802
      10 Guinea       Sub-S~ 0.617   137                  0.576                  0.71 
      # i 136 more rows
      # i abbreviated name: 1: economic_participation_and_opportunity
      # i 17 more variables: health_and_survival <dbl>, political_empowerment <dbl>,
      #   labour_force_participation <dbl>, wage_equality_for_similar_work <dbl>,
      #   estimated_earned_income <dbl>,
      #   legislators_senior_officials_and_managers <dbl>,
      #   professional_and_technical_workers <dbl>, literacy_rate <dbl>, ...

---

    Code
      tmp2
    Output
      Index pipeline: 
      
      Steps: 
    Message
      rescaling: `rescale_minmax()` -> life_exp
      rescaling: `rescale_minmax()` -> exp_sch
      rescaling: `rescale_minmax()` -> avg_sch
      rescaling: `rescale_minmax()` -> gni_pc
      dimension_reduction: `aggregate_manual()` -> sch
    Output
      
      Data: 
      # A tibble: 191 x 9
            id country                 hdi  rank life_exp exp_sch avg_sch gni_pc   sch
         <dbl> <chr>                 <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl> <dbl>
       1     1 Switzerland           0.962     3    0.984   0.917   0.924  0.983 0.920
       2     2 Norway                0.961     1    0.973   1       0.867  0.978 0.933
       3     3 Iceland               0.959     2    0.964   1       0.918  0.955 0.959
       4     4 Hong Kong, China (SA~ 0.952     4    1       0.960   0.815  0.973 0.887
       5     5 Australia             0.951     5    0.993   1       0.848  0.936 0.924
       6     6 Denmark               0.948     5    0.944   1       0.864  0.967 0.932
       7     7 Sweden                0.947     9    0.969   1       0.841  0.952 0.920
       8     8 Ireland               0.945     8    0.954   1       0.772  1     0.886
       9     9 Germany               0.942     7    0.933   0.945   0.939  0.952 0.942
      10    10 Netherlands           0.941    10    0.949   1       0.839  0.956 0.919
      # i 181 more rows

---

    Code
      tmp3
    Output
      Index pipeline: 
      
      Steps: 
    Message
      rescaling: `rescale_minmax()` -> life_exp
      rescaling: `rescale_minmax()` -> exp_sch
      rescaling: `rescale_minmax()` -> avg_sch
      rescaling: `rescale_minmax()` -> gni_pc
      dimension_reduction: `aggregate_manual()` -> sch
      dimension_reduction: `aggregate_geometrical()` -> index
    Output
      
      Data: 
      # A tibble: 191 x 10
            id country           hdi  rank life_exp exp_sch avg_sch gni_pc   sch index
         <dbl> <chr>           <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl> <dbl> <dbl>
       1     1 Switzerland     0.962     3    0.984   0.917   0.924  0.983 0.920 0.962
       2     2 Norway          0.961     1    0.973   1       0.867  0.978 0.933 0.961
       3     3 Iceland         0.959     2    0.964   1       0.918  0.955 0.959 0.959
       4     4 Hong Kong, Chi~ 0.952     4    1       0.960   0.815  0.973 0.887 0.952
       5     5 Australia       0.951     5    0.993   1       0.848  0.936 0.924 0.951
       6     6 Denmark         0.948     5    0.944   1       0.864  0.967 0.932 0.948
       7     7 Sweden          0.947     9    0.969   1       0.841  0.952 0.920 0.947
       8     8 Ireland         0.945     8    0.954   1       0.772  1     0.886 0.945
       9     9 Germany         0.942     7    0.933   0.945   0.939  0.952 0.942 0.942
      10    10 Netherlands     0.941    10    0.949   1       0.839  0.956 0.919 0.941
      # i 181 more rows

# on errors

    Code
      dimension_reduction(hdi, eco = aggregate_manual(~ labour_force_participation *
        0.199 + wage_equality_for_similar_work * 0.31))
    Condition
      Error in `check_idx_tbl()`:
      ! A index table object is required as input. Created it using `init()`.

---

    Code
      dimension_reduction(init(hdi), index = rescale_zscore(life_exp))
    Condition
      Error in `check_dim_red_obj()`:
      ! A dimension reduction object is required as input. Create it using `aggregate_*()`

