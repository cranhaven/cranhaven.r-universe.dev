# multiplication works

    Code
      dt2
    Output
      # A tibble: 4 x 2
        .params values   
        <chr>   <list>   
      1 weight  <idx_tbl>
      2 weight2 <idx_tbl>
      3 weight3 <idx_tbl>
      4 weight4 <idx_tbl>

---

    Code
      augment(dt2)
    Output
      # A tibble: 764 x 12
         .id       id country   hdi  rank life_exp exp_sch avg_sch gni_pc   sch .index
         <chr>  <dbl> <chr>   <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl> <dbl> <chr> 
       1 weight     1 Switze~ 0.962     3    0.984   0.917   0.924  0.983 0.920 index 
       2 weight     2 Norway  0.961     1    0.973   1       0.867  0.978 0.933 index 
       3 weight     3 Iceland 0.959     2    0.964   1       0.918  0.955 0.959 index 
       4 weight     4 Hong K~ 0.952     4    1       0.960   0.815  0.973 0.887 index 
       5 weight     5 Austra~ 0.951     5    0.993   1       0.848  0.936 0.924 index 
       6 weight     6 Denmark 0.948     5    0.944   1       0.864  0.967 0.932 index 
       7 weight     7 Sweden  0.947     9    0.969   1       0.841  0.952 0.920 index 
       8 weight     8 Ireland 0.945     8    0.954   1       0.772  1     0.886 index 
       9 weight     9 Germany 0.942     7    0.933   0.945   0.939  0.952 0.942 index 
      10 weight    10 Nether~ 0.941    10    0.949   1       0.839  0.956 0.919 index 
      # i 754 more rows
      # i 1 more variable: .value <dbl>

---

    Code
      dt22
    Output
      # A tibble: 2 x 2
        .params values   
        <chr>   <list>   
      1 weight  <idx_tbl>
      2 weight2 <idx_tbl>

---

    Code
      augment(dt22)
    Output
      # A tibble: 382 x 12
         .id       id country   hdi  rank life_exp exp_sch avg_sch gni_pc   sch .index
         <chr>  <dbl> <chr>   <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl> <dbl> <chr> 
       1 weight     1 Switze~ 0.962     3    0.984   0.917   0.924  0.983 0.920 index 
       2 weight     2 Norway  0.961     1    0.973   1       0.867  0.978 0.933 index 
       3 weight     3 Iceland 0.959     2    0.964   1       0.918  0.955 0.959 index 
       4 weight     4 Hong K~ 0.952     4    1       0.960   0.815  0.973 0.887 index 
       5 weight     5 Austra~ 0.951     5    0.993   1       0.848  0.936 0.924 index 
       6 weight     6 Denmark 0.948     5    0.944   1       0.864  0.967 0.932 index 
       7 weight     7 Sweden  0.947     9    0.969   1       0.841  0.952 0.920 index 
       8 weight     8 Ireland 0.945     8    0.954   1       0.772  1     0.886 index 
       9 weight     9 Germany 0.942     7    0.933   0.945   0.939  0.952 0.942 index 
      10 weight    10 Nether~ 0.941    10    0.949   1       0.839  0.956 0.919 index 
      # i 372 more rows
      # i 1 more variable: .value <dbl>

---

    Code
      dt3
    Output
      # A tibble: 2 x 2
        .exprs values   
         <int> <list>   
      1      1 <idx_tbl>
      2      2 <idx_tbl>

---

    Code
      augment(dt3)
    Output
      # A tibble: 382 x 12
         .id      id country    hdi  rank life_exp exp_sch avg_sch gni_pc   sch .index
         <chr> <dbl> <chr>    <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl> <dbl> <chr> 
       1 1         1 Switzer~ 0.962     3    0.984   0.917   0.924  0.983 0.920 index 
       2 1         2 Norway   0.961     1    0.973   1       0.867  0.978 0.933 index 
       3 1         3 Iceland  0.959     2    0.964   1       0.918  0.955 0.959 index 
       4 1         4 Hong Ko~ 0.952     4    1       0.960   0.815  0.973 0.887 index 
       5 1         5 Austral~ 0.951     5    0.993   1       0.848  0.936 0.924 index 
       6 1         6 Denmark  0.948     5    0.944   1       0.864  0.967 0.932 index 
       7 1         7 Sweden   0.947     9    0.969   1       0.841  0.952 0.920 index 
       8 1         8 Ireland  0.945     8    0.954   1       0.772  1     0.886 index 
       9 1         9 Germany  0.942     7    0.933   0.945   0.939  0.952 0.942 index 
      10 1        10 Netherl~ 0.941    10    0.949   1       0.839  0.956 0.919 index 
      # i 372 more rows
      # i 1 more variable: .value <dbl>

