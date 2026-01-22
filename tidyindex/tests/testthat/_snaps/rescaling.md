# error msgs

    Code
      rescaling(hdi, life_exp2 = rescale_zscore(life_exp))
    Condition
      Error in `check_idx_tbl()`:
      ! A index table object is required as input. Created it using `init()`.

---

    Code
      rescaling(dt, life_exp2 = scale(dt$data$life_exp))
    Condition
      Error in `check_rescale_obj()`:
      ! A rescale object is required as input. Created it using `rescale_*()`.

# rescale calculation is correct

    Code
      rescaling(dt, life_exp2 = rescale_zscore(life_exp))
    Output
      Index pipeline: 
      
      Steps: 
    Message
      rescaling: `rescale_zscore()` -> life_exp2
    Output
      
      Data: 
      # A tibble: 191 x 9
            id country             hdi  rank life_exp exp_sch avg_sch gni_pc life_exp2
         <dbl> <chr>             <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl>     <dbl>
       1     1 Switzerland       0.962     3     84.0    16.5    13.9   4.83      1.66
       2     2 Norway            0.961     1     83.2    18.2    13.0   4.81      1.56
       3     3 Iceland           0.959     2     82.7    19.2    13.8   4.75      1.49
       4     4 Hong Kong, China~ 0.952     4     85.5    17.3    12.2   4.80      1.85
       5     5 Australia         0.951     5     84.5    21.1    12.7   4.69      1.73
       6     6 Denmark           0.948     5     81.4    18.7    13.0   4.78      1.32
       7     7 Sweden            0.947     9     83.0    19.4    12.6   4.74      1.53
       8     8 Ireland           0.945     8     82.0    18.9    11.6   4.88      1.40
       9     9 Germany           0.942     7     80.6    17.0    14.1   4.74      1.22
      10    10 Netherlands       0.941    10     81.7    18.7    12.6   4.75      1.36
      # i 181 more rows

---

    Code
      rescaling(dt, life_exp2 = rescale_minmax(life_exp, min = 20, max = 85))
    Output
      Index pipeline: 
      
      Steps: 
    Message
      rescaling: `rescale_minmax()` -> life_exp2
    Output
      
      Data: 
      # A tibble: 191 x 9
            id country             hdi  rank life_exp exp_sch avg_sch gni_pc life_exp2
         <dbl> <chr>             <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl>     <dbl>
       1     1 Switzerland       0.962     3     84.0    16.5    13.9   4.83     0.984
       2     2 Norway            0.961     1     83.2    18.2    13.0   4.81     0.973
       3     3 Iceland           0.959     2     82.7    19.2    13.8   4.75     0.964
       4     4 Hong Kong, China~ 0.952     4     85.5    17.3    12.2   4.80     1    
       5     5 Australia         0.951     5     84.5    21.1    12.7   4.69     0.993
       6     6 Denmark           0.948     5     81.4    18.7    13.0   4.78     0.944
       7     7 Sweden            0.947     9     83.0    19.4    12.6   4.74     0.969
       8     8 Ireland           0.945     8     82.0    18.9    11.6   4.88     0.954
       9     9 Germany           0.942     7     80.6    17.0    14.1   4.74     0.933
      10    10 Netherlands       0.941    10     81.7    18.7    12.6   4.75     0.949
      # i 181 more rows

---

    Code
      rescaling(dt, life_exp2 = rescale_minmax(life_exp))
    Output
      Index pipeline: 
      
      Steps: 
    Message
      rescaling: `rescale_minmax()` -> life_exp2
    Output
      
      Data: 
      # A tibble: 191 x 9
            id country             hdi  rank life_exp exp_sch avg_sch gni_pc life_exp2
         <dbl> <chr>             <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl>     <dbl>
       1     1 Switzerland       0.962     3     84.0    16.5    13.9   4.83     0.955
       2     2 Norway            0.961     1     83.2    18.2    13.0   4.81     0.932
       3     3 Iceland           0.959     2     82.7    19.2    13.8   4.75     0.915
       4     4 Hong Kong, China~ 0.952     4     85.5    17.3    12.2   4.80     1    
       5     5 Australia         0.951     5     84.5    21.1    12.7   4.69     0.971
       6     6 Denmark           0.948     5     81.4    18.7    13.0   4.78     0.876
       7     7 Sweden            0.947     9     83.0    19.4    12.6   4.74     0.924
       8     8 Ireland           0.945     8     82.0    18.9    11.6   4.88     0.895
       9     9 Germany           0.942     7     80.6    17.0    14.1   4.74     0.853
      10    10 Netherlands       0.941    10     81.7    18.7    12.6   4.75     0.885
      # i 181 more rows

---

    Code
      rescaling(dt, life_exp2 = rescale_center(life_exp))
    Output
      Index pipeline: 
      
      Steps: 
    Message
      rescaling: `rescale_center()` -> life_exp2
    Output
      
      Data: 
      # A tibble: 191 x 9
            id country             hdi  rank life_exp exp_sch avg_sch gni_pc life_exp2
         <dbl> <chr>             <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl>     <dbl>
       1     1 Switzerland       0.962     3     84.0    16.5    13.9   4.83     12.7 
       2     2 Norway            0.961     1     83.2    18.2    13.0   4.81     11.9 
       3     3 Iceland           0.959     2     82.7    19.2    13.8   4.75     11.4 
       4     4 Hong Kong, China~ 0.952     4     85.5    17.3    12.2   4.80     14.2 
       5     5 Australia         0.951     5     84.5    21.1    12.7   4.69     13.2 
       6     6 Denmark           0.948     5     81.4    18.7    13.0   4.78     10.1 
       7     7 Sweden            0.947     9     83.0    19.4    12.6   4.74     11.7 
       8     8 Ireland           0.945     8     82.0    18.9    11.6   4.88     10.7 
       9     9 Germany           0.942     7     80.6    17.0    14.1   4.74      9.32
      10    10 Netherlands       0.941    10     81.7    18.7    12.6   4.75     10.4 
      # i 181 more rows

# rescaling() works with symbols

    Code
      dt
    Output
      Index pipeline: 
      
      Steps: 
    Message
      rescaling: `rescale_minmax()` -> life_exp
    Output
      
      Data: 
      # A tibble: 191 x 8
            id country                  hdi  rank life_exp exp_sch avg_sch gni_pc
         <dbl> <chr>                  <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl>
       1     1 Switzerland            0.962     3    0.984    16.5    13.9   4.83
       2     2 Norway                 0.961     1    0.973    18.2    13.0   4.81
       3     3 Iceland                0.959     2    0.964    19.2    13.8   4.75
       4     4 Hong Kong, China (SAR) 0.952     4    1        17.3    12.2   4.80
       5     5 Australia              0.951     5    0.993    21.1    12.7   4.69
       6     6 Denmark                0.948     5    0.944    18.7    13.0   4.78
       7     7 Sweden                 0.947     9    0.969    19.4    12.6   4.74
       8     8 Ireland                0.945     8    0.954    18.9    11.6   4.88
       9     9 Germany                0.942     7    0.933    17.0    14.1   4.74
      10    10 Netherlands            0.941    10    0.949    18.7    12.6   4.75
      # i 181 more rows

