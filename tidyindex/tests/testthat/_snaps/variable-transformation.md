# variable transformation works

    Code
      variable_trans(init(hdi), gni_pc = trans_log10(gni_pc))
    Output
      Index pipeline: 
      
      Steps: 
    Message
      variable_transformation: `trans_log()` -> gni_pc
    Output
      
      Data: 
      # A tibble: 191 x 8
            id country                  hdi  rank life_exp exp_sch avg_sch gni_pc
         <dbl> <chr>                  <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl>
       1     1 Switzerland            0.962     3     84.0    16.5    13.9  0.684
       2     2 Norway                 0.961     1     83.2    18.2    13.0  0.682
       3     3 Iceland                0.959     2     82.7    19.2    13.8  0.676
       4     4 Hong Kong, China (SAR) 0.952     4     85.5    17.3    12.2  0.681
       5     5 Australia              0.951     5     84.5    21.1    12.7  0.671
       6     6 Denmark                0.948     5     81.4    18.7    13.0  0.679
       7     7 Sweden                 0.947     9     83.0    19.4    12.6  0.675
       8     8 Ireland                0.945     8     82.0    18.9    11.6  0.689
       9     9 Germany                0.942     7     80.6    17.0    14.1  0.675
      10    10 Netherlands            0.941    10     81.7    18.7    12.6  0.677
      # i 181 more rows

---

    Code
      variable_trans(init(hdi), gni_pc = trans_square_root(gni_pc))
    Output
      Index pipeline: 
      
      Steps: 
    Message
      variable_transformation: `trans_square_root()` -> gni_pc
    Output
      
      Data: 
      # A tibble: 191 x 8
            id country                  hdi  rank life_exp exp_sch avg_sch gni_pc
         <dbl> <chr>                  <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl>
       1     1 Switzerland            0.962     3     84.0    16.5    13.9   2.20
       2     2 Norway                 0.961     1     83.2    18.2    13.0   2.19
       3     3 Iceland                0.959     2     82.7    19.2    13.8   2.18
       4     4 Hong Kong, China (SAR) 0.952     4     85.5    17.3    12.2   2.19
       5     5 Australia              0.951     5     84.5    21.1    12.7   2.17
       6     6 Denmark                0.948     5     81.4    18.7    13.0   2.19
       7     7 Sweden                 0.947     9     83.0    19.4    12.6   2.18
       8     8 Ireland                0.945     8     82.0    18.9    11.6   2.21
       9     9 Germany                0.942     7     80.6    17.0    14.1   2.18
      10    10 Netherlands            0.941    10     81.7    18.7    12.6   2.18
      # i 181 more rows

---

    Code
      variable_trans(init(hdi), gni_pc = trans_cubic_root(gni_pc))
    Output
      Index pipeline: 
      
      Steps: 
    Message
      variable_transformation: `trans_cubic_root()` -> gni_pc
    Output
      
      Data: 
      # A tibble: 191 x 8
            id country                  hdi  rank life_exp exp_sch avg_sch gni_pc
         <dbl> <chr>                  <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl>
       1     1 Switzerland            0.962     3     84.0    16.5    13.9   1.69
       2     2 Norway                 0.961     1     83.2    18.2    13.0   1.69
       3     3 Iceland                0.959     2     82.7    19.2    13.8   1.68
       4     4 Hong Kong, China (SAR) 0.952     4     85.5    17.3    12.2   1.69
       5     5 Australia              0.951     5     84.5    21.1    12.7   1.67
       6     6 Denmark                0.948     5     81.4    18.7    13.0   1.68
       7     7 Sweden                 0.947     9     83.0    19.4    12.6   1.68
       8     8 Ireland                0.945     8     82.0    18.9    11.6   1.70
       9     9 Germany                0.942     7     80.6    17.0    14.1   1.68
      10    10 Netherlands            0.941    10     81.7    18.7    12.6   1.68
      # i 181 more rows

---

    Code
      variable_trans(init(hdi), gni_pc = trans_quadratic(gni_pc))
    Output
      Index pipeline: 
      
      Steps: 
    Message
      variable_transformation: `trans_quadratic()` -> gni_pc
    Output
      
      Data: 
      # A tibble: 191 x 8
            id country                  hdi  rank life_exp exp_sch avg_sch gni_pc
         <dbl> <chr>                  <dbl> <dbl>    <dbl>   <dbl>   <dbl>  <dbl>
       1     1 Switzerland            0.962     3     84.0    16.5    13.9   23.3
       2     2 Norway                 0.961     1     83.2    18.2    13.0   23.1
       3     3 Iceland                0.959     2     82.7    19.2    13.8   22.5
       4     4 Hong Kong, China (SAR) 0.952     4     85.5    17.3    12.2   23.0
       5     5 Australia              0.951     5     84.5    21.1    12.7   22.0
       6     6 Denmark                0.948     5     81.4    18.7    13.0   22.9
       7     7 Sweden                 0.947     9     83.0    19.4    12.6   22.4
       8     8 Ireland                0.945     8     82.0    18.9    11.6   23.8
       9     9 Germany                0.942     7     80.6    17.0    14.1   22.4
      10    10 Netherlands            0.941    10     81.7    18.7    12.6   22.5
      # i 181 more rows

# on errors

    Code
      variable_trans(hdi, gni_pc = trans_log10(gni_pc))
    Condition
      Error in `check_idx_tbl()`:
      ! A index table object is required as input. Created it using `init()`.

---

    Code
      variable_trans(init(hdi), index = rescale_zscore(life_exp))
    Condition
      Error in `check_var_trans_obj()`:
      ! A variable transformation object is required as input. Create is using `trans_*()`

