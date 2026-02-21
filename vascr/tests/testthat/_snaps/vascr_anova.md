# Can make a significance table

    Code
      vascr_make_significance_table(growth.df, 50, "R", 4000, 0.95)
    Output
      # A tibble: 8 x 2
        Sample                       Label                                            
        <chr>                        <chr>                                            
      1 0_cells x HCMEC D3_line      "15,000_cells x HCMEC D3_line **\n20,000_cells x~
      2 10,000_cells x HCMEC D3_line "25,000_cells x HCMEC D3_line **\n30,000_cells x~
      3 15,000_cells x HCMEC D3_line "0_cells x HCMEC D3_line **\n30,000_cells x HCME~
      4 20,000_cells x HCMEC D3_line "0_cells x HCMEC D3_line ***\n30,000_cells x HCM~
      5 25,000_cells x HCMEC D3_line "0_cells x HCMEC D3_line ****\n10,000_cells x HC~
      6 30,000_cells x HCMEC D3_line "0_cells x HCMEC D3_line ****\n10,000_cells x HC~
      7 35,000_cells x HCMEC D3_line "0_cells x HCMEC D3_line ****\n10,000_cells x HC~
      8 5,000_cells x HCMEC D3_line  "20,000_cells x HCMEC D3_line *\n25,000_cells x ~

---

    Code
      vascr_make_significance_table(growth.df, 50, "R", 4000, 0.95, format = "Tukey_data")
    Output
      # A tibble: 31 x 9
         term       group1       group2 null.value estimate conf.low conf.high   p.adj
       * <chr>      <chr>        <chr>       <dbl>    <dbl>    <dbl>     <dbl>   <dbl>
       1 Experiment 1 : Experim~ 2 : E~          0    -24.8    -79.3      29.8 4.79e-1
       2 Experiment 1 : Experim~ 3 : E~          0    -91.8   -146.      -37.3 1.61e-3
       3 Experiment 2 : Experim~ 3 : E~          0    -67.1   -122.      -12.6 1.59e-2
       4 Sample     0_cells x H~ 10,00~          0    109.     -11.1     229.  8.84e-2
       5 Sample     0_cells x H~ 15,00~          0    158.      37.9     278.  6.78e-3
       6 Sample     0_cells x H~ 20,00~          0    202.      82.2     322.  7   e-4
       7 Sample     0_cells x H~ 25,00~          0    268.     148.      388.  3.4 e-5
       8 Sample     0_cells x H~ 30,00~          0    344.     224.      464.  1.76e-6
       9 Sample     0_cells x H~ 35,00~          0    424.     304.      544.  1.25e-7
      10 Sample     0_cells x H~ 5,000~          0     54.9    -65.1     175.  7.35e-1
      # i 21 more rows
      # i 1 more variable: p.adj.signif <chr>

# Vascr LM

    Code
      vascr_lm(growth.df, "R", 4000, 100)
    Output
      
      Call:
      lm(formula = formula, data = data.df)
      
      Coefficients:
                             (Intercept)           Experiment2 : Experiment2  
                                  302.97                              -81.22  
               Experiment3 : Experiment3  Sample10,000_cells + HCMEC D3_line  
                                 -123.63                              318.87  
      Sample15,000_cells + HCMEC D3_line  Sample20,000_cells + HCMEC D3_line  
                                  366.04                              365.43  
      Sample25,000_cells + HCMEC D3_line  Sample30,000_cells + HCMEC D3_line  
                                  357.85                              349.05  
      Sample35,000_cells + HCMEC D3_line   Sample5,000_cells + HCMEC D3_line  
                                  320.43                              140.83  
      

# Vascr_residuals

    Code
      vascr_residuals(growth.df, "R", "4000", 100)
    Output
                1           2           3           4           5           6 
      -71.2600254  15.1556080  56.1044174  43.5198942   0.2053120 -43.7252062 
                7           8           9          10          11          12 
       41.2513697 -11.8259343 -29.4254354  29.3639323 -31.1078063   1.7438740 
               13          14          15          16          17          18 
       17.5040807 -16.5831750  -0.9209057   3.8446770  -4.2829199   0.4382429 
               19          20          21          22          23          24 
      -21.3740220  -1.3609699  22.7349919 -42.8499065  49.7998853  -6.9499789 

# Vascr Shapiro test checks

    Code
      vascr_shapiro(growth.df, "R", 4000, 100)
    Output
      
      	Shapiro-Wilk normality test
      
      data:  aov_residuals
      W = 0.97874, p-value = 0.8716
      

# Levene test

    Code
      vascr_levene(growth.df, "R", 4000, 100)
    Output
      # A tibble: 1 x 4
          df1   df2 statistic     p
        <int> <int>     <dbl> <dbl>
      1     7    16     0.484 0.832

# Tukey tests

    Code
      vascr_tukey(growth.df, "R", 4000, 100)
    Output
      # A tibble: 31 x 9
         term       group1       group2 null.value estimate conf.low conf.high   p.adj
         <chr>      <chr>        <chr>       <dbl>    <dbl>    <dbl>     <dbl>   <dbl>
       1 Experiment 1 : Experim~ 2 : E~          0    -81.2   -134.      -28.5 3.33e-3
       2 Experiment 1 : Experim~ 3 : E~          0   -124.    -176.      -70.9 7.24e-5
       3 Experiment 2 : Experim~ 3 : E~          0    -42.4    -95.2      10.4 1.25e-1
       4 Sample     0_cells x H~ 10,00~          0    319.     203.      435.  2.99e-6
       5 Sample     0_cells x H~ 15,00~          0    366.     250.      482.  5.41e-7
       6 Sample     0_cells x H~ 20,00~          0    365.     249.      482.  5.53e-7
       7 Sample     0_cells x H~ 25,00~          0    358.     242.      474.  7.19e-7
       8 Sample     0_cells x H~ 30,00~          0    349.     233.      465.  9.8 e-7
       9 Sample     0_cells x H~ 35,00~          0    320.     204.      437.  2.81e-6
      10 Sample     0_cells x H~ 5,000~          0    141.      24.7     257.  1.3 e-2
      # i 21 more rows
      # i 1 more variable: p.adj.signif <chr>

---

    Code
      vascr_tukey(growth.df, "R", 4000, 100, raw = TRUE)
    Output
      # A tibble: 31 x 9
         term       group1       group2 null.value estimate conf.low conf.high   p.adj
       * <chr>      <chr>        <chr>       <dbl>    <dbl>    <dbl>     <dbl>   <dbl>
       1 Experiment 1 : Experim~ 2 : E~          0    -81.2   -134.      -28.5 3.33e-3
       2 Experiment 1 : Experim~ 3 : E~          0   -124.    -176.      -70.9 7.24e-5
       3 Experiment 2 : Experim~ 3 : E~          0    -42.4    -95.2      10.4 1.25e-1
       4 Sample     0_cells x H~ 10,00~          0    319.     203.      435.  2.99e-6
       5 Sample     0_cells x H~ 15,00~          0    366.     250.      482.  5.41e-7
       6 Sample     0_cells x H~ 20,00~          0    365.     249.      482.  5.53e-7
       7 Sample     0_cells x H~ 25,00~          0    358.     242.      474.  7.19e-7
       8 Sample     0_cells x H~ 30,00~          0    349.     233.      465.  9.8 e-7
       9 Sample     0_cells x H~ 35,00~          0    320.     204.      437.  2.81e-6
      10 Sample     0_cells x H~ 5,000~          0    141.      24.7     257.  1.3 e-2
      # i 21 more rows
      # i 1 more variable: p.adj.signif <chr>

# Dunnett test works

    Code
      vascr_dunnett(growth.df, "R", 4000, 50, 8)
    Output
      # A tibble: 8 x 20
         Time Unit  Frequency Sample  Instrument     sd totaln     n   min   max Well 
        <dbl> <fct>     <dbl> <chr>   <chr>       <dbl>  <int> <int> <dbl> <dbl> <chr>
      1    50 R          4000 0_cell~ ECIS         2.65      9     3  237.  242. H01,~
      2    50 R          4000 10,000~ ECIS        27.5       9     3  321.  376. F01,~
      3    50 R          4000 15,000~ ECIS        42.4       9     3  350.  432. E01,~
      4    50 R          4000 20,000~ ECIS        52.5       9     3  394.  498. D01,~
      5    50 R          4000 25,000~ ECIS        81.0       9     3  428.  590. C01,~
      6    50 R          4000 30,000~ ECIS       108.        9     3  464.  675. B01,~
      7    50 R          4000 35,000~ ECIS        79.0       9     3  574.  721. A01,~
      8    50 R          4000 5,000_~ ECIS        20.4       9     3  281.  318. G01,~
      # i 9 more variables: Value <dbl>, Experiment <chr>, sem <dbl>, Excluded <chr>,
      #   SampleID <dbl>, P <dbl>, padj <dbl>, Label <chr>, P_round <dbl>

---

    Code
      vascr_dunnett(growth.df, "R", 4000, list(50, 100), 8)
    Output
      # A tibble: 16 x 20
          Time Unit  Frequency Sample Instrument     sd totaln     n   min   max Well 
         <dbl> <fct>     <dbl> <chr>  <chr>       <dbl>  <int> <int> <dbl> <dbl> <chr>
       1    50 R          4000 0_cel~ ECIS         2.65      9     3  237.  242. H01,~
       2    50 R          4000 10,00~ ECIS        27.5       9     3  321.  376. F01,~
       3    50 R          4000 15,00~ ECIS        42.4       9     3  350.  432. E01,~
       4    50 R          4000 20,00~ ECIS        52.5       9     3  394.  498. D01,~
       5    50 R          4000 25,00~ ECIS        81.0       9     3  428.  590. C01,~
       6    50 R          4000 30,00~ ECIS       108.        9     3  464.  675. B01,~
       7    50 R          4000 35,00~ ECIS        79.0       9     3  574.  721. A01,~
       8    50 R          4000 5,000~ ECIS        20.4       9     3  281.  318. G01,~
       9   100 R          4000 0_cel~ ECIS         2.68      9     3  232.  237. H01,~
      10   100 R          4000 10,00~ ECIS       106.        9     3  454.  665. F01,~
      11   100 R          4000 15,00~ ECIS        99.5       9     3  516.  710. E01,~
      12   100 R          4000 20,00~ ECIS        84.7       9     3  547.  698. D01,~
      13   100 R          4000 25,00~ ECIS        75.5       9     3  536.  678. C01,~
      14   100 R          4000 30,00~ ECIS        65.2       9     3  529.  656. B01,~
      15   100 R          4000 35,00~ ECIS        41.6       9     3  523.  602. A01,~
      16   100 R          4000 5,000~ ECIS        54.3       9     3  313.  412. G01,~
      # i 9 more variables: Value <dbl>, Experiment <chr>, sem <dbl>, Excluded <chr>,
      #   SampleID <dbl>, P <dbl>, padj <dbl>, Label <chr>, P_round <dbl>

---

    Code
      vascr_dunnett(growth.df, "R", 4000, 50, "0_cells + HCMEC D3_line")
    Output
      # A tibble: 8 x 20
         Time Unit  Frequency Sample  Instrument     sd totaln     n   min   max Well 
        <dbl> <fct>     <dbl> <chr>   <chr>       <dbl>  <int> <int> <dbl> <dbl> <chr>
      1    50 R          4000 0_cell~ ECIS         2.65      9     3  237.  242. H01,~
      2    50 R          4000 10,000~ ECIS        27.5       9     3  321.  376. F01,~
      3    50 R          4000 15,000~ ECIS        42.4       9     3  350.  432. E01,~
      4    50 R          4000 20,000~ ECIS        52.5       9     3  394.  498. D01,~
      5    50 R          4000 25,000~ ECIS        81.0       9     3  428.  590. C01,~
      6    50 R          4000 30,000~ ECIS       108.        9     3  464.  675. B01,~
      7    50 R          4000 35,000~ ECIS        79.0       9     3  574.  721. A01,~
      8    50 R          4000 5,000_~ ECIS        20.4       9     3  281.  318. G01,~
      # i 9 more variables: Value <dbl>, Experiment <chr>, sem <dbl>, Excluded <chr>,
      #   SampleID <dbl>, P <chr>, padj <dbl>, Label <chr>, P_round <chr>

