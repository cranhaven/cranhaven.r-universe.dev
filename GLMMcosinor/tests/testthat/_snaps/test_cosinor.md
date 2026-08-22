# multi-component comparison works, print functions work

    Code
      print(object, digits = 2)
    Output
      
       Conditional Model 
      
       Raw formula: 
      Y ~ group + group:main_rrr1 + group:main_sss1 + group:main_rrr2 +      group:main_sss2 
      
       Raw Coefficients: 
                       Estimate
      (Intercept)           1.0
      group1               -0.5
      group0:main_rrr1     -2.0
      group1:main_rrr1      0.9
      group0:main_sss1      0.3
      group1:main_sss1      0.3
      group0:main_rrr2     -2.1
      group1:main_rrr2      1.0
      group0:main_sss2      0.2
      group1:main_sss2      0.3
      
       Transformed Coefficients: 
                     Estimate
      (Intercept)         1.0
      [group=1]          -0.5
      [group=0]:amp1      2.0
      [group=1]:amp1      0.9
      [group=0]:amp2      2.1
      [group=1]:amp2      1.1
      [group=0]:acr1      3.0
      [group=1]:acr1      0.3
      [group=0]:acr2      3.0
      [group=1]:acr2      0.3

