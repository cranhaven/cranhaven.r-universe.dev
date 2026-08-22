# summary print works

    Code
      print(print_obj, digits = 2)
    Output
      
       Conditional Model 
      Raw model coefficients:
                   estimate standard.error lower.CI upper.CI p.value    
      (Intercept)     30.33           0.38    29.58     31.1  <2e-16 ***
      X0:main_rrr1     0.87           0.63    -0.38      2.1     0.2    
      X1:main_rrr1     6.48           0.94     4.63      8.3   7e-12 ***
      X0:main_sss1     6.24           0.68     4.91      7.6  <2e-16 ***
      X1:main_sss1     4.67           0.91     2.89      6.4   3e-07 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Transformed coefficients:
                  estimate standard.error lower.CI upper.CI p.value    
      (Intercept)    30.33           0.38    29.58     31.1  <2e-16 ***
      [X=0]:amp1      6.30           0.68     4.97      7.6  <2e-16 ***
      [X=1]:amp1      7.98           0.91     6.20      9.8  <2e-16 ***
      [X=0]:acr1      1.43           0.10     1.24      1.6  <2e-16 ***
      [X=1]:acr1      0.62           0.12     0.39      0.9   1e-07 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

    Code
      print(print_obj, digits = 1)
    Output
      
       Conditional Model 
      Raw model coefficients:
                   estimate standard.error lower.CI upper.CI p.value    
      (Intercept)      30.3            0.4     29.6       31  <2e-16 ***
      X0:main_rrr1      1.0            0.6     -0.3        2     0.1    
      X1:main_rrr1      6.6            1.0      4.7        8   2e-11 ***
      X0:main_sss1      6.3            0.7      4.9        8  <2e-16 ***
      X1:main_sss1      4.6            0.9      2.7        6   1e-06 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Transformed coefficients:
                  estimate standard.error lower.CI upper.CI p.value    
      (Intercept)     30.3            0.4     29.6     31.1  <2e-16 ***
      [X=0]:amp1       6.3            0.7      5.0      7.7  <2e-16 ***
      [X=1]:amp1       8.0            0.9      6.2      9.8  <2e-16 ***
      [X=0]:acr1       1.4            0.1      1.2      1.6  <2e-16 ***
      [X=1]:acr1       0.6            0.1      0.4      0.8   8e-07 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      ***********************
      
       Dispersion Model 
      Raw model coefficients:
                   estimate standard.error lower.CI upper.CI p.value    
      (Intercept)      1.66           0.05     1.56      1.8  <2e-16 ***
      X0:disp_rrr1     0.19           0.09     0.01      0.4    0.04 *  
      X1:disp_rrr1    -0.04           0.12    -0.28      0.2    0.74    
      X0:disp_sss1     0.05           0.10    -0.14      0.2    0.60    
      X1:disp_sss1     0.03           0.12    -0.20      0.3    0.81    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Transformed coefficients:
                  estimate standard.error lower.CI upper.CI p.value    
      (Intercept)     1.66           0.05     1.56      1.8  <2e-16 ***
      [X=0]:amp1      0.20           0.09     0.03      0.4    0.02 *  
      [X=1]:amp1      0.05           0.12    -0.19      0.3    0.68    
      [X=0]:acr1      0.27           0.52    -0.75      1.3    0.61    
      [X=1]:acr1      2.54           2.38    -2.12      7.2    0.29    
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      ***********************
      
       Zero-Inflation Model 
      Raw model coefficients:
                  estimate standard.error lower.CI upper.CI p.value
      (Intercept)   -2e+01          5e+03   -1e+04     9742       1
      X0:zi_rrr1    -2e+00          7e+03   -1e+04    13508       1
      X1:zi_rrr1     9e-02          1e+04   -2e+04    21286       1
      X0:zi_sss1     7e-01          7e+03   -1e+04    13636       1
      X1:zi_sss1     3e-01          1e+04   -2e+04    20486       1
      
      Transformed coefficients:
                  estimate standard.error lower.CI upper.CI p.value
      (Intercept)   -2e+01          5e+03   -1e+04     9742       1
      [X=0]:amp1     2e+00          7e+03   -1e+04    13939       1
      [X=1]:amp1     3e-01          1e+04   -2e+04    20198       1
      [X=0]:acr1     3e+00          4e+03   -8e+03     7893       1
      [X=1]:acr1     1e+00          4e+04   -8e+04    77144       1

