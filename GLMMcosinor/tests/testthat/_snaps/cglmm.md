# model returns accurate parameters

    Code
      f_round(object$coefficients)
    Output
      [1]  1.0030 -0.4966  2.0122  0.9948  3.0115  0.3175

---

    Code
      f_round(object$coefficients)
    Output
      [1]  1.0089 -0.5004  1.9969  1.0107  3.0064  0.2923

---

    Code
      f_round(object$coefficients)
    Output
      [1]  1.0029 -0.5142  2.0043  0.9982  2.9990  0.2893

---

    Code
      f_round(object$coefficients)
    Output
      [1]  1.0115 -0.5103  2.0101  1.0149  2.9986  0.2806

---

    Code
      f_round(object$coefficients)
    Output
      [1]  0.9411 -0.4453  1.9462  1.0356  3.0154  0.3083

# model output is class cglmm

    Code
      print(object, digits = 2)
    Output
      
       Conditional Model 
      
       Raw formula: 
      vit_d ~ X + X:main_rrr1 + X:main_sss1 
      
       Raw Coefficients: 
                   Estimate
      (Intercept)      29.7
      X1                1.9
      X0:main_rrr1      0.8
      X1:main_rrr1      6.5
      X0:main_sss1      6.1
      X1:main_sss1      4.8
      
       Transformed Coefficients: 
                  Estimate
      (Intercept)     29.7
      [X=1]            1.9
      [X=0]:amp        6.2
      [X=1]:amp        8.1
      [X=0]:acr        1.4
      [X=1]:acr        0.6
      
      ***********************
      
       Dispersion Model 
      
       Raw  Formula: 
      ~X:disp_rrr1 + X:disp_sss1 - 1 
      
       Raw  Coefficients: 
                   Estimate
      X0:disp_rrr1      0.3
      X1:disp_rrr1     -0.1
      X0:disp_sss1      0.0
      X1:disp_sss1      0.0
      
       Transformed  Coefficients: 
                Estimate
      [X=0]:amp      0.3
      [X=1]:amp      0.1
      [X=0]:acr      0.0
      [X=1]:acr     -2.6
      
      ***********************
      
       Zero-Inflation Model 
      
       Raw  Formula: 
      ~X:zi_rrr1 + X:zi_sss1 - 1 
      
       Raw  Coefficients: 
                 Estimate
      X0:zi_rrr1     -0.2
      X1:zi_rrr1      0.1
      X0:zi_sss1      0.1
      X1:zi_sss1      0.2
      
       Transformed  Coefficients: 
                Estimate
      [X=0]:amp      0.2
      [X=1]:amp      0.2
      [X=0]:acr      2.6
      [X=1]:acr      1.3

---

    Code
      print(object, digits = 2)
    Output
      
       Conditional Model 
      
       Raw formula: 
      Y ~ group + group:main_rrr1 + group:main_sss1 + group:main_rrr2 +      group:main_sss2 + (0 + main_rrr2 + main_sss2 | group) 
      
       Raw Coefficients: 
                       Estimate
      (Intercept)           5.0
      group1               -3.0
      group0:main_rrr1      0.1
      group1:main_rrr1      0.0
      group0:main_sss1      0.9
      group1:main_sss1      1.0
      group0:main_rrr2      1.1
      group1:main_rrr2      1.1
      group0:main_sss2      1.7
      group1:main_sss2      1.7
      
       Transformed Coefficients: 
                     Estimate
      (Intercept)         5.0
      [group=1]          -3.0
      [group=0]:amp1      0.9
      [group=1]:amp1      1.0
      [group=0]:amp2      2.0
      [group=1]:amp2      2.0
      [group=0]:acr1      1.5
      [group=1]:acr1      1.6
      [group=0]:acr2      1.0
      [group=1]:acr2      1.0

# specifying no amp_acro term works

    Code
      f()
    Output
      
       Conditional Model 
      
       Raw formula: 
      vit_d ~ X + X:main_rrr1 + X:main_sss1 
      
       Raw Coefficients: 
                   Estimate
      (Intercept)  29.69639
      X1            1.86497
      X0:main_rrr1  0.96645
      X1:main_rrr1  6.43236
      X0:main_sss1  6.27112
      X1:main_sss1  4.79989
      
       Transformed Coefficients: 
                  Estimate
      (Intercept) 29.69639
      [X=1]        1.86497
      [X=0]:amp    6.34516
      [X=1]:amp    8.02584
      [X=0]:acr    1.41789
      [X=1]:acr    0.64107
      
      ***********************
      
       Dispersion Model 
      
       Raw  Formula: 
      ~X 
      
       Raw  Coefficients: 
                  Estimate
      (Intercept)  1.63602
      X1           0.09579

---

    Code
      f()
    Output
      
       Conditional Model 
      
       Raw formula: 
      vit_d ~ X + X:main_rrr1 + X:main_sss1 
      
       Raw Coefficients: 
                   Estimate
      (Intercept)  29.69639
      X1            1.86497
      X0:main_rrr1  0.96645
      X1:main_rrr1  6.43236
      X0:main_sss1  6.27114
      X1:main_sss1  4.79989
      
       Transformed Coefficients: 
                  Estimate
      (Intercept) 29.69639
      [X=1]        1.86497
      [X=0]:amp    6.34517
      [X=1]:amp    8.02585
      [X=0]:acr    1.41789
      [X=1]:acr    0.64107
      
      ***********************
      
       Zero-Inflation Model 
      
       Raw  Formula: 
      NULL 
      
       Raw  Coefficients: 
                   Estimate
      (Intercept) -15.07683
      X1           -1.30260

---

    Code
      f()
    Output
      
       Conditional Model 
      
       Raw formula: 
      vit_d ~ X + X:main_rrr1 + X:main_sss1 
      
       Raw Coefficients: 
                   Estimate
      (Intercept)  29.69639
      X1            1.86498
      X0:main_rrr1  0.96645
      X1:main_rrr1  6.43236
      X0:main_sss1  6.27113
      X1:main_sss1  4.79989
      
       Transformed Coefficients: 
                  Estimate
      (Intercept) 29.69639
      [X=1]        1.86498
      [X=0]:amp    6.34517
      [X=1]:amp    8.02584
      [X=0]:acr    1.41789
      [X=1]:acr    0.64107
      
      ***********************
      
       Dispersion Model 
      
       Raw  Formula: 
      ~X 
      
       Raw  Coefficients: 
                  Estimate
      (Intercept)  1.63602
      X1           0.09579
      
      ***********************
      
       Zero-Inflation Model 
      
       Raw  Formula: 
      NULL 
      
       Raw  Coefficients: 
                   Estimate
      (Intercept) -21.51196
      X1          -13.25923

