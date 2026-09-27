# print.summary.glmELE output matches exact snapshot formatting

    Code
      print(s_fit)
    Output
      
      Call:
      plglm(formula = y ~ x1 + x2, family = "gaussian", adjustment = adj, 
          data = data.frame(y = dat$y, dat$X))
      
      Family: gaussian
      
      --- Weighting Method: BLUE ---
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept)   1.4855     0.1490   9.968 3.55e-13 ***
      x1            0.8593     0.1030   8.343 7.88e-11 ***
      x2           -0.5486     0.2047  -2.680   0.0101 *  
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Dispersion parameter for gaussian family: 0.2262
      

