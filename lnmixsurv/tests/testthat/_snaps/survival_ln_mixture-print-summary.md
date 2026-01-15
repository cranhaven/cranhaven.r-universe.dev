# print method works

    Code
      mod
    Output
      survival_ln_mixture
       formula: survival::Surv(y, delta) ~ x
       observations: 10000
       predictors: 2
       mixture groups: 2
      ------------------
                     estimate   std.error  cred.low cred.high
      (Intercept)_1 4.0446935 0.006659593 4.0352590 4.0533201
      x1_1          0.8062953 0.010040927 0.7941995 0.8195355
      (Intercept)_2 3.4235683 0.020073543 3.3897345 3.4476133
      x1_2          0.4865902 0.021463598 0.4579802 0.5162284
      
      Auxiliary parameter(s):
              estimate  std.error   cred.low  cred.high
      phi_1 26.2490951 1.47347833 23.9429643 28.2141026
      phi_2  3.1936586 0.11375225  3.0533295  3.3741595
      eta_1  0.5089456 0.01261907  0.4950967  0.5308884

