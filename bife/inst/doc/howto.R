## ---- eval = FALSE------------------------------------------------------------
#  data(psid, package = "bife")
#  head(psid)

## ---- eval = FALSE------------------------------------------------------------
#  ##    ID LFP KID1 KID2 KID3     INCH AGE TIME
#  ## 1:  1   1    1    1    1 58807.81  26    1
#  ## 2:  1   1    1    0    2 41741.87  27    2
#  ## 3:  1   1    0    1    2 51320.73  28    3
#  ## 4:  1   1    0    1    2 48958.58  29    4
#  ## 5:  1   1    0    1    2 53634.62  30    5
#  ## 6:  1   1    0    0    3 50983.13  31    6

## ---- eval = FALSE------------------------------------------------------------
#  library(bife)
#  stat <- bife(
#    LFP ~ KID1 + KID2 + KID3 + log(INCH) + AGE + I(AGE^2) | ID,
#    data  = psid,
#    model = "probit"
#    )
#  summary(stat)

## ---- eval = FALSE------------------------------------------------------------
#  ## binomial - probit link
#  ##
#  ## LFP ~ KID1 + KID2 + KID3 + log(INCH) + AGE + I(AGE^2) | ID
#  ##
#  ## Estimates:
#  ##             Estimate Std. error z value Pr(> |z|)
#  ## KID1      -0.7144667  0.0562414 -12.704   < 2e-16 ***
#  ## KID2      -0.4114554  0.0515524  -7.981  1.45e-15 ***
#  ## KID3      -0.1298776  0.0415477  -3.126   0.00177 **
#  ## log(INCH) -0.2417657  0.0541720  -4.463  8.08e-06 ***
#  ## AGE        0.2319724  0.0375351   6.180  6.40e-10 ***
#  ## I(AGE^2)  -0.0028846  0.0004989  -5.781  7.41e-09 ***
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#  ##
#  ## residual deviance= 6058.88,
#  ## null deviance= 8152.05,
#  ## n= 5976, N= 664
#  ##
#  ## ( 7173 observation(s) deleted due to perfect classification )
#  ##
#  ## Number of Fisher Scoring Iterations: 6
#  ##
#  ## Average individual fixed effect= -1.121

## ---- eval = FALSE------------------------------------------------------------
#  apes_stat <- get_APEs(stat)
#  summary(apes_stat)

## ---- eval = FALSE------------------------------------------------------------
#  ## Estimates:
#  ##             Estimate Std. error z value Pr(> |z|)
#  ## KID1      -9.278e-02  7.728e-03 -12.006   < 2e-16 ***
#  ## KID2      -5.343e-02  7.116e-03  -7.508  5.99e-14 ***
#  ## KID3      -1.687e-02  5.995e-03  -2.813    0.0049 **
#  ## log(INCH) -3.140e-02  7.479e-03  -4.198  2.69e-05 ***
#  ## AGE        3.012e-02  5.258e-03   5.729  1.01e-08 ***
#  ## I(AGE^2)  -3.746e-04  7.015e-05  -5.340  9.29e-08 ***
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## ---- eval = FALSE------------------------------------------------------------
#  stat_bc <- bias_corr(stat)
#  summary(stat_bc)

## ---- eval = FALSE------------------------------------------------------------
#  ## binomial - probit link
#  ##
#  ## LFP ~ KID1 + KID2 + KID3 + log(INCH) + AGE + I(AGE^2) | ID
#  ##
#  ## Estimates:
#  ##             Estimate Std. error z value Pr(> |z|)
#  ## KID1      -0.6308839  0.0555073 -11.366   < 2e-16 ***
#  ## KID2      -0.3635269  0.0511325  -7.110  1.16e-12 ***
#  ## KID3      -0.1149869  0.0413488  -2.781   0.00542 **
#  ## log(INCH) -0.2139549  0.0536613  -3.987  6.69e-05 ***
#  ## AGE        0.2052708  0.0373054   5.502  3.75e-08 ***
#  ## I(AGE^2)  -0.0025520  0.0004962  -5.143  2.70e-07 ***
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#  ##
#  ## residual deviance= 6058.88,
#  ## null deviance= 8152.05,
#  ## n= 5976, N= 664
#  ##
#  ## ( 7173 observation(s) deleted due to perfect classification )
#  ##
#  ## Number of Fisher Scoring Iterations: 6
#  ##
#  ## Average individual fixed effect= -0.969

## ---- eval = FALSE------------------------------------------------------------
#  apes_stat_bc <- get_APEs(stat_bc)
#  summary(apes_stat_bc)

## ---- eval = FALSE------------------------------------------------------------
#  ## Estimates:
#  ##             Estimate Std. error z value Pr(> |z|)
#  ## KID1      -1.016e-01  7.582e-03 -13.394   < 2e-16 ***
#  ## KID2      -5.852e-02  7.057e-03  -8.292   < 2e-16 ***
#  ## KID3      -1.851e-02  5.951e-03  -3.110   0.00187 **
#  ## log(INCH) -3.444e-02  7.376e-03  -4.669  3.03e-06 ***
#  ## AGE        3.304e-02  5.235e-03   6.312  2.76e-10 ***
#  ## I(AGE^2)  -4.108e-04  6.986e-05  -5.880  4.10e-09 ***
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## ---- eval = FALSE------------------------------------------------------------
#  library(data.table)
#  setDT(psid)
#  setkey(psid, ID, TIME)
#  psid[, LLFP := shift(LFP), by = ID]

## ---- eval = FALSE------------------------------------------------------------
#  dyn <- bife(
#    LFP ~ LLFP + KID1 + KID2 + KID3 + log(INCH) + AGE + I(AGE^2) | ID,
#    data  = psid,
#    model = "probit"
#    )
#  dyn_bc <- bias_corr(dyn, L = 1L)
#  summary(dyn_bc)

## ---- eval = FALSE------------------------------------------------------------
#  ## binomial - probit link
#  ##
#  ## LFP ~ LLFP + KID1 + KID2 + KID3 + log(INCH) + AGE + I(AGE^2) |
#  ##     ID
#  ##
#  ## Estimates:
#  ##             Estimate Std. error z value Pr(> |z|)
#  ## LLFP       1.0025625  0.0473066  21.193   < 2e-16 ***
#  ## KID1      -0.4741275  0.0679073  -6.982  2.91e-12 ***
#  ## KID2      -0.1958365  0.0625921  -3.129  0.001755 **
#  ## KID3      -0.0754042  0.0505110  -1.493  0.135482
#  ## log(INCH) -0.1946970  0.0621143  -3.134  0.001722 **
#  ## AGE        0.2009569  0.0477728   4.207  2.59e-05 ***
#  ## I(AGE^2)  -0.0024142  0.0006293  -3.836  0.000125 ***
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#  ##
#  ## residual deviance= 4774.57,
#  ## null deviance= 6549.14,
#  ## n= 4792, N= 599
#  ##
#  ## ( 1461 observation(s) deleted due to missingness )
#  ## ( 6896 observation(s) deleted due to perfect classification )
#  ##
#  ## Number of Fisher Scoring Iterations: 6
#  ##
#  ## Average individual fixed effect= -1.939

## ---- eval = FALSE------------------------------------------------------------
#  apes_dyn_bc <- get_APEs(dyn_bc)
#  summary(apes_dyn_bc)

## ---- eval = FALSE------------------------------------------------------------
#  ## Estimates:
#  ##             Estimate Std. error z value Pr(> |z|)
#  ## LLFP       1.826e-01  6.671e-03  27.378   < 2e-16 ***
#  ## KID1      -7.525e-02  7.768e-03  -9.687   < 2e-16 ***
#  ## KID2      -3.108e-02  7.239e-03  -4.294  1.76e-05 ***
#  ## KID3      -1.197e-02  5.886e-03  -2.033     0.042 *
#  ## log(INCH) -3.090e-02  6.992e-03  -4.419  9.91e-06 ***
#  ## AGE        3.189e-02  5.403e-03   5.903  3.57e-09 ***
#  ## I(AGE^2)  -3.832e-04  7.107e-05  -5.391  7.00e-08 ***
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

