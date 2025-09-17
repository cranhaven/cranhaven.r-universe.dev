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
#  library(alpaca)
#  stat <- feglm(
#    LFP ~ KID1 + KID2 + KID3 + log(INCH) | ID + TIME,
#    data   = psid,
#    family = binomial("probit")
#    )
#  summary(stat)

## ---- eval = FALSE------------------------------------------------------------
#  ## binomial - probit link
#  ##
#  ## LFP ~ KID1 + KID2 + KID3 + log(INCH) | ID + TIME
#  ##
#  ## Estimates:
#  ##            Estimate Std. error z value Pr(> |z|)
#  ## KID1      -0.676898   0.056301 -12.023   < 2e-16 ***
#  ## KID2      -0.344370   0.049897  -6.902  5.14e-12 ***
#  ## KID3      -0.007045   0.035344  -0.199     0.842
#  ## log(INCH) -0.234128   0.054403  -4.304  1.68e-05 ***
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#  ##
#  ## residual deviance= 6069.65,
#  ## null deviance= 8152.05,
#  ## n= 5976, l= [664, 9]
#  ##
#  ## ( 7173 observation(s) deleted due to perfect classification )
#  ##
#  ## Number of Fisher Scoring Iterations: 7

## ---- eval = FALSE------------------------------------------------------------
#  apes.stat <- getAPEs(stat)
#  summary(apes.stat)

## ---- eval = FALSE------------------------------------------------------------
#  ## Estimates:
#  ##             Estimate Std. error z value Pr(> |z|)
#  ## KID1      -0.0880155  0.0077937 -11.293   < 2e-16 ***
#  ## KID2      -0.0447776  0.0068196  -6.566  5.17e-11 ***
#  ## KID3      -0.0009161  0.0050062  -0.183     0.855
#  ## log(INCH) -0.0304432  0.0077090  -3.949  7.85e-05 ***
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## ---- eval = FALSE------------------------------------------------------------
#  stat.bc <- biasCorr(stat)
#  summary(stat.bc)

## ---- eval = FALSE------------------------------------------------------------
#  ## binomial - probit link
#  ##
#  ## LFP ~ KID1 + KID2 + KID3 + log(INCH) | ID + TIME
#  ##
#  ## Estimates:
#  ##            Estimate Std. error z value Pr(> |z|)
#  ## KID1      -0.596285   0.055528 -10.738   < 2e-16 ***
#  ## KID2      -0.303346   0.049517  -6.126     9e-10 ***
#  ## KID3      -0.006117   0.035211  -0.174  0.862081
#  ## log(INCH) -0.207061   0.053928  -3.840  0.000123 ***
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#  ##
#  ## residual deviance= 6069.65,
#  ## null deviance= 8152.05,
#  ## n= 5976, l= [664, 9]
#  ##
#  ## ( 7173 observation(s) deleted due to perfect classification )
#  ##
#  ## Number of Fisher Scoring Iterations: 7

## ---- eval = FALSE------------------------------------------------------------
#  apes.stat.bc <- getAPEs(stat.bc)
#  summary(apes.stat.bc)

## ---- eval = FALSE------------------------------------------------------------
#  ## Estimates:
#  ##            Estimate Std. error z value Pr(> |z|)
#  ## KID1      -0.096501   0.007620 -12.664   < 2e-16 ***
#  ## KID2      -0.049093   0.006766  -7.255  4.01e-13 ***
#  ## KID3      -0.000990   0.004987  -0.198     0.843
#  ## log(INCH) -0.033510   0.007588  -4.416  1.00e-05 ***
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## ---- eval = FALSE------------------------------------------------------------
#  library(data.table)
#  setDT(psid)
#  psid[, LLFP := shift(LFP), by = ID]

## ---- eval = FALSE------------------------------------------------------------
#  dyn <- feglm(
#    LFP ~ LLFP + KID1 + KID2 + KID3 + log(INCH) | ID + TIME,
#    data   = psid,
#    family = binomial("probit")
#    )
#  dyn.bc <- biasCorr(dyn, L = 1L)
#  summary(dyn.bc)

## ---- eval = FALSE------------------------------------------------------------
#  ## binomial - probit link
#  ##
#  ## LFP ~ LLFP + KID1 + KID2 + KID3 + log(INCH) | ID + TIME
#  ##
#  ## Estimates:
#  ##           Estimate Std. error z value Pr(> |z|)
#  ## LLFP       1.01607    0.04759  21.350   < 2e-16 ***
#  ## KID1      -0.45387    0.06811  -6.664  2.67e-11 ***
#  ## KID2      -0.15736    0.06116  -2.573   0.01008 *
#  ## KID3       0.01561    0.04406   0.354   0.72315
#  ## log(INCH) -0.18833    0.06231  -3.023   0.00251 **
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#  ##
#  ## residual deviance= 4777.58,
#  ## null deviance= 6549.14,
#  ## n= 4792, l= [599, 8]
#  ##
#  ## ( 1461 observation(s) deleted due to missingness )
#  ## ( 6896 observation(s) deleted due to perfect classification )
#  ##
#  ## Number of Fisher Scoring Iterations: 6

## ---- eval = FALSE------------------------------------------------------------
#  apes.dyn.bc <- getAPEs(dyn.bc)
#  summary(apes.dyn.bc)

## ---- eval = FALSE------------------------------------------------------------
#  ## Estimates:
#  ##            Estimate Std. error z value Pr(> |z|)
#  ## LLFP       0.186310   0.006686  27.864   < 2e-16 ***
#  ## KID1      -0.072321   0.007832  -9.235   < 2e-16 ***
#  ## KID2      -0.025074   0.007003  -3.580  0.000343 ***
#  ## KID3       0.002487   0.005008   0.497  0.619447
#  ## log(INCH) -0.030009   0.007002  -4.286  1.82e-05 ***
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

