## ---- eval = FALSE------------------------------------------------------------
#  # Required packages
#  library(alpaca)
#  library(data.table)
#  library(haven)
#  
#  # Import the data set
#  cudata <- read_dta("dataaxj1.dta")
#  setDT(cudata)
#  
#  # Subset relevant variables
#  var.nms <- c(
#    "exp1to2", "custrict11", "ldist", "comlang", "border", "regional",
#    "comcol", "curcol", "colony", "comctry", "cuwoemu", "emu", "cuc",
#    "cty1", "cty2", "year", "pairid"
#    )
#  cudata <- cudata[, var.nms, with = FALSE]
#  
#  # Generate identifiers required for structural gravity
#  cudata[, pairid := factor(pairid)]
#  cudata[, exp.time := interaction(cty1, year)]
#  cudata[, imp.time := interaction(cty2, year)]
#  
#  # Generate dummies for disaggregated currency unions
#  cudata[, cuau := as.numeric(cuc == "au")]
#  cudata[, cube := as.numeric(cuc == "be")]
#  cudata[, cuca := as.numeric(cuc == "ca")]
#  cudata[, cucf := as.numeric(cuc == "cf")]
#  cudata[, cucp := as.numeric(cuc == "cp")]
#  cudata[, cudk := as.numeric(cuc == "dk")]
#  cudata[, cuea := as.numeric(cuc == "ea")]
#  cudata[, cuec := as.numeric(cuc == "ec")]
#  cudata[, cuem := as.numeric(cuc == "em")]
#  cudata[, cufr := as.numeric(cuc == "fr")]
#  cudata[, cugb := as.numeric(cuc == "gb")]
#  cudata[, cuin := as.numeric(cuc == "in")]
#  cudata[, cuma := as.numeric(cuc == "ma")]
#  cudata[, cuml := as.numeric(cuc == "ml")]
#  cudata[, cunc := as.numeric(cuc == "nc")]
#  cudata[, cunz := as.numeric(cuc == "nz")]
#  cudata[, cupk := as.numeric(cuc == "pk")]
#  cudata[, cupt := as.numeric(cuc == "pt")]
#  cudata[, cusa := as.numeric(cuc == "sa")]
#  cudata[, cusp := as.numeric(cuc == "sp")]
#  cudata[, cuua := as.numeric(cuc == "ua")]
#  cudata[, cuus := as.numeric(cuc == "us")]
#  cudata[, cuwa := as.numeric(cuc == "wa")]
#  cudata[, cuwoo := custrict11]
#  cudata[cuc %in% c("em", "au", "cf", "ec", "fr", "gb", "in", "us"), cuwoo := 0]
#  
#  # Set missing trade flows to zero
#  cudata[is.na(exp1to2), exp1to2 := 0]
#  
#  # Re-scale trade flows
#  cudata[, exp1to2 := exp1to2 / 1000]
#  
#  # Construct binary and lagged dependent variable for the extensive margin
#  cudata[, trade := as.numeric(exp1to2 > 0)]
#  cudata[, ltrade := shift(trade), by = pairid]

## ---- eval = FALSE------------------------------------------------------------
#  mod <- feglm(
#    exp1to2 ~ emu + cuwoo + cuau + cucf + cuec + cufr + cugb + cuin + cuus +
#      regional + curcol | exp.time + imp.time + pairid | cty1 + cty2 + year,
#    data   = cudata,
#    family = poisson()
#    )
#  summary(mod, "sandwich")

## ---- eval = FALSE------------------------------------------------------------
#  ## poisson - log link
#  ##
#  ## exp1to2 ~ emu + cuwoo + cuau + cucf + cuec + cufr + cugb + cuin +
#  ##     cuus + regional + curcol | exp.time + imp.time + pairid |
#  ##     cty1 + cty2 + year
#  ##
#  ## Estimates:
#  ##           Estimate Std. error z value Pr(> |z|)
#  ## emu       0.048895   0.010277   4.758  1.96e-06 ***
#  ## cuwoo     0.765988   0.053272  14.379   < 2e-16 ***
#  ## cuau      0.384469   0.118832   3.235   0.00121 **
#  ## cucf     -0.125608   0.099674  -1.260   0.20760
#  ## cuec     -0.877318   0.083451 -10.513   < 2e-16 ***
#  ## cufr      2.095726   0.062952  33.291   < 2e-16 ***
#  ## cugb      1.059957   0.034680  30.564   < 2e-16 ***
#  ## cuin      0.169745   0.147029   1.154   0.24830
#  ## cuus      0.018323   0.021530   0.851   0.39473
#  ## regional  0.159181   0.008714  18.267   < 2e-16 ***
#  ## curcol    0.386882   0.046827   8.262   < 2e-16 ***
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#  ##
#  ## residual deviance= 35830.78,
#  ## null deviance= 2245707.30,
#  ## n= 1610165, l= [11227, 11277, 34104]
#  ##
#  ## ( 1363003 observation(s) deleted due to perfect classification )
#  ##
#  ## Number of Fisher Scoring Iterations: 20

## ---- eval = FALSE------------------------------------------------------------
#  summary(mod, type = "clustered", cluster = ~ cty1 + cty2 + year)

## ---- eval = FALSE------------------------------------------------------------
#  ## poisson - log link
#  ##
#  ## exp1to2 ~ emu + cuwoo + cuau + cucf + cuec + cufr + cugb + cuin +
#  ##     cuus + regional + curcol | exp.time + imp.time + pairid |
#  ##     cty1 + cty2 + year
#  ##
#  ## Estimates:
#  ##          Estimate Std. error z value Pr(> |z|)
#  ## emu       0.04890    0.09455   0.517   0.60507
#  ## cuwoo     0.76599    0.24933   3.072   0.00213 **
#  ## cuau      0.38447    0.22355   1.720   0.08546 .
#  ## cucf     -0.12561    0.35221  -0.357   0.72137
#  ## cuec     -0.87732    0.29493  -2.975   0.00293 **
#  ## cufr      2.09573    0.30625   6.843  7.75e-12 ***
#  ## cugb      1.05996    0.23766   4.460  8.19e-06 ***
#  ## cuin      0.16974    0.30090   0.564   0.57267
#  ## cuus      0.01832    0.05092   0.360   0.71898
#  ## regional  0.15918    0.07588   2.098   0.03593 *
#  ## curcol    0.38688    0.15509   2.495   0.01261 *
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#  ##
#  ## residual deviance= 35830.78,
#  ## null deviance= 2245707.30,
#  ## n= 1610165, l= [11227, 11277, 34104]
#  ##
#  ## ( 1363003 observation(s) deleted due to perfect classification )
#  ##
#  ## Number of Fisher Scoring Iterations: 20

## ---- eval = FALSE------------------------------------------------------------
#  library(car)
#  h0_cus <- c("cuwoo", "cuau", "cucf", "cuec", "cufr", "cugb", "cuin", "cuus")
#  linearHypothesis(
#    mod, h0_cus,
#    vcov. = vcov(mod, "clustered", cluster = ~ cty1 + cty2 + year)
#    )

## ---- eval = FALSE------------------------------------------------------------
#  ## Linear hypothesis test
#  ##
#  ## Hypothesis:
#  ## cuwoo = 0
#  ## cuau = 0
#  ## cucf = 0
#  ## cuec = 0
#  ## cufr = 0
#  ## cugb = 0
#  ## cuin = 0
#  ## cuus = 0
#  ##
#  ## Model 1: restricted model
#  ## Model 2: exp1to2 ~ emu + cuwoo + cuau + cucf + cuec + cufr + cugb + cuin +
#  ##     cuus + regional + curcol | exp.time + imp.time + pairid |
#  ##     cty1 + cty2 + year
#  ##
#  ## Note: Coefficient covariance matrix supplied.
#  ##
#  ##   Df  Chisq Pr(>Chisq)
#  ## 1
#  ## 2  8 96.772  < 2.2e-16 ***
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## ---- eval = FALSE------------------------------------------------------------
#  mods <- feglm(
#    trade ~ cuwoemu + emu + regional | exp.time + imp.time + pairid,
#    data   = cudata,
#    family = binomial("logit")
#    )
#  summary(mods)

## ---- eval = FALSE------------------------------------------------------------
#  ## binomial - logit link
#  ##
#  ## trade ~ cuwoemu + emu + regional | exp.time + imp.time + pairid
#  ##
#  ## Estimates:
#  ##          Estimate Std. error z value Pr(> |z|)
#  ## cuwoemu   0.41037    0.05103   8.041  8.89e-16 ***
#  ## emu       0.55108    0.23080   2.388   0.01696 *
#  ## regional  0.10193    0.03271   3.116   0.00183 **
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#  ##
#  ## residual deviance= 674579.11,
#  ## null deviance= 1917254.65,
#  ## n= 1384892, l= [11150, 11218, 29391]
#  ##
#  ## ( 1588276 observation(s) deleted due to perfect classification )
#  ##
#  ## Number of Fisher Scoring Iterations: 11

## ---- eval = FALSE------------------------------------------------------------
#  modsbc <- biasCorr(mods, panel.structure = "network")
#  summary(modsbc)

## ---- eval = FALSE------------------------------------------------------------
#  ## binomial - logit link
#  ##
#  ## trade ~ cuwoemu + emu + regional | exp.time + imp.time + pairid
#  ##
#  ## Estimates:
#  ##          Estimate Std. error z value Pr(> |z|)
#  ## cuwoemu   0.37630    0.05107   7.369  1.72e-13 ***
#  ## emu       0.47213    0.23056   2.048   0.04058 *
#  ## regional  0.09942    0.03270   3.040   0.00236 **
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#  ##
#  ## residual deviance= 674579.11,
#  ## null deviance= 1917254.65,
#  ## n= 1384892, l= [11150, 11218, 29391]
#  ##
#  ## ( 1588276 observation(s) deleted due to perfect classification )
#  ##
#  ## Number of Fisher Scoring Iterations: 11

## ---- eval = FALSE------------------------------------------------------------
#  apesbc <- getAPEs(modsbc)
#  summary(apesbc)

## ---- eval = FALSE------------------------------------------------------------
#  ## Estimates:
#  ##          Estimate Std. error z value Pr(> |z|)
#  ## cuwoemu  0.015554   0.001957   7.946  1.92e-15 ***
#  ## emu      0.019567   0.008212   2.383  0.017185 *
#  ## regional 0.004079   0.001189   3.429  0.000605 ***
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## ---- eval = FALSE------------------------------------------------------------
#  modd <- feglm(
#    trade ~ ltrade + cuwoemu + emu + regional | exp.time + imp.time + pairid,
#    data   = cudata,
#    family = binomial("logit")
#    )
#  moddbc <- biasCorr(modd, L = 4, panel.structure = "network")
#  summary(moddbc)

## ---- eval = FALSE------------------------------------------------------------
#  ## binomial - logit link
#  ##
#  ## trade ~ ltrade + cuwoemu + emu + regional | exp.time + imp.time +
#  ##     pairid
#  ##
#  ## Estimates:
#  ##          Estimate Std. error z value Pr(> |z|)
#  ## ltrade   2.166103   0.008003 270.663   < 2e-16 ***
#  ## cuwoemu  0.268477   0.054482   4.928  8.31e-07 ***
#  ## emu      0.312551   0.254091   1.230    0.2187
#  ## regional 0.063077   0.035328   1.785    0.0742 .
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#  ##
#  ## residual deviance= 602773.13,
#  ## null deviance= 1899840.83,
#  ## n= 1372471, l= [11054, 11116, 29299]
#  ##
#  ## ( 45048 observation(s) deleted due to missingness )
#  ## ( 1555649 observation(s) deleted due to perfect classification )
#  ##
#  ## Number of Fisher Scoring Iterations: 11

## ---- eval = FALSE------------------------------------------------------------
#  apedbc <- getAPEs(moddbc)
#  summary(apedbc)

## ---- eval = FALSE------------------------------------------------------------
#  ## Estimates:
#  ##           Estimate Std. error z value Pr(> |z|)
#  ## ltrade   0.1213377  0.0004681 259.231   < 2e-16 ***
#  ## cuwoemu  0.0100223  0.0016486   6.079  1.21e-09 ***
#  ## emu      0.0116944  0.0078456   1.491    0.1361
#  ## regional 0.0023338  0.0010544   2.213    0.0269 *
#  ## ---
#  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

