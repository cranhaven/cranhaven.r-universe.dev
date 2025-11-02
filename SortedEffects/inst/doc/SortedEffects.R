## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  data("mortgage")

## -----------------------------------------------------------------------------
fm <- deny ~ black + p_irat + hse_inc + ccred + mcred + pubrec + ltv_med + ltv_high + denpmi + selfemp + single + hischl

## ---- eval=FALSE--------------------------------------------------------------
#  test <- spe(fm = fm, data = mortgage, var = "black", method = "logit",
#              us = c(2:98)/100, b = 200, bc = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  plot(x = test, ylim = c(0, 0.25), ylab = "Change in Probability",
#       main = "APE and SPE of Being Black on the prob of Mortgage Denial",
#       sub = "Logit Model")

## ---- eval=FALSE--------------------------------------------------------------
#  t <- c("deny", "p_irat", "black", "hse_inc", "ccred", "mcred", "pubrec",
#         "denpmi", "selfemp", "single", "hischl", "ltv_med", "ltv_high")

## ---- eval=FALSE--------------------------------------------------------------
#  CA <- ca(fm = fm, data = mortgage, var = "black", method = "logit",
#           cl = "both", t = t, b = 200, bc = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  summary(CA)

## ---- eval=FALSE--------------------------------------------------------------
#  CAdiff <- ca(fm = fm, data = mortgage, var = "black", t = t,
#               method = "logit", cl = "diff", b = 200, bc = TRUE)
#  # Tabulate the results
#  summary(CAdiff)

## ---- eval=FALSE--------------------------------------------------------------
#  data(wage2015)

## ---- eval=FALSE--------------------------------------------------------------
#  <<eval=FALSE>>=
#  fmla1 <- lnw ~ female* (widowed + divorced + separated + nevermarried +
#                          exp1 + exp2 + exp3 + exp4 + educ + occ2 + ind2 +
#                          mw + so + we)

## ---- eval=FALSE--------------------------------------------------------------
#  set <- subpop(fm = fmla0, data = wage2015, var = "female",
#                samp_weight = wage2015$weight, boot_type = "weighted",
#                b = 500, subgroup = wage2015[,"female"]==1, u = 0.05)
#  
#  plot(set, varx = wage2015$exp1, vary = wage2015$lnw,
#       main = "Projections of Exp-lnw", sub = "OLS", xlab = "Exp",
#       ylab = "Log Wages")
#  
#  plot(set, varx = wage2015$exp1, vary =wage2015$ms,
#       main = "Projections of Exp-MS", sub = "OLS", xlab = "Exp",
#       ylab = "Marital Status")

