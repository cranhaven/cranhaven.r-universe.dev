## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(sclr)

## -----------------------------------------------------------------------------
# One-titre fit to included simulated data
fit1 <- sclr(status ~ logHI, one_titre_data)
summary(fit1)

# Two-titre fit to included simulated data
fit2 <- sclr(status ~ logHI + logNI, two_titre_data)
summary(fit2)

## -----------------------------------------------------------------------------
# One-titre fit
preddata1 <- data.frame(logHI = seq(0, 8, length.out = 101))
pred1 <- predict(fit1, preddata1)
head(pred1[, c("logHI", "prot_l", "prot_point", "prot_u")])

# Two-titre fit
preddata2 <- data.frame(logHI = seq(0, 8, length.out = 101), logNI = 1)
pred2 <- predict(fit2, preddata2)
head(pred2[, c("logHI", "logNI", "prot_l", "prot_point", "prot_u")])

## -----------------------------------------------------------------------------
protHI1 <- get_protection_level(fit1, "logHI", lvl = 0.5)
print(protHI1)

prot_lvls2 <- data.frame(logNI = log(c(0.1, 10, 40)))
protHI2 <- get_protection_level(fit2, "logHI", prot_lvls2)
print(protHI2)

