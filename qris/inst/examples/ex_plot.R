data(cancer, package = "survival")
lung2 <- subset(lung, select = c(time, status, age, sex))
## tidy up the data
lung2$status <- lung2$status - 1
lung2$sex <- lung2$sex - 1

library(qris)
fm <- Surv(time, status) ~ age + sex
fit <- qris(fm, data = lung2, t0 = 30, Q = 0.5, nB = 50, "nonsmooth", "fmb")

## Plot with default values; Qs <- 1:9 / 10 and t0s = fit2$para$t0 (in this case 30)
plot(fit)

## Plot with without 95% CI is much faster
plot(fit, nB = 0)

## Plot feature can update qris calls 
fit <- plot(fit, Qs = 3:6 / 10, t0s = 1:6 * 10)

## Faster after updating the qris call
plot(fit, byQs = FALSE)
plot(fit, byQs = TRUE)

plot(fit, byQs = FALSE, vari = c("sex", "age"))
plot(fit, byQs = TRUE, vari = c("sex", "age"))

## Extra ggplot components
library(ggplot2)
plot(fit, byQs = FALSE, vari = c("sex", "age"), ggextra = theme(legend.position = "none"))
