## ------------------------------------------------------------------------------------------
## Library and data
## ------------------------------------------------------------------------------------------
library(tranSurv)

data(channing, package = "boot")
chan <- subset(channing, entry < exit)

## ------------------------------------------------------------------------------------------

trReg(Surv(entry, exit, cens) ~ sex, data = chan)
trReg(Surv(entry, exit, cens) ~ sex, data = chan, method = "adjust", control = list(G = 10))

(fit <- trReg(Surv(entry, exit, cens) ~ 1, data = chan))
plot(fit)

(fit <- with(chan, trSurvfit(entry, exit, cens)))
plot(fit)

gof(with(chan, Surv(entry, exit, cens)), B = 10)

(fit0 <- with(chan, trSurvfit(entry, exit, cens)))
gof(fit0, B = 20)

(fit <- trReg(Surv(entry, exit, cens) ~ sex, data = chan, B = 10))
gof(fit, B = 20)

(fit <- trReg(Surv(entry, exit, cens) ~ sex, data = chan, B = 10))
(fit <- trReg(Surv(entry, exit, cens) ~ sex, data = chan, B = 10, control = list(P = 2)))
(fit <- trReg(Surv(entry, exit, cens) ~ sex, data = chan, B = 10, control = list(P = 3)))

(fit <- trReg(Surv(entry, exit, cens) ~ sex, data = chan, B = 10, method = "adjust"))
(fit <- trReg(Surv(entry, exit, cens) ~ sex, data = chan, B = 10, method = "adjust", control = list(P = 1)))
(fit <- trReg(Surv(entry, exit, cens) ~ sex, data = chan, B = 10, method = "adjust", control = list(P = 2)))

## errored because of tiny intervals
## (fit <- trReg(Surv(entry, exit, cens) ~ sex, data = chan, B = 10, method = "adjust", control = list(P = 3)))

(fit <- trReg(Surv(entry, exit, cens) ~ sex, data = chan, B = 10, method = "adjust", control = list(Q = 1)))
(fit <- trReg(Surv(entry, exit, cens) ~ sex, data = chan, B = 10, method = "adjust", control = list(Q = 2)))

names(fit)
fit$PEta


(trReg(Surv(entry, exit, cens) ~ sex, data = chan, B = 10, method = "adjust", control = list(Q = 2, a = -0)))
(trReg(Surv(entry, exit, cens) ~ sex, data = chan, B = 10, method = "adjust", control = list(Q = 2, a = -0.7977801)))
