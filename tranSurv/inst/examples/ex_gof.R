data(channing, package = "boot")
chan <- subset(channing, entry < exit)
fit <- trReg(Surv(entry, exit, cens) ~ sex, data = chan)
gof(fit, B = 10)
