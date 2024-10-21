data(channing, package = "boot")
chan <- subset(channing, entry < exit)
trReg(Surv(entry, exit, cens) ~ sex, data = chan)

