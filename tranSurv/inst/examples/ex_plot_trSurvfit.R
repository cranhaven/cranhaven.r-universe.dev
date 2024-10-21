data(channing, package = "boot")
chan <- subset(channing, entry < exit)

plot(trReg(Surv(entry, exit, cens) ~ 1, data = chan))
