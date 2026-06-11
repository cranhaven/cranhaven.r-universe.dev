data(blaTum)
## Plot the fit of bladder tumor data set
fm <- PanelSurv(id, time, count) ~ num + size + treatment
fit1 <- panelReg(fm, data=blaTum, method = "AEE", se = "Sandwich")
plot(fit1)

fit2 <- panelReg(fm, data=blaTum, method = "MLs", se = "NULL")
plot(fit2)
