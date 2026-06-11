library(spef)

data(blaTum)
## Fit the bladder tumor data set
fm <- PanelSurv(id, time, count) ~ num + size + treatment

panelReg(fm, data = blaTum, method = "AEE", se = "Sandwich")
panelReg(fm, data = blaTum, method = "AEEX", se = "Impute",
         control = list(a = 0.1, R = 30))
panelReg(fm, data = blaTum, method = "HWZ", se = "Bootstrap",
         control = list(R = 30))
panelReg(fm, data = blaTum, method = "MLs", se = "NULL")
panelReg(fm, data = blaTum, method = "EE.SWa", se = "Bootstrap",
         control = list(R = 30))
panelReg(fm, data = blaTum, method = "EE.HSWc", se = "Bootstrap",
         control = list(R = 30))
