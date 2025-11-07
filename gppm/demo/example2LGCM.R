source(file.path(system.file(package = "gppm"), "demo_helpers.R"))
## settings
nP <- 500
nT <- 10

## define latent growth curve model using SEM software
lgcModel <- generateLGCM(nT)
trueModel <- omxSetParameters(lgcModel, labels = c("muI", "muS", "varI", "varS", "covIS", "sigma"), values = c(10, 3, 4, 10, 0.5, 10))


## simulate data
yMatrix <- simulateData(trueModel, N = nP)
tMatrix <- matrix(rep(0:(nT - 1), each = nP), nrow = nP, ncol = nT)
colnames(tMatrix) <- paste0("t", 1:nT)
myData <- as.data.frame(cbind(tMatrix, yMatrix))

# fit data using SEM
semModel <- mxModel(lgcModel, mxData(yMatrix, type = "raw"))
semModel <- mxRun(semModel, silent = TRUE)




## fit data using GPPMNew
longData <- convertFromWide(myData)
gpModel <- gppm("muI+muS*t", "varI+covIS*(t+t#)+varS*t*t#+(t==t#)*sigma", longData, ID = "ID", DV = "Y")
fittedModel <- fit(gpModel)



## compare results
lgcmSame <- all.equal(coef(fittedModel), omxGetParameters(semModel)[names(coef(fittedModel))], check.attributes = FALSE, tolerance = 0.0001)
message(sprintf("Estimated parameters for the LGCM model are the same: %s", lgcmSame))
