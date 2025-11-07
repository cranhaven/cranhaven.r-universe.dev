source(file.path(system.file(package = "gppm"), "demo_helpers.R"))

## settings
nT <- 100 # number of time points

## define autoregressive model using SEM software
arModel <- generateAR(nT)
trueModel <- omxSetParameters(arModel, labels = c("b0", "b1", "sigma"), values = c(10, 0.5, 1))

## simulate data
yVector <- simulateData(trueModel)
tVector <- 1:nT

## fit data using SEM
semModel <- mxModel(arModel, mxData(yVector, type = "raw"))
semModel <- mxRun(semModel)


# get results using GPPM
myData <- data.frame(ID = rep(1, nT), t = tVector, y = as.numeric(yVector))
gpModel <- gppm("b0/(1-b1)", "b1^(fabs(t-t#))*sigma/(1-b1^2)", myData, ID = "ID", DV = "y")
gpModel <- fit(gpModel)

## check results
arSame <- all.equal(coef(gpModel), omxGetParameters(semModel)[names(coef(gpModel))], check.attributes = FALSE, tolerance = 0.0001)
message(sprintf("Estimated parameters for the AR(1) model are the same: %s", arSame))
