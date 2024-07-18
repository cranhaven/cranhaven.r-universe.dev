## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setupMorse, eval=FALSE---------------------------------------------------
#  install.packages(morse) # install the package
#  library(morse) # load the package

## ----testJAGS, eval=FALSE-----------------------------------------------------
#  install.packages("runjags")
#  library("runjags")
#  testjags()

## ----pathJAGS, eval=FALSE-----------------------------------------------------
#  testjags(jags=runjags.getOption('jagspath')) # replace jagspath by the PATH to JAGS
#  # FOR INSTANCE, on the current machine I do:
#  testjags(jags=runjags.getOption('C:/Program Files/JAGS/JAGS-4.3.0/x64/bin/jags-terminal.exe'))

## ----fitSurv, eval=FALSE------------------------------------------------------
#  library(morse)
#  data("propiconazole")
#  survData_PRZ = survData(propiconazole)
#  fit_cstSD = survFit(survData_PRZ, model_type = "SD")

## ----plotSurv, eval=FALSE-----------------------------------------------------
#  library(morse)
#  data("propiconazole")
#  survData_PRZ = survData(propiconazole)
#  fit_cstSD = survFit(survData_PRZ, model_type = "SD")
#  plot(fit_cstSD)

## ----LCxSurv, eval=FALSE------------------------------------------------------
#  library(morse)
#  data("propiconazole")
#  survData_PRZ = survData(propiconazole)
#  fit_cstSD = survFit(survData_PRZ, model_type = "SD")
#  LCx(fit_cstSD, X = 50)

## ----plotLCxSurv, eval=FALSE--------------------------------------------------
#  library(morse)
#  data("propiconazole")
#  survData_PRZ = survData(propiconazole)
#  fit_cstSD = survFit(survData_PRZ, model_type = "SD")
#  LCX_cstSD = LCx(fit_cstSD, X = 50)
#  plot(LCX_cstSD)

## ----MFx, eval=FALSE----------------------------------------------------------
#  library(morse)
#  data("propiconazole")
#  survData_PRZ = survData(propiconazole)
#  fit_cstSD = survFit(survData_PRZ, model_type = "SD")
#  data_4MFx <- data.frame(time = 1:10,
#                          conc = c(0,0.5,8,3,0,0,0.5,8,3.5,0))
#  MFx_PRZ_cstSD <- MFx(object = fit_cstSD, data_predict = data_4MFx, ode = TRUE)

## ----plotMFx, eval=FALSE------------------------------------------------------
#  library(morse)
#  data("propiconazole")
#  survData_PRZ = survData(propiconazole)
#  fit_cstSD = survFit(survData_PRZ, model_type = "SD")
#  data_4MFx <- data.frame(time = 1:10,
#                          conc = c(0,0.5,8,3,0,0,0.5,8,3.5,0))
#  MFx_PRZ_cstSD <- MFx(object = fit_cstSD, data_predict = data_4MFx, ode = TRUE)
#  plot(MFx_PRZ_cstSD)

## ----predictSurv, eval=FALSE--------------------------------------------------
#  library(morse)
#  data("propiconazole")
#  survData_PRZ = survData(propiconazole)
#  fit_cstSD = survFit(survData_PRZ, model_type = "SD")
#  data_example <- data.frame(
#     time = c(1,1.9,2,15,15.1,20),
#     conc = c(0,0,20,20,0,0),
#     replicate = rep("example", 6)
#  )
#  predict_example_NULL = predict_ode(
#    object = fit_cstSD,
#    data_predict = data_example,
#    mcmc_size = 10,
#    interpolate_length = NULL)

## ----plot1predictSurv, eval=FALSE---------------------------------------------
#  plot(predict_example_NULL)

## ----plot2predictSurv, eval=FALSE---------------------------------------------
#  predict_example_100 = predict_ode(
#    object = fit_cstSD,
#    data_predict = data_example,
#    mcmc_size = 10,
#    interpolate_length = 100)
#  plot(predict_example_100)

## ----plotpredictSurv_FOCUS, eval=FALSE----------------------------------------
#  data("FOCUSprofile")
#  predict_FOCUS = predict_ode(
#    object = fit_cstSD,
#    data_predict = FOCUSprofile,
#    mcmc_size = 10,
#    interpolate_length = NULL)
#  plot(predict_FOCUS)

## ----predictNsurv, eval=FALSE-------------------------------------------------
#  library(morse)
#  data("propiconazole")
#  survData_PRZ = survData(propiconazole)
#  fit_cstSD = survFit(survData_PRZ, model_type = "SD")
#  data("propiconazole_pulse_exposure")
#  predict_Nsurv = predict_Nsurv_ode(
#    object = fit_cstSD,
#    data_predict = propiconazole_pulse_exposure
#  )

## ----plotPredictNsurv, eval=FALSE---------------------------------------------
#  library(morse)
#  data("propiconazole")
#  survData_PRZ = survData(propiconazole)
#  fit_cstSD = survFit(survData_PRZ, model_type = "SD")
#  data("propiconazole_pulse_exposure")
#  predict_Nsurv = predict_Nsurv_ode(
#    object = fit_cstSD,
#    data_predict = propiconazole_pulse_exposure
#  )
#  plot(predict_Nsurv)

