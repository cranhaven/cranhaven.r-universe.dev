## ----include=FALSE, echo=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.width = 7,
                      fig.height = 4,
                      cache = TRUE)

## ---- echo=FALSE, results='hide', message=FALSE-------------------------------
library(morse)
library(dplyr)

## ----step1TT, cache=TRUE------------------------------------------------------
data(cadmium2)
survDataCheck(cadmium2)

## ----step2TT, cache=TRUE------------------------------------------------------
dat <- survData(cadmium2)
head(dat)

## ----step3TT, cache=TRUE------------------------------------------------------
plot(dat, pool.replicate = FALSE)

## ---- cache=TRUE--------------------------------------------------------------
plot(dat, concentration = 124, addlegend = TRUE,
     pool.replicate = FALSE, style ="generic")

## ---- cache=TRUE--------------------------------------------------------------
plotDoseResponse(dat, target.time = 21, addlegend = TRUE)

## ---- cache=TRUE--------------------------------------------------------------
summary(dat)

## ----step4TT, results="hide", cache=TRUE--------------------------------------
fit <- survFitTT(dat,
                 target.time = 21,
                 lcx = c(10, 20, 30, 40, 50))

## ----step4TTsummary, cache=TRUE-----------------------------------------------
summary(fit)

## ----step4TTplot, cache=TRUE--------------------------------------------------
plot(fit, log.scale = TRUE, adddata = TRUE,   addlegend = TRUE)

## ---- cache=TRUE--------------------------------------------------------------
plot(fit, log.scale = TRUE, style = "generic", adddata = TRUE, addlegend = TRUE)

## ----wrongTT, results="hide", cache=TRUE--------------------------------------
data("cadmium1")
doubtful_fit <- survFitTT(survData(cadmium1),
                       target.time = 21,
                       lcx = c(10, 20, 30, 40, 50))
plot(doubtful_fit, log.scale = TRUE, style = "ggplot", adddata = TRUE,
     addlegend = TRUE)

## ----step5TT,cache=TRUE, results="hide"---------------------------------------
ppc(fit)

## ----TKTDcst, cache=TRUE, eval=FALSE------------------------------------------
#  # (1) load data set
#  data(propiconazole)
#  
#  # (2) check structure and integrity of the data set
#  survDataCheck(propiconazole)
#  
#  # (3) create a `survData` object
#  dat <- survData(propiconazole)
#  
#  # (4) represent the number of survivors as a function of time
#  plot(dat, pool.replicate = FALSE)
#  
#  # (5) check information on the experimental design
#  summary(dat)

## ----fitcstSD, cache=TRUE, echo=TRUE, eval=FALSE------------------------------
#  # (6) fit the TKTD model SD
#  fit_cstSD <- survFit(dat, quiet = TRUE, model_type = "SD")

## ---- cache=TRUE, eval=FALSE--------------------------------------------------
#  # (7) summary of parameters estimates
#  summary(fit_cstSD)
#  # OR
#  fit_cstSD$estim.par

## ----PLOTpriors_post_cstSD, cache=TRUE, eval=FALSE----------------------------
#  plot_prior_post(fit_cstSD)

## ---- cache=TRUE, eval=FALSE--------------------------------------------------
#  plot(fit_cstSD)

## ---- cache=TRUE, eval=FALSE--------------------------------------------------
#  plot(fit_cstSD, adddata = FALSE)

## ---- cache=TRUE, eval=FALSE--------------------------------------------------
#  ppc(fit_cstSD)

## ----fitcstSDFIXhb, cache=TRUE, eval=FALSE------------------------------------
#  # fit the TKTD model SD with fixed hb value
#  fit_cstSDFIXhb <- survFit(dat, quiet = TRUE, model_type = "SD", hb_value=FALSE, hb_valueFIXED = 0.2)

## ----summarycstSDFIXhbSummary, cache=TRUE, eval=FALSE-------------------------
#  summary(fit_cstSDFIXhb)

## ----summarycstSDFIXhb_PrintVal, cache=TRUE, eval=FALSE-----------------------
#  fit_cstSDFIXhb$hb

## ----fitcstIT, echo=TRUE, cache=TRUE, eval=FALSE------------------------------
#  fit_cstIT <- survFit(dat, quiet = TRUE, model_type = "IT")

## ---- cache=TRUE, eval=FALSE--------------------------------------------------
#  summary(fit_cstIT)
#  # OR
#  fit_cstIT$estim.par

## ----PLOTpriors_post_cstIT, cache=TRUE, eval=FALSE----------------------------
#  plot_prior_post(fit_cstIT)

## ---- cache=TRUE, eval=FALSE--------------------------------------------------
#  plot(fit_cstIT)

## ---- cache=TRUE, eval=FALSE--------------------------------------------------
#  ppc(fit_cstIT)

## ----TKTDvar, cache=TRUE, eval=FALSE------------------------------------------
#  # (1) load data set
#  data("propiconazole_pulse_exposure")
#  
#  # (2) check structure and integrity of the data set
#  survDataCheck(propiconazole_pulse_exposure)
#  
#  # (3) create a `survData` object
#  dat <- survData(propiconazole_pulse_exposure)
#  
#  # (4) represent the number of survivor as a function of time
#  plot(dat)
#  
#  # (5) check information on the experimental design
#  summary(dat)

## ----fitvarSD, echo=TRUE, cache=TRUE, eval=FALSE------------------------------
#  # (6) fit the TKTD model SD
#  fit_varSD <- survFit(dat, quiet = TRUE, model_type = "SD")

## ---- cache=TRUE, eval=FALSE--------------------------------------------------
#  # (7) summary of the fit object
#  summary(fit_varSD)

## ----PLOTpriors_post_varSD, cache=TRUE, eval=FALSE----------------------------
#  plot_prior_post(fit_varSD)

## ---- cache=TRUE, eval=FALSE--------------------------------------------------
#  plot(fit_varSD)

## ---- cache=TRUE, eval=FALSE--------------------------------------------------
#  ppc(fit_varSD)

## ----fitvarIT, echo=TRUE, cache=TRUE, eval=FALSE------------------------------
#  # fit a TKTD model IT
#  fit_varIT <- survFit(dat, quiet = TRUE, model_type = "IT")

## ---- cache=TRUE, eval=FALSE--------------------------------------------------
#  # (7) summary of the fit object
#  summary(fit_varIT)

## ----PLOTpriors_post_varIT, cache=TRUE, eval=FALSE----------------------------
#  plot_prior_post(fit_varIT)

## ---- cache=TRUE, eval=FALSE--------------------------------------------------
#  plot(fit_varIT)

## ---- cache=TRUE, eval=FALSE--------------------------------------------------
#  ppc(fit_varIT)

## ----predict, cache=TRUE, eval=FALSE------------------------------------------
#  # (1) upload or build a data frame with the exposure profile
#  # argument `replicate` is used to provide several profiles of exposure
#  data_4prediction <- data.frame(time = c(1:10, 1:10),
#                                 conc = c(c(0,0,40,0,0,0,40,0,0,0),
#                                          c(21,19,18,23,20,14,25,8,13,5)),
#                                 replicate = c(rep("pulse", 10), rep("random", 10)))
#  
#  # (2) Use the fit on constant exposure propiconazole with model SD (see previously)
#  predict_PRZ_cstSD_4pred <- predict(object = fit_cstSD, data_predict = data_4prediction)

## ----predictPlot, cache=TRUE, eval=FALSE--------------------------------------
#  # (3) Plot the predicted survival rate under the new exposure profiles.
#  plot(predict_PRZ_cstSD_4pred)

## ----predict_ode, cache=TRUE, eval=FALSE--------------------------------------
#  predict_PRZ_cstSD_4pred_ode <- predict_ode(object = fit_cstSD, data_predict = data_4prediction)

## ----predict_ode_plot, cache=TRUE, eval=FALSE---------------------------------
#  plot(predict_PRZ_cstSD_4pred_ode)

## ----hb_value, cache=TRUE, eval=FALSE-----------------------------------------
#  # Use the same data set profile to predict without 'hb'
#  predict_PRZ_cstSD_4pred_hbOUT <- predict_ode(object = fit_cstSD, data_predict = data_4prediction, hb_value = FALSE, hb_valueFORCED = 0)
#  # Plot the prediction:
#  plot(predict_PRZ_cstSD_4pred_hbOUT)

## ----hb_valueFIX2, cache=TRUE, eval=FALSE-------------------------------------
#  # Use the same data set profile to predict without 'hb'
#  predict_PRZ_cstSD_4pred_hbFIX2 <- predict_ode(object = fit_cstSD, data_predict = data_4prediction,
#                                                hb_value = FALSE, hb_valueFORCED = 0.2)
#  # Plot the prediction:
#  plot(predict_PRZ_cstSD_4pred_hbFIX2)

## ----cstTOcst, cache=TRUE, eval=FALSE-----------------------------------------
#  predict_Nsurv_PRZ_SD_cstTOcst <- predict_Nsurv(fit_cstSD, propiconazole)

## ----varTOcst, cache=TRUE, eval=FALSE-----------------------------------------
#  predict_Nsurv_PRZ_SD_varTOcst <- predict_Nsurv(fit_varSD, propiconazole)

## ----cstTOvar, cache=TRUE, eval=FALSE-----------------------------------------
#  predict_Nsurv_PRZ_SD_cstTOvar <- predict_Nsurv(fit_cstSD, propiconazole_pulse_exposure)

## ----varTOvar, cache=TRUE, eval=FALSE-----------------------------------------
#  predict_Nsurv_PRZ_SD_varTOvar <- predict_Nsurv(fit_varSD, propiconazole_pulse_exposure)

## ----cstTOcstPredict_ODE, cache=TRUE, eval=FALSE------------------------------
#  predict_Nsurv_PRZ_SD_cstTOcst_ode <- predict_Nsurv_ode(fit_cstSD, propiconazole)

## ----cstTOcstPredict_ODE_PLOT, fig.align='center', out.width='.49\\linewidth',fig.show='hold', eval=FALSE----
#  plot(predict_Nsurv_PRZ_SD_cstTOcst)
#  plot(predict_Nsurv_PRZ_SD_cstTOcst_ode)

## ----cstTOcstPredict_ODE_PLOT1, eval=FALSE------------------------------------
#  plot(predict_Nsurv_PRZ_SD_cstTOcst_ode)

## ----checkNsurvPRED_1, cache=TRUE, eval=FALSE---------------------------------
#  predict_Nsurv_check(predict_Nsurv_PRZ_SD_cstTOvar)

## ----plotPredict_Nsurv, cache=TRUE, eval=FALSE--------------------------------
#  plot(predict_Nsurv_PRZ_SD_cstTOvar)

## ----ppcPredict_Nsurv, cache=TRUE, eval=FALSE---------------------------------
#  ppc(predict_Nsurv_PRZ_SD_cstTOvar)

## ----nameEFSA_SD, cache=TRUE, eval=FALSE--------------------------------------
#  summary(fit_cstSD, EFSA_name = TRUE)
#  head(priors_distribution(fit_cstSD, EFSA_name = TRUE))
#  plot_prior_post(fit_cstSD, EFSA_name = TRUE)

## ----nameEFSA_IT, cache=TRUE, eval=FALSE--------------------------------------
#  summary(fit_cstIT, EFSA_name = TRUE)
#  head(priors_distribution(fit_cstIT, EFSA_name = TRUE))
#  plot_prior_post(fit_cstIT, EFSA_name = TRUE)

## ----cstSDLCx, cache=TRUE, eval=FALSE-----------------------------------------
#  # LC50 at the maximum time-point:
#  LCx_cstSD <- LCx(fit_cstSD, X = 50)
#  plot(LCx_cstSD)
#  
#  # LC50 at time = 2
#  LCx(fit_cstSD, X = 50, time_LCx = 2) %>% plot()
#  ## Note the use of the pipe operator, `%>%`, which is a powerful tool for clearly expressing a sequence of multiple operations.
#  ## For more information on pipes, see: http://r4ds.had.co.nz/pipes.html

## ----cstSDLCx_3015, eval=FALSE------------------------------------------------
#  # LC50 at time = 15
#  LCx(fit_cstSD, X = 50, time_LCx = 15) %>% plot()

## ----cstITLCx, eval=FALSE-----------------------------------------------------
#  # LC50 at the maximum time-point:
#  LCx_cstIT <- LCx(fit_cstIT, X = 50)
#  plot(LCx_cstIT)
#  
#  # LC50 at time = 2
#  LCx(fit_cstIT, X = 50, time_LCx = 2) %>% plot()
#  
#  # LC30 at time = 15
#  LCx(fit_cstIT, X = 30, time_LCx = 15) %>% plot()

## ----varSDLCx, eval=FALSE-----------------------------------------------------
#  # LC50 at time = 4
#  LCx_varSD <- LCx(fit_varSD, X = 50, time_LCx = 4, conc_range = c(0,100))
#  plot(LCx_varSD)
#  
#  # LC50 at time = 30
#  LCx(fit_varSD, X = 50, time_LCx = 30,  conc_range = c(0,100)) %>% plot()

## ----varITLCx, eval=FALSE-----------------------------------------------------
#  # LC50 at time = 4
#  LCx(fit_varIT, X = 50, time_LCx = 4, conc_range = c(0,200)) %>% plot()
#  
#  # LC50 at time = 30
#  LCx(fit_varIT, X = 50, time_LCx = 30, conc_range = c(0,100)) %>% plot()

## ----MFx_compt, cache=TRUE, eval=FALSE----------------------------------------
#  # (1) upload or build a data frame with the exposure profile
#  data_4MFx <- data.frame(time = 1:10,
#                          conc = c(0,0.5,8,3,0,0,0.5,8,3.5,0))
#  
#  # (2) Use the fit on constant exposure propiconazole with model SD (see previously)
#  MFx_PRZ_cstSD_4MFx <- MFx(object = fit_cstSD, data_predict = data_4MFx, ode = TRUE)

## ----MFx_plot, cache=TRUE, eval=FALSE-----------------------------------------
#  # (3) Plot the survival rate as function of the multiplication factors.
#  plot(MFx_PRZ_cstSD_4MFx)

## ----MFx_plotLog, cache=TRUE, eval=FALSE--------------------------------------
#  # (3 bis) Plot the survival rate as function of the multiplication factors in log-scale.
#  plot(MFx_PRZ_cstSD_4MFx, log_scale = TRUE)

## ----MFx_plotTime, cache=TRUE, eval=FALSE-------------------------------------
#  # (4) Plot the survival rate versus time. Control (MFx = 1) and estimated MFx.
#  plot(MFx_PRZ_cstSD_4MFx, x_variable =  "Time")

## ---- cache=TRUE, eval=FALSE--------------------------------------------------
#  MFx_PRZ_cstSD_4MFx$df_MFx

## ----MFx_x10, cache=TRUE, eval=FALSE------------------------------------------
#  # (2 bis) fit on constant exposure propiconazole with model SD (see previously)
#  MFx_PRZ_cstSD_4MFx_x10 <- MFx(object = fit_cstSD, data_predict = data_4MFx, X = 10)

## ----MFx_x10_plot, cache=TRUE, eval=FALSE-------------------------------------
#  # Plot with log scale
#  plot(MFx_PRZ_cstSD_4MFx_x10, log_scale = TRUE)

## ----MFx_x100_threshold, cache=TRUE, eval=FALSE-------------------------------
#  # (2 ter) fit on constant exposure propiconazole with model SD (see previously)
#  MFx_PRZ_cstSD_4MFx_x10_thresh20 <- MFx(object = fit_cstSD, data_predict = data_4MFx, X = 10, threshold_iter = 20)
#  plot(MFx_PRZ_cstSD_4MFx_x10_thresh20, log_scale = TRUE)

## ----MFx_IT, cache=TRUE, eval=FALSE-------------------------------------------
#  # (2) Use the fit on constant exposure propiconazole with model IT. No print of run messages.
#  MFx_PRZ_cstIT_4pred <- MFx(object = fit_cstIT, data_predict = data_4MFx, time_MFx = 4, quiet = TRUE)
#  
#  # (3) Plot the survival rate versus multiplication factors.
#  plot(MFx_PRZ_cstIT_4pred, log_scale = TRUE)

## ----MFx_ITplot, cache=TRUE, eval=FALSE---------------------------------------
#  # (2) Use the fit on constant exposure propiconazole with model IT. No print of run messages.
#  MFx_PRZ_cstIT_4pred <- MFx(object = fit_cstIT, X=10, hb_value = FALSE, data_predict = data_4MFx, time_MFx = 4, quiet = TRUE)
#  
#  plot(MFx_PRZ_cstIT_4pred, log_scale = TRUE)
#  
#  plot(MFx_PRZ_cstIT_4pred, x_variable =  "Time")

## ----MFx_range, cache=TRUE, eval=FALSE----------------------------------------
#  # Use the fit on constant exposure propiconazole with model SD.
#  MFx_PRZ_cstSD_4pred_range <- MFx(object = fit_cstSD, data_predict = data_4MFx, X = NULL, MFx_range = 1:6)

## ----plt_MFx_range, cache=TRUE, eval=FALSE------------------------------------
#  # Plot survival rate versus the range of multiplication factor.
#  plot(MFx_PRZ_cstSD_4pred_range)

## ----plt_MFx_range_Time, cache=TRUE, eval=FALSE-------------------------------
#  # Plot Survival rate as function of time.
#  plot(MFx_PRZ_cstSD_4pred_range, x_variable = "Time")

## ---- cache=TRUE, eval=FALSE--------------------------------------------------
#  # Plot a specific time series.
#  plot(MFx_PRZ_cstSD_4pred_range$ls_predict[[4]])

## ----MFx_PRZ_cstSD_4MFxFIXhbr, cache=TRUE, eval=FALSE-------------------------
#  MFx_PRZ_cstSD_4MFxFIXhb <- MFx(object = fit_cstSDFIXhb, data_predict = data_4MFx, ode = FALSE,
#                                 hb_value = FALSE, hb_valueFORCED = fit_cstSDFIXhb$hb_valueFIXED)

## ----plotMFx_PRZ_cstSD_4MFxFIXhb, cache=TRUE, eval=FALSE----------------------
#  plot(MFx_PRZ_cstSD_4MFxFIXhb)

## ---- cache=TRUE--------------------------------------------------------------
# (1) load data set
data(cadmium2)

# (2) check structure and integrity of the data set
reproDataCheck(cadmium2)

# (3) create a `reproData` object
dat <- reproData(cadmium2)

# (4) represent the cumulated number of offspring as a function of time
plot(dat, concentration = 124, addlegend = TRUE, pool.replicate = FALSE)

# (5) represent the reproduction rate as a function of concentration
plotDoseResponse(dat, target.time = 28)

# (6) check information on the experimental design
summary(dat)

# (7) fit a concentration-effect model at target-time
fit <- reproFitTT(dat, stoc.part = "bestfit",
                  target.time = 21,
                  ecx = c(10, 20, 30, 40, 50),
                  quiet = TRUE)
summary(fit)

## ---- cache=TRUE--------------------------------------------------------------
plot(fit, log.scale = TRUE, adddata = TRUE,
     cicol = "orange",
     addlegend = TRUE)

## ---- cache=TRUE--------------------------------------------------------------
ppc(fit)

## ---- cache=TRUE--------------------------------------------------------------
summary(fit)

## ---- cache=TRUE--------------------------------------------------------------
dat <- reproData(cadmium2)
plot(survData(dat))

