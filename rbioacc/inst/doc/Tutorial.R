## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----loadPackage--------------------------------------------------------------
library(rbioacc)
# library(ggplot2)

## ----dataMGS------------------------------------------------------------------
data("Male_Gammarus_Single")

## ----fitMGS, cache=TRUE, results="hide"---------------------------------------
modelData_MGS <- modelData(Male_Gammarus_Single, time_accumulation = 4)
fit_MGS <- fitTK(modelData_MGS, iter = 10000)

## ----statsMGS-----------------------------------------------------------------
quantile_table(fit_MGS)

## ----plotMGS, fig.height=4, fig.width=5---------------------------------------
plot(fit_MGS)

## ----ppcMGS, fig.height=4, fig.width=5----------------------------------------
ppc(fit_MGS)

## ----fitMGM708, cache=TRUE, results="hide", eval=FALSE------------------------
#  data("Male_Gammarus_Merged")
#  data_MGM708 <- Male_Gammarus_Merged[Male_Gammarus_Merged$expw == 7.08021e-05, ]
#  modelData_MGM708 <- modelData(data_MGM708, time_accumulation = 4)
#  fit_MGM708 <- fitTK(modelData_MGM708, iter = 10000)

## ----statMGM708, eval=FALSE---------------------------------------------------
#  quantile_table(fit_MGM708)

## ----plotMGM708, fig.height=4, fig.width=5, eval=FALSE------------------------
#  plot(fit_MGM708)

## ----ppcMGM708, fig.height=4, fig.width=5, eval=FALSE-------------------------
#  ppc(fit_MGM708)

## ----fitMGM141, cache=TRUE, results="hide", eval=FALSE------------------------
#  data_MGM141 <- Male_Gammarus_Merged[Male_Gammarus_Merged$expw == 1.41604e-04, ]
#  modelData_MGM141 <- modelData(data_MGM141, time_accumulation = 7)
#  fit_MGM141 <- fitTK(modelData_MGM141, iter = 20000)

## ----statMGM141, eval=FALSE---------------------------------------------------
#  quantile_table(fit_MGM141)

## ----plotMGM141, fig.height=4, fig.width=5, eval=FALSE------------------------
#  plot(fit_MGM141)

## ----ppcMGM141, fig.height=4, fig.width=5, eval=FALSE-------------------------
#  ppc(fit_MGM141)

## ----fitMGM283, cache=TRUE, results="hide", eval=FALSE------------------------
#  data_MGM283 <- Male_Gammarus_Merged[Male_Gammarus_Merged$expw == 2.83208e-04, ]
#  modelData_MGM283 <- modelData(data_MGM283, time_accumulation = 4)
#  fit_MGM283 <- fitTK(modelData_MGM283, iter = 10000)

## ----statMGM283, eval=FALSE---------------------------------------------------
#  quantile_table(fit_MGM283)

## ----plotMGM283, fig.height=4, fig.width=5, eval=FALSE------------------------
#  plot(fit_MGM283)

## ----ppcMGM283, fig.height=4, fig.width=5, eval=FALSE-------------------------
#  ppc(fit_MGM283)

## ----fitMGSG, eval=FALSE------------------------------------------------------
#  data("Male_Gammarus_seanine_growth")
#  modelData_MGSG <- modelData(Male_Gammarus_seanine_growth, time_accumulation = 1.417)
#  fit_MGSG <- fitTK(modelData_MGSG, iter = 10000)

## ----statsMGSG, eval=FALSE----------------------------------------------------
#  quantile_table(fit_MGSG)

## ----plotMGSG, fig.height=6, fig.width=7, eval=FALSE--------------------------
#  plot(fit_MGSG)

## ----ppcMGSG, fig.height=6, fig.width=7, eval=FALSE---------------------------
#  ppc(fit_MGSG)

## ----fitOT440, cache=TRUE, results="hide", eval=FALSE-------------------------
#  data("Oncorhynchus_two") #  Pimephales_two
#  data_OT440 = Oncorhynchus_two[Oncorhynchus_two$expw == 0.00440,]
#  modelData_OT440 <- modelData(data_OT440, time_accumulation = 49)
#  fit_OT440 <- fitTK(modelData_OT440, iter = 10000)

## ----statOT440, eval=FALSE----------------------------------------------------
#  quantile_table(fit_OT440)

## ----plotOT440, fig.height=4, fig.width=5, eval=FALSE-------------------------
#  plot(fit_OT440)

## ----ppcOT440, fig.height=4, fig.width=5, eval=FALSE--------------------------
#  ppc(fit_OT440)

## ----fitOT041, cache=TRUE, results="hide", eval=FALSE-------------------------
#  data_OT041 <- Oncorhynchus_two[Oncorhynchus_two$expw == 0.00041,]
#  modelData_OT041 <- modelData(data_OT041, time_accumulation = 49)
#  fit_OT041 <- fitTK(modelData_OT041, iter = 10000)

## ----statOT041, eval=FALSE----------------------------------------------------
#  quantile_table(fit_OT041)

## ----plotOT041, fig.height=4, fig.width=5, eval=FALSE-------------------------
#  plot(fit_OT041)

## ----ppcOT041, fig.height=4, fig.width=5, eval=FALSE--------------------------
#  ppc(fit_OT041)

## ----fitCB, cache=TRUE, results="hide", eval=FALSE----------------------------
#  data("Chironomus_benzoapyrene")
#  modelData_CB <- modelData(Chironomus_benzoapyrene, time_accumulation = 3)
#  modelData_CB$unifMax = modelData_CB$unifMax  * 100
#  fit_CB <- fitTK(modelData_CB, iter = 10000)

## ----statCB, eval=FALSE-------------------------------------------------------
#  quantile_table(fit_CB)

## ----plotCB, fig.height=4, fig.width=5, eval=FALSE----------------------------
#  plot(fit_CB)

## ----ppcCB, fig.height=4, fig.width=5, eval=FALSE-----------------------------
#  ppc(fit_CB)

## ----predictMGS, eval=FALSE---------------------------------------------------
#  data("Male_Gammarus_Single")
#  modelData_MGS <- modelData(Male_Gammarus_Single, time_accumulation = 4)
#  fit_MGS <- fitTK(modelData_MGS, iter = 5000, chains = 3)
#  
#  # Data 4 prediction should respect the exposure routes
#  data_4pred <- data.frame( time = 1:25, expw = 4e-5 )
#  predict_MGS <- predict(fit_MGS, data_4pred)
#  plot(predict_MGS)

## ----predictMGSG, eval=FALSE--------------------------------------------------
#  # data("Male_Gammarus_seanine_growth")
#  # modelData_MGSG <- modelData(Male_Gammarus_seanine_growth, time_accumulation = 4)
#  # fit_MGSG <- fitTK(modelData_MGSG, iter = 5000, chains = 3)
#  #
#  # # Data 4 prediction should respect the exposure routes
#  # data_4pred <- data.frame( time = 1:25, expw = 18 )
#  # predict_MGSG <- predict(fit_MGSG, data_4pred)
#  # plot(predict_MGSG)

## ----predictCC, eval=FALSE----------------------------------------------------
#  data("Chiro_Creuzot")
#  Chiro_Creuzot <- Chiro_Creuzot[Chiro_Creuzot$replicate == 1,]
#  modelData_CC <- modelData(Chiro_Creuzot, time_accumulation = 1.0)
#  fit_CC <- fitTK(modelData_CC, iter = 5000, chains = 3)
#  # --------
#  quantile_table(fit_CC)
#  
#  # Data 4 prediction should respect the exposure routes
#  data_4pred <- data.frame( time = 1:25, expw = 18, exps = 1200, exppw = 15 )
#  predict_CC <- predict(fit_CC, data_4pred)
#  plot(predict_CC)

