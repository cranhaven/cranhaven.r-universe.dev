test_that("predict", {
  
  skip_on_cran()
  
  # TEST 1
  data("Male_Gammarus_Single")
  Male_Gammarus_Single <- Male_Gammarus_Single[Male_Gammarus_Single$replicate == 1, ]
  modelData_MGS <- modelData(Male_Gammarus_Single, time_accumulation = 4)
  fit_MGS <- fitTK(modelData_MGS, iter = 1000, chains = 2)
  
  data_4pred_MGS <- data.frame( time = 0:25, expw = 7.08e-05)
  predict_MGS <- predict(fit_MGS, data_4pred_MGS, fixed_init = TRUE)
  plot(fit_MGS)
  plot(predict_MGS)
  
  parfit_MGS <- rstan::extract(fit_MGS[["stanfit"]])
  # SEE quantile_table(fit_MGS)
  manual_MGS = data.frame(
    kee = parfit_MGS$ke[,1],
    kuw = parfit_MGS$ku[,1],
    sigmaConc = parfit_MGS$sigmaCGpred[,1]
  )
  predict_MGS_m <- predict_manual(manual_MGS, data_4pred_MGS, C0 = 0.023, time_accumulation = 4)
  plot(predict_MGS_m)

  mcmc_MGS_m1 = data.frame(
    kee = 0.4,
    kuw = 592.024
  )
  
  predict_MGS_mcmc_1 <- predict_manual(mcmc_MGS_m1, data_4pred_MGS, C0 = 0.023, time_accumulation = 4)
  plot(predict_MGS_mcmc_1)
  
  ### TEST 2
  data("Male_Gammarus_seanine_growth")
  modelData_MGSG <- modelData(Male_Gammarus_seanine_growth, time_accumulation = 1.417)
  fit_MGSG <- fitTK(modelData_MGSG, iter = 1000, chains=2)
  
  data_4pred_MGSG <- data.frame(time = sort(c(0:6,1.417)), expw = 15.533)
  predict_MGSG <- predict(fit_MGSG, data_4pred_MGSG)
  plot(fit_MGSG)
  plot(predict_MGSG)
  
  parfit_MGSG <- rstan::extract(fit_MGSG[["stanfit"]])
  # quantile_table(fit_MGSG)
  manual_MGSG = data.frame(
    kee = parfit_MGSG$ke[,1],
    keg = parfit_MGSG$ke[,2],
    kuw = parfit_MGSG$ku[,1],
    sigmaConc = parfit_MGSG$sigmaCGpred[,1],
    sigmaGrowth = parfit_MGSG$sigmaCGpred[,2],
    km1 = parfit_MGSG$km[,1],
    km2 = parfit_MGSG$km[,2],
    km3 = parfit_MGSG$km[,3],
    kem1 = parfit_MGSG$kem[,1],
    kem2 = parfit_MGSG$kem[,2],
    kem3 = parfit_MGSG$kem[,3],
    sigmaCmet1 = parfit_MGSG$sigmaCmetpred[,1],
    sigmaCmet2 = parfit_MGSG$sigmaCmetpred[,2],
    sigmaCmet2 = parfit_MGSG$sigmaCmetpred[,3]
  )
  predict_MGSG_m <- predict_manual(
    manual_MGSG, data_4pred_MGSG, C0 = 0, time_accumulation = 1.417, G0 = 2e-1, gmax=4.5e-1
  )
  plot(predict_MGSG_m)
  
  predict_MGSG_m1 <- predict_manual(
    manual_MGSG[1,], data_4pred_MGSG, C0 = 0, time_accumulation = 1.417, G0 = 2e-1, gmax=4.5e-1
  )
  plot(predict_MGSG_m1)
  
  ### TEST 3
  data("Chiro_Creuzot")
  Chiro_Creuzot <- Chiro_Creuzot[Chiro_Creuzot$replicate == 1,]
  modelData_CC <- modelData(Chiro_Creuzot, time_accumulation = 1.0)
  fit_CC <- fitTK(modelData_CC, iter = 1000, chains=2)
  
  data_4pred_CC <- data.frame(time = seq(0,4,0.5), expw = 22.9, exps = 1315.7, exppw = 16.24)
  predict_CC <- predict(fit_CC, data_4pred_CC)
  plot(fit_CC)
  plot(predict_CC)
  
  parfit_CC <- rstan::extract(fit_CC[["stanfit"]])
  # quantile_table(fit_CC)
  manual_CC = data.frame(
    kee = parfit_CC$ke[,1],
    kuw = parfit_CC$ku[,1],
    kus = parfit_CC$ku[,2],
    kupw = parfit_CC$ku[,3],
    sigmaConc = parfit_CC$sigmaCGpred[,1],
    km1 = parfit_CC$km[,1],
    km2 = parfit_CC$km[,2],
    kem1 = parfit_CC$kem[,1],
    kem2 = parfit_CC$kem[,2],
    sigmaCmet1 = parfit_CC$sigmaCmetpred[,1],
    sigmaCmet2 = parfit_CC$sigmaCmetpred[,2]
  )
  predict_CC_m <- predict_manual(manual_CC, data_4pred_CC, C0 = 371.9, time_accumulation = 1.0)
  plot(predict_CC_m)
  
  predict_CC_m1 <- predict_manual(manual_CC[1,], data_4pred_CC, C0 = 371.9, time_accumulation = 1.0)
  plot(predict_CC_m1)
  
})