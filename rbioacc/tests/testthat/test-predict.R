test_that("predict", {
  
  skip_on_cran()
  
  # small test to run fast
  data("Male_Gammarus_Single")
  Male_Gammarus_Single <- Male_Gammarus_Single[Male_Gammarus_Single$replicate == 1, ]
  modelData_MGS <- modelData(Male_Gammarus_Single, time_accumulation = 4)
  fit_MGS <- fitTK(modelData_MGS, iter = 10, chains = 2)
  
  data("Male_Gammarus_seanine_growth")
  modelData_MGSG <- modelData(Male_Gammarus_seanine_growth, time_accumulation = 1.417)
  fit_MGSG <- fitTK(modelData_MGSG, iter = 10, chains=2)

  # reduce to have only one metabolite
  MSGG_1met <- Male_Gammarus_seanine_growth[, c("time", "expw","conc","concm1","replicate","growth")]
  modelData_MGSG_1met <- modelData(MSGG_1met, time_accumulation = 1.417)
  fit_MGSG_1met <- fitTK(modelData_MGSG_1met, iter = 10, chains=2)
  
  data("Chiro_Creuzot")
  Chiro_Creuzot <- Chiro_Creuzot[Chiro_Creuzot$replicate == 1,]
  modelData_CC <- modelData(Chiro_Creuzot, time_accumulation = 1.0)
  fit_CC <- fitTK(modelData_CC, iter = 10, chains=2)
  
  # SAME DATA for test
  # Data 4 prediction should respect the exposure routes
  data_4pred_MGS <- data.frame( time = 0:25, expw = 7.08e-05)
  predict_MGS <- predict(fit_MGS, data_4pred_MGS, fixed_init = TRUE)
  plot(fit_MGS)
  plot(predict_MGS)
  
  predict_MGS <- predict(fit_MGS, data_4pred_MGS, fixed_init = FALSE)
  plot(fit_MGS)
  plot(predict_MGS)
  
  ###############
  data_4pred_MGSG <- data.frame(time = sort(c(0:6,1.417)), expw = 15.533)
  predict_MGSG <- predict(fit_MGSG, data_4pred_MGSG)
  plot(fit_MGSG)
  plot(predict_MGSG)
  
  ###############
  predict_MGSG_1met <- predict(fit_MGSG_1met, data_4pred_MGSG)
  plot(fit_MGSG_1met)
  plot(predict_MGSG_1met)
  
  ############
  data_4pred_CC <- data.frame( time = seq(0,4,0.5), expw = 22.9, exps = 1315.7, exppw = 16.24)
  predict_CC <- predict(fit_CC, data_4pred_CC)
  plot(fit_CC)
  plot(predict_CC)
  
  expect_true(all(class(plot(predict_MGS)) == c("gg","ggplot")))
  expect_true(all(class(plot(predict_MGSG)) == c("gg", "ggplot")))
  expect_true(all(class(plot(predict_CC)) == c("gg", "ggplot")))
  
})

test_that( "predict_stan", {
  
  skip_on_cran()
  
  # small test to run fast
  data("Male_Gammarus_Single")
  Male_Gammarus_Single <- Male_Gammarus_Single[Male_Gammarus_Single$replicate == 1, ]
  modelData_MGS <- modelData(Male_Gammarus_Single, time_accumulation = 4)
  fit_MGS <- fitTK(modelData_MGS, iter = 100, chains = 2)
  data_4pred_MGS <- data.frame( time = 0:25, expw = 7.08e-05)
  
  data("Male_Gammarus_seanine_growth")
  modelData_MGSG <- modelData(Male_Gammarus_seanine_growth, time_accumulation = 1.417)
  fit_MGSG <- fitTK(modelData_MGSG, iter = 100, chains=2)
  data_4pred_MGSG <- data.frame(time = sort(c(0:6,1.417)), expw = 15.533)

  # Classical
  predict_MGS <- predict(fit_MGS, data_4pred_MGS, fixed_init = TRUE)
  plot(fit_MGS)
  plot(predict_MGS)
  
  predict_MGS <- predict(fit_MGS, fit_MGS$stanTKdata$origin_data, fixed_init = TRUE)
  plot(fit_MGS)
  plot(predict_MGS)
  
  # Same timeline
  predict_MGS_data <- modelData_predictStan(fit_MGS, data_4pred_MGS, fixed_init = TRUE)
  predict_MGS_stan <- predict_stan(fit_MGS, data_4pred_MGS, iter = 1000, chains = 1)
  plot(predict_MGS_stan, add_data = TRUE)
  
  predict_MGSG_data <- modelData_predictStan(fit_MGSG, data_4pred_MGSG, fixed_init = TRUE)
  predict_MGSG_stan <- predict_stan(fit_MGSG, data_4pred_MGSG, iter = 1000, chains = 1)
  plot(predict_MGSG_stan)
  plot(predict_MGSG_stan, add_data = TRUE)
  plot(fit_MGSG)
  
  # Extended timeline
  fit_pred_extend <- predict_stan(fit_MGS, data_4pred_MGS, iter = 1000, chains = 3, time_interp = seq(0,25,0.1))
  plot(fit_pred_extend)
  
  predict_MGSG_data <- modelData_predictStan(fit_MGSG, data_4pred_MGSG, time_interp = seq(0,6,0.1))
  predict_MGSG_stan_ext <- predict_stan(fit_MGSG, data_4pred_MGSG, iter = 1000, chains = 1, time_interp = seq(0,6,0.1))
  plot(predict_MGSG_stan_ext)
  plot(predict_MGSG_stan_ext, add_data = TRUE)
  plot(fit_MGSG)
  plot(predict_MGSG_stan)
  
  plot(fit_MGSG, time_interp = seq(0,6,0.1))

})

  