test_that("mcmcTraces", {
  
  # Very small fit to run super fast
  data("Male_Gammarus_Single")
  modelData_MGS <- modelData(Male_Gammarus_Single, time_accumulation = 4)
  fit_MGS <- fitTK(modelData_MGS, iter = 10, chains = 2)
  data("Male_Gammarus_seanine_growth")
  modelData_MGSG <- modelData(Male_Gammarus_seanine_growth, time_accumulation = 1.417)
  fit_MGSG <- fitTK(modelData_MGSG, iter = 10, chains=2)
  
  
  plt_MGS <- mcmcTraces(fit_MGS)
  plt_MGS_det <- mcmcTraces(fit_MGS,"deterministic")
  plt_MGS_sto <- mcmcTraces(fit_MGS,"stochastic")
  plt_MGSG <- mcmcTraces(fit_MGSG)
  plt_MGSG_det <- mcmcTraces(fit_MGSG,"deterministic")
  plt_MGSG_sto <- mcmcTraces(fit_MGSG,"stochastic")
  
  # Check class
  expect_true(all(class(plt_MGS) == c("gg", "ggplot")))
  expect_true(all(class(plt_MGS_det) == c("gg", "ggplot")))
  expect_true(all(class(plt_MGS_sto) == c("gg", "ggplot")))
  expect_true(all(class(plt_MGSG) == c("gg", "ggplot")))
  expect_true(all(class(plt_MGSG_det) == c("gg", "ggplot")))
  expect_true(all(class(plt_MGSG_sto) == c("gg", "ggplot")))
})