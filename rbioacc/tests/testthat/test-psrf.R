test_that("psrf",{
  # Very small fit to run super fast
  data("Male_Gammarus_Single")
  modelData_MGS <- modelData(Male_Gammarus_Single, time_accumulation = 4)
  fit_MGS <- fitTK(modelData_MGS, iter = 10, chains = 2)
  data("Male_Gammarus_seanine_growth")
  modelData_MGSG <- modelData(Male_Gammarus_seanine_growth, time_accumulation = 1.417)
  fit_MGSG <- fitTK(modelData_MGSG, iter = 10, chains=2)
  
  psrf_MGS <- psrf(fit_MGS)
  psrf_MGSG <- psrf(fit_MGSG)
  
  # Check class
  expect_true(class(psrf_MGS) == "data.frame")
  expect_true(class(psrf_MGSG) == "data.frame")
  
  # Check column names
  expect_true(all(colnames(psrf_MGS) == c("PSRF", "parameter")))
  expect_true(all(colnames(psrf_MGSG) == c("PSRF", "parameter")))
  
  # Check number of parameter estimates
  expect_equal(length(psrf(fit_MGS)[,1]),3)
  expect_equal(length(psrf(fit_MGSG)[,1]),16)
})
