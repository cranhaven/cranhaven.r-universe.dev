test_that("waic", {
  
  # Very small fit to run super fast
  data("Male_Gammarus_Single")
  modelData_MGS <- modelData(Male_Gammarus_Single, time_accumulation = 4)
  fit_MGS <- fitTK(modelData_MGS, iter = 10, chains = 2)
  data("Male_Gammarus_seanine_growth")
  modelData_MGSG <- modelData(Male_Gammarus_seanine_growth, time_accumulation = 1.417)
  fit_MGSG <- fitTK(modelData_MGSG, iter = 10, chains=2)
  
  expect_error(waic_MGS <- waic(fit_MGS), NA)
  expect_error(waic_MGSG <- waic(fit_MGSG), NA)
  
  expect_true(all(class(waic_MGS) == "numeric"))
  expect_true(all(class(waic_MGSG) == "numeric"))
  
})
