test_that("fit ode", {
  
  skip_on_cran()
  
  data("Exposure_Sialis_lutaria")
  data("Internal_Sialis_lutaria")
  Exposure_Sialis_lutaria$value = Exposure_Sialis_lutaria$Cwater
  Internal_Sialis_lutaria$value = Internal_Sialis_lutaria$Cinternal

  modeldata_SL <- modelData_ode(Exposure_Sialis_lutaria, Internal_Sialis_lutaria, time_accumulation = 2.170)
  expect_error(modeldata_SL, NA)
  
  fit_SL <- fitTK(modeldata_SL, iter = 1000)
  expect_error(fit_SL, NA)

})


