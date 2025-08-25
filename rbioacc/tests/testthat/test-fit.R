test_that("test complex is running", {
  
  skip_on_cran()
  
  data("Male_Gammarus_Single")
  
  object = Male_Gammarus_Single
  time_accumulation = 4
  modelData_MGS = modelData(Male_Gammarus_Single, time_accumulation = 4)
  
  expect_error(modelData_MGS, NA)
  
  fit <- fitTK(modelData_MGS, iter = 1000)
  expect_error(fit, NA)

})
