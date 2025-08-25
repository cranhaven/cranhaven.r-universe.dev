test_that("modelData outputs", {

  data("Male_Gammarus_Single")
  expect_error(modelData(Male_Gammarus_Single, time_accumulation = 4), NA)
  
  data("Male_Gammarus_seanine_growth")
  expect_error(modelData(Male_Gammarus_seanine_growth, time_accumulation = 1.417), NA)
  
  data("Male_Gammarus_Merged")
  df_1 <- with(Male_Gammarus_Merged, Male_Gammarus_Merged[expw == 7.08021e-05,])
  expect_error(modelData(df_1, time_accumulation = 4), NA)
  
  data("Oncorhynchus_two")
  df_2 <- with(Oncorhynchus_two, Oncorhynchus_two[replicate == 1,])
  expect_error(modelData(df_2, time_accumulation = 49), NA)
  
  data("Chironomus_benzoapyrene")
  expect_error(modelData(Chironomus_benzoapyrene, time_accumulation = 3), NA)
  
})

test_that("TEST .check_modelData_object", {
  data("Male_Gammarus_Single")
  test_object <- Male_Gammarus_Single
  expect_null(rbioacc:::.check_modelData_object(test_object))
})



