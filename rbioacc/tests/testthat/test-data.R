test_that("data load no complain", {
  
  expect_silent(data("Male_Gammarus_Single"))
  expect_silent(data("Male_Gammarus_Merged"))
  expect_silent(data("Male_Gammarus_seanine_growth"))
  
  expect_silent(data("Oncorhynchus_two"))
  expect_silent(data("Chironomus_benzoapyrene"))
  expect_silent(data("Chiro_Creuzot"))
  
  expect_silent(data("Exposure_Sialis_lutaria"))
  expect_silent(data("Internal_Sialis_lutaria"))
  
})

test_that("data.frame class", {
  
  expect_true("data.frame" %in% class(Male_Gammarus_Single))
  expect_true("data.frame" %in% class(Male_Gammarus_seanine_growth))
  expect_true("data.frame" %in% class(Male_Gammarus_Merged))
  expect_true("data.frame" %in% class(Oncorhynchus_two))
  expect_true("data.frame" %in% class(Chironomus_benzoapyrene))
  expect_true("data.frame" %in% class(Chiro_Creuzot))
  expect_true("data.frame" %in% class(Exposure_Sialis_lutaria))
  expect_true("data.frame" %in% class(Internal_Sialis_lutaria))
  
})
