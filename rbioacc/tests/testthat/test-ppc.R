test_that("ppc", {
  data("Male_Gammarus_Single")
  modelData_MGS <- modelData(Male_Gammarus_Single, time_accumulation = 4)
  fit_MGS <- fitTK(modelData_MGS, iter = 10, chains = 2)
  data("Male_Gammarus_seanine_growth")
  modelData_MGSG <- modelData(Male_Gammarus_seanine_growth, time_accumulation = 1.417)
  fit_MGSG <- fitTK(modelData_MGSG, iter = 10, chains=2)
  
  test_that("df_ppc", {
    expect_true(class(df_ppc(fit_MGS)) ==  "data.frame")
    expect_true(class(df_ppc(fit_MGSG)) ==  "data.frame")
  })
  
  test_that("plot ppc", {
    expect_true(all(class(ppc(fit_MGS)) == c("gg", "ggplot")))
    expect_true(all(class(ppc(fit_MGSG)) == c("gg", "ggplot")))
  })
})