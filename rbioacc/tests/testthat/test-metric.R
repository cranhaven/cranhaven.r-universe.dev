test_that("bioacc_metric", {
  
  # test_that("internal fun bioacc_metric",{
  #   expect_true(all(rbioacc:::.switch_k(c("w", "s", "f", "pw")) == c("BCFk","BSAFk","BMFk","BCFpwk")))
  #   expect_true(all(rbioacc:::.switch_ss(c("w", "s", "f", "pw")) == c("BCFss","BSAFss","BMFss","BCFpwss")))
  # })
  
  # small test to run fast
  data("Male_Gammarus_Single")
  modelData_MGS <- modelData(Male_Gammarus_Single, time_accumulation = 4)
  fit_MGS <- fitTK(modelData_MGS, iter = 10, chains = 4)
  data("Male_Gammarus_seanine_growth")
  modelData_MGSG <- modelData(Male_Gammarus_seanine_growth, time_accumulation = 1.417)
  fit_MGSG <- fitTK(modelData_MGSG, iter = 10, chains=4)
  data("Chiro_Creuzot")
  Chiro_Creuzot <- Chiro_Creuzot[Chiro_Creuzot$replicate == 1,]
  modelData_CC <- modelData(Chiro_Creuzot, time_accumulation = 1.0)
  fit_CC <- fitTK(modelData_CC, iter = 10, chains=4)
  
  BFCk_MGS = bioacc_metric(fit_MGS, "k")
  BFCk_MGSG = bioacc_metric(fit_MGSG, "k")
  BFCk_CC = bioacc_metric(fit_CC, "k")
  BFCk_CCw = bioacc_metric(fit_CC, "k", route = "w")
  # Check class
  expect_true(all(class(BFCk_MGS) == c("bioaccMetric", "data.frame")))
  expect_true(all(class(BFCk_MGSG) == c("bioaccMetric", "data.frame")))
  expect_true(all(class(BFCk_CC) == c("bioaccMetric", "data.frame")))
  # Check colnames
  expect_true(all(colnames(BFCk_MGS) == c("BCFk")))
  expect_true(all(colnames(BFCk_MGSG) == c("BCFk")))
  expect_true(all(colnames(BFCk_CC) == c("BCFk","BSAFk","BCFpwk")))
  # Check class
  expect_true(all(class(plot(BFCk_MGS)) == c("gg", "ggplot")))
  expect_true(all(class(plot(BFCk_MGSG)) == c("gg", "ggplot")))
  expect_true(all(class(plot(BFCk_CC)) == c("gg", "ggplot")))
  
  BFCss_MGS = bioacc_metric(fit_MGS, "ss")
  BFCss_MGSG = bioacc_metric(fit_MGSG, "ss")
  BFCss_CC = bioacc_metric(fit_CC, "ss")
  # Check class
  expect_true(all(class(BFCss_MGS) == c("bioaccMetric", "data.frame")))
  expect_true(all(class(BFCss_MGSG) == c("bioaccMetric", "data.frame")))
  expect_true(all(class(BFCss_CC) == c("bioaccMetric", "data.frame")))
  # Check colnames
  expect_true(all(colnames(BFCss_MGS) == c("BCFss")))
  expect_true(all(colnames(BFCss_MGSG) == c("BCFss")))
  expect_true(all(colnames(BFCss_CC) == c("BCFss","BSAFss","BCFpwss")))
  # Check class
  expect_true(all(class(plot(BFCss_MGS)) == c("gg", "ggplot")))
  expect_true(all(class(plot(BFCss_MGSG)) == c("gg", "ggplot")))
  expect_true(all(class(plot(BFCss_CC)) == c("gg", "ggplot")))
  
})
