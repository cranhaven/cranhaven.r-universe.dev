test_that("input works", {
  data("LDAdata")
  Z1 <- subset.data.frame(LDAdata,subset = name == unique(LDAdata$name)[1])
  LDA_tab <- Z1[,c("S-value","# Tested","# Clonal growth","Group","replicate")]
  expect_error(LDA_plot_activity(LDA_tab))
})
