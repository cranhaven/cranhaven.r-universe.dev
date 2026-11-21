test_that("input works", {
  data("LDAdata")
  Z1 <- subset.data.frame(LDAdata,subset = name == unique(LDAdata$name)[1])
  LDA_tab <- Z1[,c("S-value","# Tested","# Clonal growth","Group","replicate")]
  expect_error(LDA_prepare_plot(list("eins" = LDA_tab)))
  expect_error(LDA_prepare_plot(LDA_tab = LDA_tab,
                                uncertainty = "no"))
  L0 <- subset.data.frame(x = LDA_tab,
                          subset = Group == 0)
  expect_identical(LDA_prepare_plot(LDA_tab = L0[,1:3]),
                   LDA_prepare_plot(LDA_tab = L0[,1:4]))
})
