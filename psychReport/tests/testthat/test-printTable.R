context("printTable")

test_that("printTable", {

  # create dataframe
  dat <- createDF(nVP = 6, nTrl = 1,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat, RT = list("Comp comp"   = c(500, 150, 100),
                                  "Comp incomp" = c(520, 150, 100)))

  # base R aov
  aovRT <- aov(RT ~ Comp + Error(VP/Comp), dat)
  aovRT <- aovTable(aovRT)

  testthat::expect_error(printTable(aovRT$ANOVA), NA)

  # ezANOVA
  aovRT <- ez::ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                       return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT)

  testthat::expect_error(printTable(dat), NA)
  testthat::expect_error(printTable(dat, caption = "Test", digits = c(0, 2)), NA)
  testthat::expect_error(printTable(dat, caption = "Test", digits = c(0, 2)), NA)
  testthat::expect_error(printTable(dat, caption = "Test", digits = c(0, 2, 0)))
  testthat::expect_error(printTable(aovRT$ANOVA), NA)

})
