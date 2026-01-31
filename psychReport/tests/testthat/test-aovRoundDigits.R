context("aovRoundDigits")

test_that("aovRoundDigits", {

  set.seed(1)

  # create dataframe
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("comp", "neutral", "incomp")))

  dat <- addDataDF(dat,
                   RT = list("Comp comp"    = c(500, 150, 150),
                             "Comp neutral" = c(550, 150, 150),
                             "Comp incomp"  = c(600, 150, 150)))

  # base R aov
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  aovRT <- aovRoundDigits(aovRT)

  testthat::expect_equal(as.character(aovRT$ANOVA$F[1]), "2.17")

  # repeated measures ANOVA using ezANOVA
  aovRT <- ez::ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                       return_aov = TRUE, detailed = TRUE)
  aovRT <- aovRoundDigits(aovRT)  # 3 sig decimal places

  testthat::expect_equal(as.character(aovRT$ANOVA$F[1]), "1346.04")
  testthat::expect_equal(as.character(aovRT$ANOVA$F[2]), "2.17")

  aovRT <- ez::ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                       return_aov = TRUE, detailed = TRUE)
  aovRT <- aovRoundDigits(aovRT)  # 1 sig decimal places

  testthat::expect_equal(as.character(aovRT$ANOVA$F[1]), "1346.04")
  testthat::expect_equal(as.character(aovRT$ANOVA$F[2]), "2.17")

})
