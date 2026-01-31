context("sphericityValueString")

test_that("sphericityValueString", {

  set.seed(1)

  # create dataframe and add data with 3(Comp: neutral vs. comp vs. incomp) levels
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("neutral", "comp", "incomp")))
  dat <- addDataDF(dat, RT = list("Comp neutral" = c(510, 150, 100),
                                  "Comp comp"    = c(500, 150, 100),
                                  "Comp incomp"  = c(520, 150, 100)))

  # ezANOVA
  aovRT <- ez::ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT)

  sphericityValue <- sphericityValueString(aovRT, "Comp")
  testthat::expect_equal(sphericityValue, "$\\epsilon$ = 0.99")

  # repeated measures ANOVA using ezANOVA
  aovRT <- ez::ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT, sphericityCorrectionType = "HF")

  sphericityValue <- sphericityValueString(aovRT, "Comp")
  testthat::expect_equal(sphericityValue, "$\\epsilon$ = 1.03")

})
