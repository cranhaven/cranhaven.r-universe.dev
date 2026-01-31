context("aovEffectSize")

test_that("aovEffectSize", {

  set.seed(1)

  dat <- createDF(nVP = 20, nTrl = 1,
                  design = list("Comp" = c("comp", "incomp")))

  dat <- addDataDF(dat, RT = list("Comp comp"   = c(500, 150, 100),
                                  "Comp incomp" = c(550, 150, 100)))

  # base R aov
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)

  testthat::expect_error(aovEffectSize(aovRT, effectSize = "pes"), NA)
  testthat::expect_error(aovEffectSize(aovRT, effectSize = "ges"), NA)

  # ezANOVA
  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = TRUE)

  testthat::expect_error(aovEffectSize(aovRT, effectSize = "pes"), NA)
  testthat::expect_error(aovEffectSize(aovRT, effectSize = "ges"), NA)

})
