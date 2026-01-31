context("fValueString")

test_that("fValueString", {

  set.seed(1)

  # simulated data for ANOVA
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("comp", "incomp"),
                                "Side" = c("left", "right")))
  dat <- addDataDF(dat, RT = list("Comp:Side comp:left"    = c(500, 150, 100),
                                  "Comp:Side comp:right"   = c(500, 150, 100),
                                  "Comp:Side incomp:left"  = c(550, 150, 100),
                                  "Comp:Side incomp:right" = c(550, 150, 100)))

  # base R aov
  aovRT <- aov(RT ~ Comp*Side + Error(VP/(Comp*Side)), dat)

  testthat::expect_error(fValueString(aovRT, "Comp"))

  aovRT <- aovTable(aovRT)

  testthat::expect_equal(fValueString(aovRT, "Comp"), "\\emph{F}(1, 49) = 4.39")
  testthat::expect_equal(fValueString(aovRT, "Side"), "\\emph{F}(1, 49) = 0.32")
  testthat::expect_equal(fValueString(aovRT, "Comp:Side"), "\\emph{F}(1, 49) = 0.49")


  # repeated measures ANOVA using ezANOVA
  aovRT <- ez::ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp, Side),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT)

  testthat::expect_equal(fValueString(aovRT, "Comp"), "\\emph{F}(1, 49) = 4.39")
  testthat::expect_equal(fValueString(aovRT, "Side"), "\\emph{F}(1, 49) = 0.32")
  testthat::expect_equal(fValueString(aovRT, "Comp:Side"), "\\emph{F}(1, 49) = 0.49")

})
