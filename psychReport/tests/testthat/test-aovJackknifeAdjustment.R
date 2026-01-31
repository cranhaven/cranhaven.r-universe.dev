context("aovJackknifeAdjustment")

test_that("aovJackknifeAdjustment", {

  set.seed(1)

  # create dataframe with 2(Comp: comp vs. incomp) and 2(Side: left vs. right) factors/levels
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("comp", "incomp")))

  dat <- addDataDF(dat,
                   RT = list("Comp comp"   = c(500, 30, 50),
                             "Comp incomp" = c(800, 30, 50)))

  # base R aov
  aovRT      <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  aovRT_pre  <- aovTable(aovRT)
  aovRT      <- aovJackknifeAdjustment(aovRT, length(unique(dat$VP)))
  aovRT_post <- aovTable(aovRT)

  testthat::expect_equal(round(as.numeric(aovRT_pre$ANOVA$F)/(49*49), 2), as.numeric(aovRT_post$ANOVA$F))

  # ezANOVA
  aovRT      <- ez::ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp), return_aov = TRUE, detailed = TRUE)
  aovRT_pre  <- aovTable(aovRT)
  aovRT      <- aovJackknifeAdjustment(aovRT, length(unique(dat$VP)))
  aovRT_post <- aovTable(aovRT)

  testthat::expect_equal(round(as.numeric(aovRT_pre$ANOVA$F)/(49*49), 2), as.numeric(aovRT_post$ANOVA$F))

})
