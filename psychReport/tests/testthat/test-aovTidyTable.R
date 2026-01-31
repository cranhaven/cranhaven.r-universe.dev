context("aovTidyTable")

test_that("aovTidyTable", {

  set.seed(1)

  # create dataframe
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("comp", "neutral", "incomp")))

  dat <- addDataDF(dat,
                   RT = list("Comp comp"    = c(500, 150, 150),
                             "Comp neutral" = c(550, 150, 150),
                             "Comp incomp"  = c(600, 150, 150)))

  aovRt <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  testthat::expect_error(aovTidyTable(aovRt), NA)

})

