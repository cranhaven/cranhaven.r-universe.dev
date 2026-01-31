context("addDataDF")

test_that("addDataDF", {

  set.seed(1)

  # default 2*2
  dat <- createDF(nTrl = 100)
  dat <- addDataDF(dat)

  testthat::expect_equal(nrow(dat), 8000)
  testthat::expect_equal(ncol(dat), 5)
  testthat::expect_equal(names(dat), c("VP", "A", "B", "RT", "Error"))

  # default 2*2 with defined rt + error rate
  dat <- createDF(nTrl = 100)
  dat <- addDataDF(dat, RT = c(500, 150, 100), Error = c(10))

  testthat::expect_equal(nrow(dat), 8000)
  testthat::expect_equal(ncol(dat), 5)
  testthat::expect_equal(names(dat), c("VP", "A", "B", "RT", "Error"))

  # 1 factor with 2 levels + defined rt and error rates
  dat <- createDF(nVP = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat, RT = list("Comp comp"   = c(500, 150, 150),
                                  "Comp incomp" = c(550, 150, 150)),
                   Error = list("Comp comp"   = c(10, 8, 6, 4),
                                "Comp incomp" = c(15, 10, 7, 5)))

  testthat::expect_equal(nrow(dat), 20*50*2)
  testthat::expect_equal(ncol(dat), 4)
  testthat::expect_equal(names(dat), c("VP", "Comp", "RT", "Error"))

  # 1 factor with 3 levels
  dat <- createDF(nVP = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp", "neutral")))
  dat <- addDataDF(dat)

  testthat::expect_equal(nrow(dat), 20*50*3)
  testthat::expect_equal(ncol(dat), 4)
  testthat::expect_equal(names(dat), c("VP", "Comp", "RT", "Error"))

  # 2 factors, 3 & 2 levels
  dat <- createDF(nVP = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp", "neutral"),
                                                     "Side" = c("left", "right")))
  dat <- addDataDF(dat)

  testthat::expect_equal(nrow(dat), 20*50*6)
  testthat::expect_equal(ncol(dat), 5)
  testthat::expect_equal(names(dat), c("VP", "Comp", "Side", "RT", "Error"))

  # 1 factor, 1 VP, 1 trial
  dat <- createDF(nVP = 1, nTrl = 1, design = list("A" = c("a", "b")))
  dat <- addDataDF(dat)

  testthat::expect_equal(nrow(dat), 2)
  testthat::expect_equal(ncol(dat), 4)
  testthat::expect_equal(names(dat), c("VP", "A", "RT", "Error"))

})
