test_that("evbs_monitor reproduces the Itajai control chart", {
  data(itajai)
  ct <- evbs_monitor(cbind(1, itajai$pressure), itajai$wind, burn = 40)
  expect_true(all(c("t","ymax","flagged","F") %in% names(ct)))
  before <- mean(ct$F[ct$t < 82], na.rm = TRUE)
  after  <- mean(ct$F[ct$t >= 82], na.rm = TRUE)
  expect_gt(before, 1)   # em controle antes do evento
  expect_lt(after, 1)    # fora de controle depois
  expect_lt(ct$F[ct$t == 82], 0.9)
})
test_that("the naive index disagrees with the curvature-based one", {
  data(itajai)
  a <- evbs_monitor(cbind(1, itajai$pressure), itajai$wind, burn = 40, use = "curvature")
  b <- evbs_monitor(cbind(1, itajai$pressure), itajai$wind, burn = 40, use = "max")
  expect_false(isTRUE(all.equal(a$F, b$F)))
})
