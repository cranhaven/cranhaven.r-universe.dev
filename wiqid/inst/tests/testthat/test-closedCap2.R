
# test_that code for closedCap series of functions
#   which need a full capture-history matrix

# library(testthat)

context("Closed captures/CHmatrix")

test_that("closedCapMb gives right answers",  {
  data(KanhaTigers)
  # Check dots passed to nlm
  expect_warning(closedCapMb(KanhaTigers, iterlim=4),
      "Convergence may not have been reached")
  res <- closedCapMb(KanhaTigers)
  resM <- closedCapMb(KanhaTigers, ciType='MARK')
  expect_that(class(res), equals(c("wiqid", "list")))
  expect_that(names(res), equals(c("call", "beta", "beta.vcv", "real", "logLik")))
  expect_that(colnames(res$real), equals(c("est", "lowCI", "uppCI")))
  expect_that(rownames(res$real), equals(c("Nhat", "phat", "chat")))
  expect_that(rownames(resM$real), equals(c("Nhat", "phat", "chat")))
  expect_that(round(as.vector(res$real[1, ]), 2),
        is_equivalent_to(c(26.94, 26.02, 67.96)))
  # Different R versions give different results for the 3rd decimal place.
  expect_that(round(as.vector(resM$real[1, ]), 3),
      is_equivalent_to(c(26.939, 26.081, 36.858)))
      # MARK gives 36.861 for the upper limit
  expect_that(round(as.vector(res$real[-1, ]), 4),
      is_equivalent_to(c(0.2539, 0.1916, 0.1515, 0.1388, 0.3935, 0.2584)))
      # same values as MARK
  expect_that(resM$real[-1, ], equals(res$real[-1, ]))
  expect_that(round(AIC(res), 4), equals(156.4496))
      # Same as MARK.
  expect_that(AIC(resM), equals(AIC(res)))

  res <- closedCapMb(KanhaTigers, ci=0.85)
  resM <- closedCapMb(KanhaTigers, ci=0.85, ciType='MARK')
  expect_that(round(as.vector(res$real[ ,2]), 4),
      is_equivalent_to(c(26.0576, 0.1749, 0.1515)))
  expect_that(round(as.vector(resM$real[1, ]), 3),
      is_equivalent_to(c(26.939, 26.156, 31.668)))
} )

test_that("closedCapMt gives right answers",  {
  data(KanhaTigers)
  # Check dots passed to nlm
  expect_warning(closedCapMt(KanhaTigers, iterlim=4),
      "Convergence may not have been reached")
  res <- closedCapMt(KanhaTigers)
  resM <- closedCapMt(KanhaTigers, ciType='MARK')
  expect_that(class(res), equals(c("wiqid", "list")))
  expect_that(names(res), equals(c("call", "beta", "beta.vcv", "real", "logLik")))
  expect_that(colnames(res$real), equals(c("est", "lowCI", "uppCI")))
  expect_that(rownames(res$real), equals(c("Nhat", "p1", "p2", "p3", "p4",
      "p5", "p6", "p7", "p8", "p9", "p10")))
  expect_that(rownames(resM$real), equals(c("Nhat", "p1", "p2", "p3", "p4",
      "p5", "p6", "p7", "p8", "p9", "p10")))
  expect_that(round(as.vector(res$real[1, ]), 3),
      is_equivalent_to(c(28.051, 26.304, 39.860)))
  expect_that(round(as.vector(resM$real[1, ]), 3),
      is_equivalent_to(c(28.051, 26.413, 36.178)))
      # MARK gives 36.180 for the upper limit
  expect_that(round(as.vector(res$real[-1, ]), 4),
      is_equivalent_to(c(
        0.1782,0.1782,0.1069,0.4634,0.2495,0.1426,0.1426,0.1426,0.1782,0.2852,
        0.0752,0.0752,0.0346,0.2823,0.1217,0.0539,0.0539,0.0539,0.0752,0.1466,
        0.3667,0.3667,0.2860,0.6547,0.4438,0.3267,0.3267,0.3267,0.3667,0.4809)))
      # same values as MARK, +/- 0.0001
  expect_that(resM$real[-1, ], equals(res$real[-1, ]))
  expect_that(round(AIC(res), 4), equals(158.0698))
      # Same as MARK.
  expect_that(AIC(resM), equals(AIC(res)))

  res <- closedCapMt(KanhaTigers, ci=0.85)
  resM <- closedCapMt(KanhaTigers, ci=0.85, ciType='MARK')
  expect_that(round(as.vector(res$real[ ,2]), 4),
      is_equivalent_to(c(26.5041, 0.0954, 0.0954, 0.0470, 0.3265,
      0.1488, 0.0704, 0.0704, 0.0704, 0.0954, 0.1769)))
  expect_that(round(as.vector(resM$real[1, ]), 3),
      is_equivalent_to(c(28.051, 26.632, 32.652)))
} )

test_that("closedCapMtcov gives right answers",  {
  data(KanhaTigers)
  # Check dots passed to nlm
  expect_warning(closedCapMtcov(KanhaTigers, iterlim=4),
      "Convergence may not have been reached")
  res0 <- closedCapMtcov(KanhaTigers)
  expect_that(class(res0), equals(c("wiqid", "list")))
  expect_that(names(res0), equals(c("call", "beta", "beta.vcv", "real", "logLik")))
  expect_that(colnames(res0$real), equals(c("est", "lowCI", "uppCI")))
  expect_that(rownames(res0$real), equals(c("Nhat", "p1", "p2", "p3", "p4",
      "p5", "p6", "p7", "p8", "p9", "p10")))
  expect_that(round(as.vector(res0$real[1, ]), 3),
      is_equivalent_to(c(28.446, 26.429, 39.953)))
  expect_that(round(as.vector(res0$real[-1, ]), 4),
      is_equivalent_to(rep(c(0.2039, 0.1536, 0.2655), each=10)))
  expect_that(round(AIC(res0), 4), equals(155.2433))
      # Same as M0 model.
  # Fictitious covars:
  set.seed(123) # NB new RNG for sample from R 3.6.0
  covars <- data.frame(Temp = runif(ncol(KanhaTigers), 15, 25),
      Cloud = sample(0:8, ncol(KanhaTigers), replace=TRUE))
  # Check dots passed to nlm
  expect_warning(closedCapMtcov(KanhaTigers, p~Cloud, data=covars, iterlim=4),
      "Convergence may not have been reached")
  resC <- closedCapMtcov(KanhaTigers, p~Cloud, data=covars)
  expect_that(round(as.vector(resC$real[1, ]), 2),
      is_equivalent_to(c(28.39, 26.41, 39.92)))
  expect_that(round(as.vector(resC$real[-1, ]), 4),
      # is_equivalent_to(c(  ## these values are for old RNG
     # 0.2147,0.2029,0.2088,0.2058,0.1916,0.2147,0.1972,0.1916,0.1972,0.2147,
     # 0.1438,0.1526,0.1532,0.1545,0.1212,0.1438,0.1398,0.1212,0.1398,0.1438,
     # 0.3080,0.2647,0.2778,0.2687,0.2893,0.3080,0.2706,0.2893,0.2706,0.3080)))
      is_equivalent_to(c(
      0.2430, 0.2095, 0.1658, 0.2258, 0.2610, 0.1658, 0.1658, 0.1658, 0.2610,
      0.1795, 0.1725, 0.1578, 0.1100, 0.1672, 0.1750, 0.1100, 0.1100, 0.1100,
      0.1750, 0.1274, 0.3307, 0.2725, 0.2422, 0.2976, 0.3704, 0.2422, 0.2422,
      0.2422, 0.3704, 0.2468)))
  expect_that(round(AIC(resC), 4), equals(154.6523))

  resTC <- closedCapMtcov(KanhaTigers, p~Temp+Cloud, data=covars)
  expect_that(round(as.vector(resTC$real[1, ]), 3),
      is_equivalent_to(c(28.353, 26.398, 39.898)))
  expect_that(round(as.vector(resTC$real[-1, ]), 4),
      # is_equivalent_to(c(
     # 0.1905,0.2793,0.1798,0.3715,0.2116,0.1142,0.1227,0.1924,0.1291,0.2638,
     # 0.1227,0.2022,0.1265,0.2431,0.1331,0.0588,0.0732,0.1202,0.0790,0.1757,
     # 0.2837,0.3722,0.2492,0.5210,0.3195,0.2103,0.1983,0.2934,0.2037,0.3759)))
      is_equivalent_to(c(
     0.1910, 0.2335, 0.1556, 0.2600, 0.2965, 0.1218, 0.1683, 0.2120, 0.2371, 0.1700,
     0.1117, 0.1704, 0.1010, 0.1824, 0.1927, 0.0631, 0.1114, 0.1247, 0.1522, 0.1185,
     0.3071, 0.3112, 0.2322, 0.3561, 0.4266, 0.2222, 0.2460, 0.3370, 0.3498, 0.2379)))
  # expect_that(round(AIC(resTC), 4), equals(149.4179))
  expect_that(round(AIC(resTC), 4), equals(154.7872))
} )

