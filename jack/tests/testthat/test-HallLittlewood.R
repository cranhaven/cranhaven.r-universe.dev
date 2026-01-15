test_that("Hall-Littlewood P (2,2,1)", {
  hlp <- HallLittlewoodPol(5, c(2, 2, 1), "P")
  t <- qlone(1)
  M221 <- as(MSFpoly(5, c(2, 2, 1)), "symbolicQspray")
  M2111 <- as(MSFpoly(5, c(2, 1, 1, 1)), "symbolicQspray")
  M11111 <- as(MSFpoly(5, c(1, 1, 1, 1, 1)), "symbolicQspray")
  expected <- M221 + (2-t-t^2)*M2111 + (5-4*t-4*t^2+t^3+t^4+t^5)*M11111
  expect_true(hlp == expected)
})

test_that("Hall-Littlewood Q", {
  HLQ2 <- HallLittlewoodPol(4, c(2), "Q")
  HLQ22 <- HallLittlewoodPol(4, c(2, 2), "Q")
  HLQ31 <- HallLittlewoodPol(4, c(3, 1), "Q")
  HLQ4 <- HallLittlewoodPol(4, c(4), "Q")
  t <- qlone(1)
  expected <- HLQ22 + (1-t)*HLQ31 + (1-t)*HLQ4
  expect_true(HLQ2^2 == expected)
})
