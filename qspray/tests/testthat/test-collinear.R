test_that("Collinear qsprays", {
  qspray1 <- qsprayMaker(string = "1/2 x^(1, 1) + 4 x^(0, 2) + 5")
  qspray2 <- "4/7" * qsprayMaker(string = "10 + x^(1, 1) + 8 x^(0, 2)")
  expect_true(collinearQsprays(qspray1, qspray2))
})