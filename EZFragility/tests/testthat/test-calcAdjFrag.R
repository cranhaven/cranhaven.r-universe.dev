nelec <- 10
ntime <- 100
set.seed(123)
ieegts <- matrix(rnorm(ntime * nelec, -10, 10), nrow = nelec)
frag <- NULL

test_that("calcAdjFrag", {
  frag <<- calcAdjFrag(ieegts, 20, 10, nSearch = 2) |> expect_no_error()
})

test_that("calcAdjFrag marginal cases", {
  calcAdjFrag(ieegts, window = 1, step = 10, nSearch = 2) |> expect_error()
  calcAdjFrag(ieegts, window = 2, step = 10, nSearch = 2) |> expect_no_error()
})

test_that("S4 methods", {
  expect_s4_class(frag, "Fragility")
  print(frag) |> capture.output() |> expect_no_error()
  frag |> is("Fragility") |> expect_equal(TRUE)
})
