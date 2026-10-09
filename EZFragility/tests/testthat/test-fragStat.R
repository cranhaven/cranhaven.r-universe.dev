stat <- NULL
nelec <- 10
ntime <- 100
elecSoz <- c(1, 2, 3)
set.seed(123)
ieegts <- matrix(rnorm(ntime * nelec, -10, 10), nrow = nelec)
frag <- calcAdjFrag(ieegts, 20, 10, nSearch = 2)


test_that("fragStat", {
  skip_if(!is(frag, "Fragility"))
  stat <- fragStat(frag = frag, groupIndex = elecSoz) |> expect_no_error()
  expect_s4_class(stat, "FragStat")
  ## Test the show method
  print(stat) |>  capture.output() |> expect_no_error()
})
