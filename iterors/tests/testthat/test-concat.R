test_that("concatenate vector", {

  for (i in 0:38) {
    expected <- as.numeric(do.call("c", lapply(seq_len(i), seq_len)))
    it <- i_apply(icount(i), seq_len) # [1], [1, 2], [1, 2, 3], ...
    actual <- concat(it, length.out=75, mode="numeric")
    rest <- concat(it, "numeric")
    expect_equal(expected, c(actual, rest))
  }

  for (i in 1:10) {
    expected <- as.numeric(do.call("c", lapply(seq_len(i), seq_len)))
    it <- i_apply(icount(i), seq_len) # [1], [1, 2], [1, 2, 3], ...
    actual <- concat(it, n = i, mode="numeric")
    expect_equal(expected, actual)
  }

  expect_equal(concat(rep(NULL, 10)),list())

})
