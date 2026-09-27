test_that("Iteror from Python object", {
  skip_if_no_python()

  l <- reticulate::r_to_py(list(1, 2, 3))
  Robj <- iteror(l)
  lR <- as.list(Robj)
  expect_equal(lapply(lR, reticulate::py_to_r), list(1, 2, 3))

})

test_that("Python object from iteror", {
  skip_if_no_python()

  pit <- reticulate::r_to_py(iseq(2, 11, 3))
  expect_equal(reticulate::iter_next(pit), 2)
  expect_equal(reticulate::iter_next(pit), 5)
  expect_equal(reticulate::iter_next(pit), 8)
  expect_equal(reticulate::iter_next(pit), 11)
  expect_equal(reticulate::iter_next(pit, "what"), "what")

})

test_that("round trip both ways", {
  skip_if_no_python()

  rit <- iseq(2, 11, 3)
  pit <- reticulate::r_to_py(rit)
  rit2 <- reticulate::py_to_r(pit)
  expect_equal(as.numeric(iteror(rit2)),
               c(2, 5, 8, 11))

  builtins <- reticulate::import_builtins()
  pit <- reticulate::py_eval("range(2, 11, 3)")
  rit <- iteror(pit)
  pit2 <- reticulate::r_to_py(rit)
  # note py has a different idea about the upper limit
  expect_equal(
    builtins$list(pit2),
    c(2, 5, 8))

})
