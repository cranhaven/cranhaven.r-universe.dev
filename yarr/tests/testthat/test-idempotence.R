test_that("read.write = 1 & write.read.write = write", {
  filename <- tempfile()
  write.arff(iris, "iris", file = filename)
  ir2 <- read.arff(filename)
  expect_true(all(ir2 == iris))

  fn2 <- tempfile()
  write.arff(ir2, "iris", file = fn2)
  expect_true(all(readLines(file(filename, "r")) == readLines(file(fn2, "r"))))
})
test_that("read.write = 1 & write.read.write = write (sparse)", {
  filename <- tempfile()
  write.arff(iris, "iris", file = filename, sparse = TRUE)
  ir2 <- read.arff(filename)
  expect_true(all(ir2 == iris))

  fn2 <- tempfile()
  write.arff(ir2, "iris", file = fn2, sparse = TRUE)
  expect_true(all(readLines(file(filename, "r")) == readLines(file(fn2, "r"))))
})
