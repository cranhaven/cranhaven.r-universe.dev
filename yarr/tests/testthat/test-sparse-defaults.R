test_that("factors can have a zero", {
  filename <- tempfile()
  write("
@relation test
@attribute Test {-1,0,1}
@data

{}
{0 0}
{0 1}", filename)
  ds <- read.arff(filename)

  expect_true(ds[1,1] == "-1")
  expect_true(ds[2,1] == "0")
  expect_true(ds[3,1] == "1")
})
