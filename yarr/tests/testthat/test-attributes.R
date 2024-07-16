test_that("a single attribute may be numeric", {
  filename <- tempfile()
  write("
@relation test
@attribute Test numeric
@data
{}
{0 -1}
{0 1}", filename)
  ds <- read.arff(filename)

  expect_true(ds[1,1] == 0)
  expect_true(ds[2,1] == -1)
  expect_true(ds[3,1] == 1)
})
test_that("attribute types are respected", {
  filename <- tempfile()
  write("
@relation test
@attribute V0 numeric
@attribute V1 string
@attribute V2 {0,1,2}
@attribute V3 integer
@attribute V4 real
@data
 % comment

1,1,1,1,1
0,0,0,1,1", filename)
  ds <- read.arff(filename)

  expect_is(ds[,1], "numeric")
  expect_is(ds[,2], "character")
  expect_is(ds[,3], "factor")
  expect_equivalent(levels(ds[,3]), c("0", "1", "2"))
  expect_is(ds[,4], "numeric")
  expect_is(ds[,5], "numeric")
})
test_that("attribute names are respected", {
  filename <- tempfile()
  write("
@relation test
@attribute test numeric
@attribute 'Var 1' string
@attribute \"A \\\"test\\\"\" {0,1,2}
@attribute 'yarr\\'s hell' integer
@data
1,1,1,1
0,0,0,1", filename)
  ds <- read.arff(filename)
  names <- attr.names(ds)

  expect_equal(names[1], "test")
  expect_equal(names[2], "Var 1")
  expect_equal(names[3], "A \"test\"")
  expect_equal(names[4], "yarr's hell")

  fn2 <- tempfile()
  write.arff(ds, file = fn2)
  expect_equal(grep("\"test\"", readLines(file(fn2, "r")), fixed = T), 4)
  expect_equal(grep("\\'s", readLines(file(fn2, "r")), fixed = T), 5)
})
test_that("factors are converted into factor attributes", {
  expect_equivalent(compute_types.default(data.frame(factor(c("a", "a")))), "{a}")
})
