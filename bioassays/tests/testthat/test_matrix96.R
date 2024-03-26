context("matrix96")

## loading data
data(rawdata96, metafile96, metafile384)
rawdata<- data2plateformat(rawdata96, platetype = 96)
rawdata<- plate2df(rawdata)

## eg:1 rawdata to matrix format (column: value)
eg1<-matrix96(rawdata,"value")
test_that("eg1 matrix96 is working",{
  expect_that(class(eg1), equals(c("matrix","array")))
  expect_that(dim(eg1), equals(c(8, 12)))
  expect_that(colnames(eg1), equals(c("1", "2", "3", "4", "5", "6","7","8","9","10","11","12")))
  expect_that(rownames(eg1), equals(c("A", "B", "C", "D", "E", "F","G","H")))
})

## eg:2 metafile96 to matrix format (column: id)
eg2<-matrix96(metafile96,"id")
test_that("eg2 matrix96 is working",{
  expect_that(class(eg2), equals(c("matrix","array")))
  expect_that(dim(eg2), equals(c(8, 12)))
  expect_that(colnames(eg2), equals(c("1", "2", "3", "4", "5", "6","7","8","9","10","11","12")))
  expect_that(rownames(eg2), equals(c("A", "B", "C", "D", "E", "F","G","H")))
})

## eg:3 metafile384 to matrix format (column: cell)
eg3<-matrix96(metafile384,"cell")
test_that("eg3 matrix96 is working",{
  expect_that(class(eg3), equals(c("matrix","array")))
  expect_that(dim(eg3), equals(c(16, 24)))
  expect_that(colnames(eg3), equals(c("1", "2", "3", "4", "5", "6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24")))
  expect_that(rownames(eg3), equals(c("A", "B", "C", "D", "E", "F","G","H","I","J","K","L","M","N","O","P")))
})
