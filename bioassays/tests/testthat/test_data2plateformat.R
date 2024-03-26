context("data2plateformat")

## loading data
data(rawdata24,rawdata96,rawdata384)

## eg:1 spectrophotometer reading from 24 well plate
eg1<-data2plateformat(rawdata24, platetype = 24)
test_that("eg1 data2plateformat are working",{
  expect_that(class(eg1), equals("data.frame"))
  expect_that(rownames(eg1), equals(c("A", "B", "C", "D")))
  expect_that(colnames(eg1), equals(c("1", "2", "3", "4", "5", "6")))
})

## eg:2 spectrophotometer reading from 96 well plate
eg2<-data2plateformat(rawdata96, platetype = 96)
test_that("eg2 data2plateformat are working",{
  expect_that(class(eg2), equals("data.frame"))
  expect_that(rownames(eg2), equals(c("A", "B", "C", "D","E","F","G","H")))
  expect_that(colnames(eg2), equals(c("1","2","3","4","5","6","7","8","9","10","11","12")))
})

## eg:3 spectrophotometer reading from 384 well plate
eg3<-data2plateformat(rawdata384, platetype = 384)
test_that("eg3 data2plateformat are working",{
  expect_that(class(eg3), equals("data.frame"))
  expect_that(rownames(eg3), equals(c("A", "B", "C", "D","E","F","G","H","I","J","K","L","M","N","O","P")))
  expect_that(colnames(eg3), equals(c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24")))
})
