context("Discrete functions")

test_that("Strings that are not numbers are considered discrete", {
  stringCollection <- c("0", "AB", "A",  "A",  "A",  "A",  "0",  "B",  "0",  "A",
                        "A", "A",  "0",  "AB", "AB", "B",  "A",  "A",  "0",  "A",
                        "AB", "A",  "0",  "A", "0", "AB", "A",  "A",  "AB", "0",
                        "0",  "B",  "0",  "AB", "AB", "B",  "B",  "B",  "B",  "0")
  expect_equal(is.discrete(stringCollection), TRUE)
})

test_that("Integer collections with few values are considered discrete", {
  integerCollection <- rep(seq(1,4),40)
  expect_equal(is.discrete(integerCollection), TRUE)
})

test_that("Stringified numbers are treated as integers", {
  stringifiedNumberscollection <- rep(c('1','2','3','4'),40)
  expect_equal(is.discrete(stringifiedNumberscollection), TRUE)
})

test_that("Numeric integer numbers are treated as integers", {
  numericIntegerscollection <- rep(c(1.0,2.0,3.0,4.0),40)
  expect_equal(is.discrete(numericIntegerscollection), TRUE)
})

test_that("Small integer collections are considered discrete", {
  smallNumericCollection <- seq(1,15)
  expect_equal(is.discrete(smallNumericCollection), TRUE)
})

test_that("Some dataset columns are considered discrete", {
  expect_equal(is.discrete(BOD$Time), TRUE)
  expect_equal(is.discrete(ChickWeight$Chick), TRUE)
  expect_equal(is.discrete(Seatbelts[,7]), TRUE) #VanKilled
})

test_that("Integer collections are not considered discrete if there are many values", {
  integerCollection <- seq(1,50)
  expect_equal(is.discrete(integerCollection), FALSE)
})

test_that("Short numeric collections are not considered discrete if there are few values", {
  longNumericCollection <- rep(c(1.5,2.5),10)
  expect_equal(is.discrete(longNumericCollection), FALSE)
})

test_that("Long numeric collections are not considered discrete if there are many values", {
  numericIntegerscollection <- rep(c(1.2,2.0,3.9,4.0,5.8,6.0,7.7,8.0,9.6,10.5,11.4,12.3,13.0,14.2,15.1),4)
  expect_equal(is.discrete(numericIntegerscollection), FALSE)
})

test_that("Some dataset columns are not considered discrete", {
  expect_equal(is.discrete(BOD$demand), FALSE)
  expect_equal(is.discrete(ChickWeight$weight), FALSE)
  expect_equal(is.discrete(Seatbelts[,6]), FALSE) #PetrolPrice
})

test_that("isDataframeDiscrete recognises discrete data sets", {
  streetNumber <- rep(seq(1,10),4)
  doorLetter <- rep(c("A","B","C"),40)
  dataset <- data.frame(streetNumber, doorLetter)
  expect_equal(isDataframeDiscrete(dataset), TRUE)
})

test_that("isDataframeDiscrete recognises non discrete data sets", {
  expect_equal(isDataframeDiscrete(mtcars), FALSE)
  expect_equal(isDataframeDiscrete(iris), FALSE)
  expect_equal(isDataframeDiscrete(ChickWeight), FALSE)
})