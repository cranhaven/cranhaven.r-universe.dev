context("Relief")
test_that("Is discrete should return true", {
  expect_equal(is.discrete(BOD$Time), TRUE)
  expect_equal(is.discrete(ChickWeight$Chick), TRUE)
  expect_equal(is.discrete(Seatbelts[,7]), TRUE) #VanKilled
  
})

test_that("Returns values between 0 and 1", {
  result <- relief()(iris,'Species',c('Sepal.Length'))
  expect_gte(result, 0)
  expect_lte(result, 1)
})
test_that("Names are set", {
  expect_equal(attr(relief(),'name'),"Relief");
  expect_equal(attr(relief(),'shortName'),"relief");
})