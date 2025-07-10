library(stringr)
library(stats)
library(handyFunctions)

data(people)
data(grade)
test_that("mergeCustom works", {
  expect_silent(mergeCustom(people, grade, "..name", "name"))
  expect_silent(queryingInfo(grade, "name", "chinese", c("Ming Li", "Bang Wei")))
  expect_silent(matchIndex(grade[, "name"], c("Ming Li", "Bang Wei")))
})
