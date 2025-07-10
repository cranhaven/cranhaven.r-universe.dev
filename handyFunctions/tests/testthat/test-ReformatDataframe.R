library(handyFunctions)
library(stringr)
library(stats)

data(people)
vector <- c(1, 2, 3, "", NA, "  ", "four", "NA", 5)

test_that("multiplication works", {
  expect_silent(modifyColNames(people,rawSep = "[.][.]"))
  expect_silent(checkCols(people, c("..name", "..sex")))
  expect_output(checkDtype(vector),regexp = NA)
  expect_output(modifyColTypes(people),regexp = NA)
  expect_silent(modifyRowNames(people))
  expect_output(unifyDataframe(people,rawColSep = "[.][.]"),regexp = NA)
  expect_silent(splitCol(people, col = 1, sep = " ", index = 2))
})
