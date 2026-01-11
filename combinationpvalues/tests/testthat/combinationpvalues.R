library(testthat)        # load testthat package
library(combinationpvalues)       # load our package
library(spatstat.utils)

# Test whether the output is a data frame
test_that("test whether InfinitePs returns a list", {
  output<- InfinitePs(0.2)
  expect_type(output, "list")
})

# Test whether InfinitePs returns length >= 2
test_that("test whether InfinitePs returns length >= 2", {
  output<- InfinitePs(0.2,0.1,0.9)
  expect_gte(length(output), 2)
})

#expect_condition function
#created a function to check whether list of values are within a given range
expect_condition<- function(object) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")

  # 2. Call expect()
  rr <- c(0, 1)
  act$val <-lapply(act$val, as.numeric)
  #print(act)
  for (x in act$val) {
    act$range <-inside.range(x, rr)
    #print(act)
  }
  expect(
    act$range,
    sprintf("%s has range %e,%f, not range 0 to 1.", act$lab, min(unlist(act$val))[1],max(unlist(act$val))[1])
  )

  # 3. Invisibly return the value
  invisible(act$range)
}


# Test whether InfinitePs inputs are within range of 0 to 1 inclusive
test_that("test whether InfinitePs inputs are within range of 0 to 1 inclusive", {
  output<- InfinitePs(0.2,0.1,0.9)
  expect_condition(output)
})

## Test whether one of the six functions return one test statistic value only
test_that("Test whether one of the six functions return one p value only",
          {
            input <- InfinitePs(0.1,0.4,0.02)
            output <- StoufferMethod(input)
            expect_length(output, 1)
          })

## Test whether the final combined p value should return only one value
test_that("Test whether the final combined p value should return only one value",
          {
            input <- InfinitePs(0.016,0.067, 0.25,  0.405, 0.871)
            Fisher <- FishersMethod(input)
            output <- CombinedPValueMethod(Fisher,"Fisher") #return 0.014522139680083129
            #print(output)
            expect_length(output, 1)
          })
