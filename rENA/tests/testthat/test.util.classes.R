suppressMessages(library(rENA, quietly = T, verbose = F))
context("Test util class methods");

library(magrittr)

data(RS.data)
units <- c("UserName", "Condition")
conversation <- c("ActivityNumber", "GroupName")
codes <- c("Data", "Technical.Constraints", "Performance.Parameters",
            "Client.and.Consultant.Requests", "Design.Reasoning",
            "Collaboration")

set_end <- RS.data %>%
  ena(
    units = units,
    conversation = conversation,
    codes = codes,
    window.size.back = 4
  )

# test_that("Verify square matrices", {
#
# })

test_that("Verify raw input", {
  testthat::expect_true(
    all(colnames(RS.data) %in% colnames(set_end$model$raw.input))
  )
})

test_that("Named centroid dimensions", {
  testthat::expect_equal(
    colnames(as.matrix(set_end$model$centroids)),
    colnames(as.matrix(set_end$points))
  )

  testthat::expect_equal(
    as.character(set_end$model$centroids$unit),
    set_end$model$unit.labels
  )
})


test_that("Named variance dimensions", {
  testthat::expect_equal(
    names(set_end$model$variance),
    colnames(set_end$rotation$rotation.matrix)[-1]
  )
})
