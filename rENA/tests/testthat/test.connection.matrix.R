suppressMessages(library(rENA, quietly = T, verbose = F))
context("Test connection matrices");

library(magrittr)

data(RS.data)
units <- c("Condition", "UserName")
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

test_that("return square matrix", {
  connections <- connection.matrix(set_end)

  testthat::expect_equal(ncol(connections[[1]]), nrow(connections[[1]]))
})

test_that("return all units", {
  connections <- connection.matrix(set_end)
  testthat::expect_equal(length(connections), length(set_end$model$unit.labels))
  testthat::expect_equal(names(connections), set_end$model$unit.labels)
})

test_that("return single unit", {
  connections <- connection.matrix(set_end$connection.counts$ENA_UNIT$`FirstGame.steven z`)

  testthat::expect_is(connections, "matrix")
  testthat::expect_equal(nrow(connections), ncol(connections))
})
test_that("stop on non-connections", {
  testthat::expect_error(connection.matrix(1))
})

test_that("test additional metadata", {
  meta <- colnames(RS.data)[3:8]
  set_end <- RS.data %>%
    ena(
      units = units,
      conversation = conversation,
      metadata = meta,
      codes = codes,
      window.size.back = 4,
    )

  testthat::expect_equal(c("ENA_UNIT",units, meta), colnames(set_end$meta.data))
})
