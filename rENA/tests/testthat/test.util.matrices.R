suppressMessages(library(rENA, quietly = T, verbose = F))
context("Test util matrix methods");

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

test_that("Verify line.weights matrices", {
  lws <- as.matrix(set_end$line.weights[1,], square = T)
  testthat::expect_equal(length(lws), 1)
  lw_one <- lws[[1]]
  testthat::expect_equal(nrow(lw_one), ncol(lw_one))
  testthat::expect_equal(colnames(lw_one), set_end$rotation$codes)
})

test_that("Verify ena.connection matrices", {
  conn_one <- as.matrix(set_end$connection.counts[1,], square = T)
  testthat::expect_equal(nrow(conn_one), ncol(conn_one))
  testthat::expect_equal(colnames(conn_one), set_end$rotation$codes)
  testthat::expect_is(conn_one, "matrix")

  conn_list <- as.matrix(set_end$connection.counts[1,], square = T, simplify = F)
  testthat::expect_is(conn_list, "list")
  testthat::expect_equal(length(conn_list), 1)
  testthat::expect_equal(nrow(conn_list[[1]]), ncol(conn_list[[1]]))
  testthat::expect_equal(nrow(conn_list[[1]]), length(set_end$rotation$codes))
})

# test_that("Verify ena.connection line", {
#   conn_one <- as.matrix(set_end$connection.counts[1,], square = T)
# })
