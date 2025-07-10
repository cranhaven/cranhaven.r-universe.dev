suppressMessages(library(rENA, quietly = T, verbose = F))
context("Test group methods");

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

test_that("group works on regular set", {
  grps <- ena.group(set_end, set_end$meta.data$Condition)
  testthat::expect_equal(as.character(grps$names), c("FirstGame", "SecondGame"))
  testthat::expect_equal(ncol(grps$points), ncol(as.matrix(set_end$points))+1)
  testthat::expect_equal(ncol(grps$line.weights), ncol(as.matrix(set_end$line.weights))+1)
})

test_that("group works on old R6 set", {
  df.accum <- suppressWarnings(
    rENA:::ena.accumulate.data.file(
      RS.data, units.by = c("UserName", "Condition"),
      conversations.by = c("ActivityNumber", "GroupName"),
      codes = codes, as.list = FALSE
    )
  )
  df.set <- suppressWarnings(
    ena.make.set(df.accum, as.list = FALSE)
  )

  grps <- ena.group(df.set, by = df.set$enadata$metadata$Condition)
  testthat::expect_equal(ncol(grps$points), ncol(as.matrix(df.set$points.rotated))+1)
  testthat::expect_equal(ncol(grps$line.weights), ncol(as.matrix(df.set$line.weights))+1)
})

test_that("group works different by logical values", {
  grps <- ena.group(set_end, set_end$meta.data$Condition == "FirstGame", names = "FirstGame")
  testthat::expect_equal(ncol(grps$points), ncol(as.matrix(set_end$points))+1)
  testthat::expect_equal(ncol(grps$line.weights), ncol(as.matrix(set_end$line.weights))+1)
})

test_that("group works using column name", {
  grps <- ena.group(set_end, "Condition")
  testthat::expect_equal(ncol(grps$points), ncol(as.matrix(set_end$points))+1)
  testthat::expect_equal(ncol(grps$line.weights), ncol(as.matrix(set_end$line.weights))+1)
})

test_that("group using string for function name", {
  grps <- ena.group(set_end, set_end$meta.data$Condition, method = "mean")
  testthat::expect_equal(ncol(grps$points), ncol(as.matrix(set_end$points))+1)
  testthat::expect_equal(ncol(grps$line.weights), ncol(as.matrix(set_end$line.weights))+1)
})

test_that("group just a table", {
  grpd_pts <- ena.group(set_end$points, set_end$meta.data$Condition)
  testthat::expect_equal(ncol(grpd_pts), ncol(as.matrix(set_end$points))+1)
})

test_that("verify values", {
  grpd_pts <- ena.group(set_end$points, set_end$meta.data$Condition)
  reg_mean <- as.numeric(colMeans(as.matrix(set_end$points$Condition$FirstGame)))

  gprd_means <- as.numeric(grpd_pts[grpd_pts$ENA_GROUP_NAME == "FirstGame", find_dimension_cols(grpd_pts), with = F])
  testthat::expect_equal(reg_mean, gprd_means)
})
