context("table_subsetting")

test_that("Remove rows from meta data", {
  data(RS.data)
  units <- c("UserName", "Condition")
  conversation <- c("GroupName")
  codes <- c("Data", "Technical.Constraints", "Performance.Parameters",
              "Client.and.Consultant.Requests", "Design.Reasoning",
              "Collaboration")

  set_end <- ena(
      RS.data,
      units = c(units, "GroupName"),
      conversation = conversation,
      codes = codes,
      window.size.back = 4
    )

  testthat::expect_true(all(sapply(set_end$meta.data, is, "ena.metadata")))
  testthat::expect_true(all(sapply(set_end$meta.data[-1, ], is, "ena.metadata")))
})
