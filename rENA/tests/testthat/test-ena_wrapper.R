suppressMessages(library(rENA, quietly = T, verbose = F))
context("Test util methods");

library(magrittr)

data(RS.data)
units <- c("UserName", "Condition")
conversation <- c("GroupName")
codes <- c("Data", "Technical.Constraints", "Performance.Parameters",
            "Client.and.Consultant.Requests", "Design.Reasoning",
            "Collaboration")

test_that("Verify group with single unit", {
  test_data <- data.table::data.table(RS.data)
  test_data[UserName == "carl b", GroupName := "MeOnly"]

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
