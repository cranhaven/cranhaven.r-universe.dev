suppressMessages(library(rENA, quietly = T, verbose = F))
context("Test making sets 1");

data(RS.data)
codenames <- c("Data", "Technical.Constraints", "Performance.Parameters",
  "Client.and.Consultant.Requests", "Design.Reasoning", "Collaboration");

test_that("Test rotation matrix properties", {
  accum <- ena.accumulate.data.file(
    RS.data, units.by = c("UserName", "Condition"),
    conversations.by = c("ActivityNumber", "GroupName"),
    codes = codenames
  );
  set <- ena.make.set(accum)

  testthat::expect_equal(as.character(set$rotation.matrix[[1]]), colnames(as.matrix(set$connection.counts)))
  testthat::expect_equal(as.character(set$rotation$rotation.matrix[[1]]), colnames(as.matrix(set$connection.counts)))
})
