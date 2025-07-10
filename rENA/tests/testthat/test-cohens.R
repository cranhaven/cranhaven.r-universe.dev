suppressMessages(library(rENA, quietly = T, verbose = F))
context("Test util methods");

library(magrittr)

codenames <- c("Data", "Technical.Constraints", "Performance.Parameters",
  "Client.and.Consultant.Requests", "Design.Reasoning", "Collaboration");

test_that("verify cohens", {
  data(RS.data)
  accum <- ena.accumulate.data.file(
    RS.data, units.by = c("UserName", "Condition"),
    conversations.by = c("ActivityNumber", "GroupName"),
    codes = codenames
  );
  set <- ena.make.set(accum)
  d <- fun_cohens.d(as.matrix(set$points$Condition$FirstGame)[,1], as.matrix(set$points$Condition$SecondGame)[,1])
  testthat::expect_equal(d, 0.1142617)

  testthat::expect_equal(fun_cohens.d(1:10, 1:10), 0)
})
