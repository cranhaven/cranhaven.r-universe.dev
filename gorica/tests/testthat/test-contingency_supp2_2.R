dat <- structure(c(7L, 5L, 8L, 0L), .Dim = c(2L, 2L))

class(dat) <- "table"
#Parameters used in evaluation (eta)
hyp <- "a:=x[1,1]/c(x[1,1]+x[1,2]);
b:=x[1,2]/c(x[1,1]+x[1,2]);
a>b"
#Number of equality and inequality constraints per model
# gorica:::parse_hypothesis(varnames = gorica:::rename_table_est(c("x[1,1]", "x[2,1]", "x[1,2]", "x[2,2]")), gorica:::rename_table_est(hyp))

test_that("contingency table supp 2_2 throws error", {
  expect_error(gorica(dat, hyp))
})
