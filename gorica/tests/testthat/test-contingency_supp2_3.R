dat <- structure(c(7L, 5L, 8L, 0L), .Dim = c(2L, 2L))

class(dat) <- "table"
#Parameters used in evaluation (eta)
hyp <- "a:=x[1,1]/c(x[1,1]+x[1,2]);
a>.5"
#Number of equality and inequality constraints per model
# gorica:::parse_hypothesis(varnames = gorica:::rename_table_est(c("x[1,1]", "x[2,1]", "x[1,2]", "x[2,2]")), gorica:::rename_table_est(hyp))
res <- gorica(dat, hyp)
test_that("contingency table supp 2_3 works", {
  expect_equivalent(res$fit$gorica_weights, c(0.6146977, 0.3853023), tolerance = .03)
  })
