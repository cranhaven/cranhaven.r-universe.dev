dat <- structure(c(7L, 5L, 8L, 0L), .Dim = c(2L, 2L))

class(dat) <- "table"
#Parameters used in evaluation (eta)
hyp <- "x[1,1]=x[2,1]&x[1,2]>x[2,2];
x[1,1]>x[2,1]&x[1,2]>x[2,2]"
#Number of equality and inequality constraints per model
# gorica:::parse_hypothesis(varnames = gorica:::rename_table_est(c("x[1,1]", "x[2,1]", "x[1,2]", "x[2,2]")), gorica:::rename_table_est(hyp))

res <- gorica(dat, hyp)

test_that("contingency table supp 1 works best", {
  expect_equivalent(res$fit$gorica_weights,
                    c(0.5013348, 0.3666024, 0.1320628)
                    ,
                    tolerance = .03)
})
