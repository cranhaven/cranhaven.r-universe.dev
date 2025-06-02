mydata <- as.table(matrix(c(5, 0, 15, 7, 3, 8, 0, 6), nrow = 2, ncol = 4, byrow = TRUE))

res <- gorica(mydata, hypothesis = "a:=x[1,1]*x[2,2]-x[1,2]*x[2,1];
a > 0")
#Parameters used in evaluation (eta)
test_that("contingency table supp 4_1 throws error", {
  expect_equivalent(res$fit$gorica_weights, c(.622, .378), tolerance = .01)
})
# LogLikelihood Penalty GORICA_value GORICA_weight
# H1     0.1956781  0.5004    0.6094439     0.6223653
# H2     0.1956781  1.0000    1.6086439     0.3776347
#
# some estimated as 0
