mydata <- as.table(matrix(c(7, 0, 6, 3, 2, 3), nrow = 2, ncol = 3, byrow = TRUE))

hyp <- "a:=x[1,1]/(x[1,1]+x[2,1]);
b:=x[1,2]/(x[1,2]+x[2,2]);c:=x[1,3]/(x[1,3]+x[2,3]);a > b & b > c"

res <- gorica(mydata, hyp)
res$estimates

test_that("contingency table supp 3_1 works", {
  expect_equivalent(res$fit$gorica_weights,
                    c(0.0004775331, 0.9995224669),
                    tolerance = .03)
  })

