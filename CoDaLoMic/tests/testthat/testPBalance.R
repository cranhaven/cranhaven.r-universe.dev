#context("PBalance")

especie1=cbind(c(0.5,0.3,0.1,0.1), c(0.1,0.3,0.6,0.1))
Num=c(1,2)
Dem=c(3,4)
A=2

fin=c(1.354025, -0.3465736)

test_that("PBalance", {
  expect_equal(PBalance(A,Num,Dem,especie1),fin,tolerance=1e-5)

})
