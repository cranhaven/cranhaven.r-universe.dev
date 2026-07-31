estimate<-EstMLEGHGBB(Chromosome_data$No.of.Asso,Chromosome_data$fre,0.1,0.5,0.4)
context_start_file("Checking outputs EstMLEGHGBB")
test_that("estimate method",{
  expect_identical(estimate@method,"BFGS")
})
test_that("estimate method",{
  expect_identical(estimate@optimizer,"optim")
})
test_that("minimized negative ll value",{
  expect_identical(round(estimate@min,2),436.81)
})
test_that("Checking class of output",{
  expect_s4_class(estimate,"mle2")
})
test_that("outputs",{
  expect_type(estimate@coef[1],"double")
})
