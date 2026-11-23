#context("Percen_Variance")

matt=matrix(c(1:4),2,2)

fin=c(50,50)

test_that("Percen_Variance", {
  expect_equal(Percen_Variance(matt),fin,tolerance=1e-5)

})
