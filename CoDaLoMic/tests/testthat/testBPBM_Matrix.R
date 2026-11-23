#context("BPBM_Matrix")

matt=matrix(c(1:12),3,4)
rows.position=c(2,3)

fin=rbind(c(1,1,1,1), matt[2,],matt[3,])

test_that("Percen_Variance", {
  expect_equal(BPBM_Matrix(rows.position,matt,4),fin,tolerance=1e-5)

})
