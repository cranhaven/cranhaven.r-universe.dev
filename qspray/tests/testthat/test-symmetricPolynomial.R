test_that("Symmetric polynomial", {
  e1 <- ESFpoly(4, 1)
  e2 <- ESFpoly(4, 2)
  e3 <- ESFpoly(4, 3)
  f <- e1 + 2*e2 + 3*e3
  expect_true(isSymmetricQspray(f))
  expect_false(isSymmetricQspray(qlone(1) - qlone(2)))
})

test_that("MSPcombination", {
  qspray <- PSFpoly(4, c(3, 1)) + ESFpoly(4, c(2, 2)) + 4L
  expect_no_error(MSPcombination(qspray, check = TRUE))
})

test_that("compactSymmetricQspray", {
  qspray <- PSFpoly(4, c(3, 1)) - ESFpoly(4, c(2, 2)) + 4L
  expect_identical(
    compactSymmetricQspray(qspray, check = TRUE),
    "M[4] + M[3, 1] - M[2, 2] - 2*M[2, 1, 1] - 6*M[1, 1, 1, 1] + 4"
  )
})

test_that("PSPexpression", {
  qspray <- PSFpoly(4, c(3, 1)) - ESFpoly(4, c(2, 2)) + 4L
  pspExpr <- PSPexpression(qspray)
  psPolys <- lapply(1:numberOfVariables(pspExpr), function(i) PSFpoly(4, i))
  obtained <- changeVariables(pspExpr, psPolys)
  expect_true(obtained == qspray)
})
