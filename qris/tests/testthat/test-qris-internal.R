test_that("Test ghatC", {
  expect_equal(drop(ghatC(1:5, c(1, 1, 0, 0, 1), rep(1, 5))), c(4, 3, 3, 3, 0) / 5)
  expect_equal(drop(ghatC(1:5, c(1, 1, 0, 0, 1), 5:1)), 2 / c(3, 5, 5, 5, Inf))
})

test_that("Test Amat", {
  expect_equal(Amat(double(2), matrix(1:4, 2), 1:2 / 2, diag(2), 0:1, 1:2 / 10, .5),
               matrix(c(-0.3564682, -0.7129364, -0.7129364, -1.4258727), 2),
               tolerance = .01)
  expect_equal(Amat(double(2), matrix(1:4, 2), 1:2 / 2, diag(2), 0:1, 1:2 / 10, .2),
               matrix(c(-0.3564682, -0.7129364, -0.7129364, -1.4258727), 2),
               tolerance = .01)
  expect_equal(unique(c(Amat(double(2), matrix(1, 2, 2), 1:2 / 2, diag(2), 0:1, 1:2 / 10, .1))),
               -0.2792879, tolerance = .01)
  
  expect_equal(Amat(1:2, matrix(1:4, 2), 1:2 / 2, diag(2), 0:1, 1:2 / 10, .5),
               matrix(c(-0.03233806, -0.06467613, -0.06467613, -0.12935225), 2),
               tolerance = .01) 
  expect_equal(Amat(1:2, matrix(1:4, 2), 1:2 / 2, diag(2), 0:1, 1:2 / 10, .2),
               matrix(c(-0.03233806, -0.06467613, -0.06467613, -0.12935225), 2),
               tolerance = .01)
  expect_equal(unique(c(Amat(1:2, matrix(1, 2, 2), 1:2 / 2, diag(2), 0:1, 1:2 / 10, .1))),
               -0.03973543, tolerance = .01)  
})

test_that("Test isObj", {
  expect_equal(drop(isObj(double(2), matrix(1:4, 2), 1:2 / 2, diag(2), 0:1, 1:2 / 10, .5)),
               c(-0.03567059, -0.07134118), tolerance = .01)
  expect_equal(drop(isObj(double(2), matrix(1:4, 2), 1:2 / 2, diag(2), 0:1, 1:2 / 10, .1)),
               c(0.7643294, 1.5286588), tolerance = .01)  
  expect_equal(drop(isObj(1:2, matrix(1:4, 2), 1:2 / 2, diag(2), 0:1, 1:2 / 10, .5)),
               c(0.9715733, 1.9431466), tolerance = .01)
  expect_equal(drop(isObj(1:2, matrix(1:4, 2), 1:2 / 2, diag(2), 0:1, 1:2 / 10, .1)),  
               c(1.771573, 3.543147), tolerance = .01) 
})

test_that("Test rev_isObj", {
  expect_equal(drop(rev_isObj(double(2), matrix(1:4, 2), 1:2 / 2, diag(2), c(1, 1), 0:1, 1:2 / 10, .5)),
               c(-0.03567059, -0.07134118), tolerance = .01)
  expect_equal(drop(rev_isObj(double(2), matrix(1:4, 2), 1:2 / 2, diag(2), c(1, 1), 0:1, 1:2 / 10, .1)),
               c(0.7643294, 1.5286588), tolerance = .01)  
  expect_equal(drop(rev_isObj(1:2, matrix(1:4, 2), 1:2 / 2, diag(2), c(1, 1), 0:1, 1:2 / 10, .5)),
               c(0.9715733, 1.9431466), tolerance = .01)
  expect_equal(drop(rev_isObj(1:2, matrix(1:4, 2), 1:2 / 2, diag(2), c(1, 1), 0:1, 1:2 / 10, .1)),  
               c(1.771573, 3.543147), tolerance = .01) 
})

test_that("Test isObjL", {
  expect_equal(drop(isObjL(double(2), matrix(1:4, 2), diag(2), 1:2 / 10)),
               c(0.4873864, 0.4821647), tolerance = .01)
  expect_equal(drop(isObjL(1:2, matrix(1:4, 2), diag(2), 1:2 / 10)),
               c(0.9854442, 0.9857866), tolerance = .01)
})
