test_that("find_Xse_ind handles factor < 0", {
  CV = c(1.7, 1.36, 1.1, 1.03, 1.13, 1.2, 1.3, 1.7)
  ind_min = which.min(CV)
  oneSE = c(0.15, 0.2, 0.13, 0.22, 0.15, 0.07, 0.09, 0.13)
  factor = -1
  
  result = find_Xse_ind(CV, ind_min, oneSE, factor)
  
  expect_equal(result, 3)
})


test_that("find_Xse_ind handles large factor < 0", {
  CV = c(1.7, 1.36, 1.1, 1.03, 1.13, 1.2, 1.3, 1.7)
  ind_min = which.min(CV)
  oneSE = c(0.15, 0.2, 0.13, 0.22, 0.15, 0.07, 0.09, 0.13)
  factor = -2
  
  result = find_Xse_ind(CV, ind_min, oneSE, factor)
  
  expect_equal(result, 2)
})


test_that("find_Xse_ind handles factor > 0", {
  CV = c(1.7, 1.36, 1.1, 1.03, 1.13, 1.2, 1.3, 1.7)
  ind_min = which.min(CV)
  oneSE = c(0.15, 0.2, 0.13, 0.22, 0.15, 0.07, 0.09, 0.13)
  factor = 1
  
  result = find_Xse_ind(CV, ind_min, oneSE, factor)
  
  expect_equal(result, 6)
})


test_that("find_Xse_ind handles factor = 0", {
  CV = c(1.7, 1.36, 1.1, 1.03, 1.13, 1.2, 1.3, 1.7)
  ind_min = which.min(CV)
  oneSE = c(0.15, 0.2, 0.13, 0.22, 0.15, 0.07, 0.09, 0.13)
  factor = 0
  
  result = find_Xse_ind(CV, ind_min, oneSE, factor)
  
  expect_equal(result, ind_min)
})


test_that("find_Xse_ind handles ind_min = 1 for factor < 0", {
  CV = c(0.93, 1.03, 1.1, 1.11, 1.13, 1.2, 1.3, 1.7)
  ind_min = which.min(CV)
  oneSE = c(0.15, 0.2, 0.13, 0.22, 0.15, 0.07, 0.09, 0.13)
  factor = -1
  
  result = find_Xse_ind(CV, ind_min, oneSE, factor)
  
  expect_equal(result, 1)
})


test_that("find_Xse_ind handles ind_min = 1 for factor > 0", {
  CV = c(0.93, 1.03, 1.1, 1.11, 1.13, 1.2, 1.3, 1.7)
  ind_min = which.min(CV)
  oneSE = c(0.15, 0.2, 0.13, 0.22, 0.15, 0.07, 0.09, 0.13)
  factor = 1
  
  result = find_Xse_ind(CV, ind_min, oneSE, factor)
  
  expect_equal(result, 2)
})

test_that("find_Xse_ind handles ind_min = max for factor > 0", {
  CV = c(1.7, 1.66, 1.4, 1.1, 1.09, 1.03, 1.02, 1.01)
  ind_min = which.min(CV)
  oneSE = c(0.15, 0.2, 0.13, 0.22, 0.15, 0.07, 0.09, 0.13)
  factor = 1
  
  result = find_Xse_ind(CV, ind_min, oneSE, factor)
  
  expect_equal(result, length(CV))
})


test_that("find_Xse_ind handles ind_min = max for factor < 0", {
  CV = c(1.7, 1.66, 1.4, 1.1, 1.09, 1.03, 1.02, 1.01)
  ind_min = which.min(CV)
  oneSE = c(0.15, 0.2, 0.13, 0.22, 0.15, 0.07, 0.09, 0.13)
  factor = -1
  
  result = find_Xse_ind(CV, ind_min, oneSE, factor)
  
  expect_equal(result, 4)
})
