test_that("handle_weights handles missing weights", {
  
  n = 5
  result = handle_weights(NULL, n)

  expect_identical(result, matrix(rep(1, n),nrow=n,ncol=1,dimnames=list(NULL,"w")))
})


test_that("handle_weights handles provided (normalized) weights", {
  
  w = c(1.5, 0.9, 0.6)
  n = length(w)
  result = handle_weights(w, n)
  
  expect_equal(result, matrix(w,nrow=n,ncol=1,dimnames=list(NULL,"w")))
})


test_that("handle_weights handles (normalized) matrix weights", {

  w = matrix(c(0.5, 2, 0.5),nrow=3,ncol=1)
  n = nrow(w)
  result = handle_weights(w, n)
  colnames(w) = "w"
  
  expect_identical(result, w)
})


test_that("handle_weights normalizes weights", {

  w = c(2, 3, 5)
  n = length(w)
  
  result = handle_weights(w, n)
  w_new = w / sum(w) * length(w)
  
  expect_identical(result, matrix(w_new, nrow=n,ncol=1,dimnames=list(NULL,"w")))
})