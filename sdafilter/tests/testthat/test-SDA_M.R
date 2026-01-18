test_that("SDA_M returns indices of rejected hypotheses", {

  n = 50
  p = 100
  dat = matrix(rnorm(n*p), nrow=n)
  mu = rep(0, p)
  mu[1:as.integer(0.1*p)]=0.5
  dat = dat+rep(1, n)%*%t(mu)
  alpha = 0.2
  out = SDA_M(dat, alpha, nonsparse = TRUE)

  if(is.character(out)){
    expect_match(out, 'no rejection')
  }else{
    a_in_b = all(out %in% 1:p)
    expect_equal(a_in_b, TRUE)
  }

})
