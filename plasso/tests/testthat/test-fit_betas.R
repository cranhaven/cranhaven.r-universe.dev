test_that("fit_betas with some active variables", {
  
  n = 100
  n_pred = 10
  p = 5
  X = matrix(runif(n*p,-pi,pi),ncol=p)
  colnames(X) = sprintf("var%s",seq(1:p))
  X = add_intercept(X)
  nm_act = c("(Intercept)","var1","var4")
  w = runif(n)
  w = handle_weights(w,nrow(X))
  y = runif(n)

  coef_lasso = rep(0,p+1)
  names(coef_lasso) = colnames(X)
  
  result = fit_betas(X,y,w,nm_act,coef_lasso)
  
  mod = stats::lm(y ~ var1 + var4, weights = w, data.frame(cbind(y,X[,nm_act],w)))
  coef_lm = coef_lasso
  coef_lm[nm_act] = coef(mod)
  
  expect_equal(result, coef_lm)
  
})


test_that("fit_betas with only intercept", {
  
  n = 100
  n_pred = 10
  p = 5
  X = matrix(runif(n*p,-pi,pi),ncol=p)
  colnames(X) = sprintf("var%s",seq(1:p))
  X = add_intercept(X)
  nm_act = c("(Intercept)")
  w = runif(n)
  w = handle_weights(w,nrow(X))
  y = runif(n)
  
  coef_lasso = rep(0,p+1)
  names(coef_lasso) = colnames(X)
  
  result = fit_betas(X,y,w,nm_act,coef_lasso)
  
  mod = stats::lm(y ~ 1, weights = w, data.frame(cbind(y,X[,nm_act],w)))
  coef_lm = coef_lasso
  coef_lm[nm_act] = coef(mod)
  
  expect_equal(result, coef_lm)
  
})