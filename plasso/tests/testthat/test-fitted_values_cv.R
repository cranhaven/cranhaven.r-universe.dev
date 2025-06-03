test_that("fitted_values_cv handles normal case", {
  
  n = 100
  n_pred = 10
  p = 5
  X = matrix(runif(n*p,-pi,pi),ncol=p)
  colnames(X) = sprintf("var%s",seq(1:p))
  X = add_intercept(X)
  nm_act = c("(Intercept)","var1","var2")
  X_pred = matrix(runif(n_pred*p,-pi,pi),ncol=p)
  colnames(X_pred) = sprintf("var%s",seq(1:p))
  X_pred = add_intercept(X_pred)
  y = runif(n)
  XtX_all = crossprod(X)
  Xty_all = crossprod(X,y)
  
  result = as.vector(fitted_values_cv(XtX_all, Xty_all, X_pred[,nm_act], nm_act))
  
  mod = stats::lm(y ~ var1 + var2, data.frame(cbind(y,X[,nm_act])))
  pred_lm = predict(mod, newdata=data.frame(X_pred[,nm_act]))
  names(pred_lm) = NULL
  
  expect_equal(result, pred_lm)
  
})


test_that("fitted_values_cv with weighted matrices", {
  
  n = 100
  n_pred = 10
  p = 5
  X = matrix(runif(n*p,-pi,pi),ncol=p)
  colnames(X) = sprintf("var%s",seq(1:p))
  X = add_intercept(X)
  nm_act = c("(Intercept)","var1","var2")
  X_pred = matrix(runif(n_pred*p,-pi,pi),ncol=p)
  colnames(X_pred) = sprintf("var%s",seq(1:p))
  X_pred = add_intercept(X_pred)
  w = runif(n)
  w = handle_weights(w,nrow(X))
  y = runif(n)
  
  X_w = apply(X,2,`*`,sqrt(w))
  y_w = y * sqrt(w)
  XtX_all = crossprod(X_w)
  Xty_all = crossprod(X_w,y_w)
  
  result = as.vector(fitted_values_cv(XtX_all, Xty_all, X_pred[,nm_act], nm_act))
  
  mod = stats::lm(y ~ var1 + var2, weights = w, data.frame(cbind(y,X[,nm_act],w)))
  pred_lm = predict(mod, newdata=data.frame(X_pred[,nm_act]))
  names(pred_lm) = NULL
  
  expect_equal(result, pred_lm)
  
})