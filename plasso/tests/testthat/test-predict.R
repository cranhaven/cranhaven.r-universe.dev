test_that("predict.plasso for all lambda values", {
  
  # dgp
  n = 100
  n_pred = 5
  n_var = 10
  X = matrix(runif(n*n_var,-pi,pi),ncol=n_var)
  X_pred = matrix(runif(n_pred*n_var,-pi,pi),ncol=n_var)
  y = runif(n)
  
  # fit model and extract predictions
  p = plasso::plasso(X,y)
  pred_p = predict(p,newx=X_pred)
  coef_p = predict(p,type="coefficients")
  
  # create dataframes for stats::lm model
  df = data.frame(cbind(y,X))
  colnames(df) = c("y",rownames(coef_p[["plasso"]])[2:(n_var+1)])
  df_new = as.data.frame(X_pred)
  colnames(df_new) = rownames(coef_p[["plasso"]])[2:(n_var+1)]
  
  # get active coefficients
  active_list = lapply(seq_len(ncol(coef_p[["plasso"]])), function(i) {
    rownames(coef_p[["plasso"]])[coef_p[["plasso"]][,i] != 0]
  })
  
  # get predictions from stats::lm
  predict_lm_models = vapply(active_list, function(r) {
    formula_str = paste("y ~", gsub(r"(Intercept)",1,paste(r, collapse = " + ")))
    formula = as.formula(formula_str)
    model = stats::lm(formula, data=df)
    pred_lm = predict(model, newdata=df_new)
    return(pred_lm)
  }, FUN.VALUE = numeric(n_pred))
  dimnames(predict_lm_models) = dimnames(pred_p[["plasso"]])
  
  # get coefficients from stats::lm
  coef_lm_models = vapply(active_list, function(r) {
    placeholder = rep(0,n_var+1)
    names(placeholder) = rownames(coef_p[["plasso"]])
    formula_str = paste("y ~", gsub(r"(Intercept)",1,paste(r, collapse = " + ")))
    formula = as.formula(formula_str)
    model = stats::lm(formula, data=df)
    coef_lm_init = coef(model)
    placeholder[r] = coef_lm_init
    return(placeholder)
  }, FUN.VALUE = numeric(n_var + 1))
  dimnames(coef_lm_models) = dimnames(coef_p[["plasso"]])
  
  # check fitted values for plasso
  expect_equal(pred_p[["plasso"]], predict_lm_models)
  
  # check coefficients for plasso
  expect_equal(as.matrix(coef_p[["plasso"]]),coef_lm_models)
  
  # check predictions for lasso
  pred_g = predict(p$lasso_full,newx=X_pred)
  coef_g = coef(p$lasso_full)
  expect_equal(pred_p[["lasso"]],pred_g)
  
  # check coefficients for lasso
  expect_identical(coef_p[["lasso"]][-1,],p$lasso_full$beta)
  expect_identical(coef_p[["lasso"]],coef_g)
  
  # check format of plasso fitted values
  expect_identical(dimnames(pred_p[["plasso"]]),dimnames(pred_g))
  
  # check format of plasso coefficients
  expect_identical(dimnames(coef_p[["plasso"]]),dimnames(coef_g))
  
})


test_that("predict.plasso response for single lambda value", {

  n = 50
  n_pred = 10
  n_var = 8
  X = matrix(runif(n*n_var,-pi,pi),ncol=n_var)
  X_pred = matrix(runif(n_pred*n_var,-pi,pi),ncol=n_var)
  y = runif(n)

  s = 0.01
  p = plasso::plasso(X,y)
  pred_p = predict(p,newx=X_pred,s=s)
  coef_p = predict(p,type="coefficients",s=s)

  df = data.frame(cbind(y,X))
  colnames(df) = c("y",rownames(coef_p[["plasso"]])[2:(n_var+1)])
  df_new = as.data.frame(X_pred)
  colnames(df_new) = rownames(coef_p[["plasso"]])[2:(n_var+1)]

  vars = rownames(coef_p[["plasso"]])[as.vector(coef_p[["plasso"]] != 0)]
  formula = as.formula(paste("y ~", gsub(r"(Intercept)",1,paste(vars, collapse = " + "))))

  model = stats::lm(formula, data=df)
  pred_lm = matrix(predict(model, newdata=df_new), dimnames=list(NULL,"s1"))
  
  placeholder = rep(0,n_var+1)
  names(placeholder) = rownames(coef_p[["plasso"]])
  coef_lm_init = coef(model)
  placeholder[vars] = coef_lm_init
  coef_lm = as.matrix(placeholder)
  colnames(coef_lm) = "s1"
  
  # check fitted values for plasso
  expect_equal(pred_p[["plasso"]],pred_lm)
  
  # check coefficients for plasso
  expect_equal(as.matrix(coef_p[["plasso"]]),coef_lm)
  
  
  pred_g = predict(p$lasso_full,newx=X_pred,s=s)
  coef_g = coef(p$lasso_full,s=s)
  
  # check format of plasso fitted values
  expect_identical(dimnames(pred_p[["plasso"]]),dimnames(pred_g))
  expect_identical(dimnames(pred_p[["lasso"]]),dimnames(pred_g))
  
  # check format of plasso coefficients
  expect_identical(dimnames(coef_p[["plasso"]]),dimnames(coef_g))
  expect_identical(dimnames(coef_p[["lasso"]]),dimnames(coef_g))


})


test_that("predict.cv.plasso for s=all", {
  
  # dgp
  n = 1000
  n_pred = 3
  n_var = 10
  X = matrix(runif(n*n_var,-pi,pi),ncol=n_var)
  X_pred = matrix(runif(n_pred*n_var,-pi,pi),ncol=n_var)
  y = runif(n)
  
  s="all"
  
  # fit model and extract predictions
  p = plasso::cv.plasso(X,y)
  pred_p = predict(p,newx=X_pred,s=s)
  coef_p = predict(p,type="coefficients",s=s)
  
  # create dataframes for stats::lm model
  df = data.frame(cbind(y,X))
  colnames(df) = c("y",rownames(coef_p[["plasso"]])[2:(n_var+1)])
  df_new = as.data.frame(X_pred)
  colnames(df_new) = rownames(coef_p[["plasso"]])[2:(n_var+1)]
  
  # get active coefficients
  active_list = lapply(seq_len(ncol(coef_p[["plasso"]])), function(i) {
    rownames(coef_p[["plasso"]])[coef_p[["plasso"]][,i] != 0]
  })
  
  # get predictions from stats::lm
  predict_lm_models = vapply(active_list, function(r) {
    formula_str = paste("y ~", gsub(r"(Intercept)",1,paste(r, collapse = " + ")))
    formula = as.formula(formula_str)
    model = stats::lm(formula, data=df)
    pred_lm = predict(model, newdata=df_new)
    return(pred_lm)
  }, FUN.VALUE = numeric(n_pred))
  dimnames(predict_lm_models) = dimnames(pred_p[["plasso"]])
  
  # get coefficients from stats::lm
  coef_lm_models = vapply(active_list, function(r) {
    placeholder = rep(0,n_var+1)
    names(placeholder) = rownames(coef_p[["plasso"]])
    formula_str = paste("y ~", gsub(r"(Intercept)",1,paste(r, collapse = " + ")))
    formula = as.formula(formula_str)
    model = stats::lm(formula, data=df)
    coef_lm_init = coef(model)
    placeholder[r] = coef_lm_init
    return(placeholder)
  }, FUN.VALUE = numeric(n_var + 1))
  dimnames(coef_lm_models) = dimnames(coef_p[["plasso"]])
  
  # check fitted values for plasso
  expect_equal(pred_p[["plasso"]],predict_lm_models)
  
  # check coefficients for plasso
  expect_equal(as.matrix(coef_p[["plasso"]]),coef_lm_models)
  
  # check predictions for lasso
  pred_g = predict(p$lasso_full,newx=X_pred)
  coef_g = coef(p$lasso_full)
  expect_equal(pred_p[["lasso"]],pred_g)
  
  # check coefficients for lasso
  expect_identical(coef_p[["lasso"]][-1,],p$lasso_full$beta)
  expect_identical(coef_p[["lasso"]],coef_g)
  
  # check format of plasso fitted values
  expect_equal(dimnames(pred_p[["plasso"]]),dimnames(pred_g))
  
  # check format of plasso coefficients
  expect_identical(dimnames(coef_p[["plasso"]]),dimnames(coef_g))
  
})


test_that("predict.cv.plasso response for s=optimal", {
  
  n = 1000
  n_pred = 7
  n_var = 20
  X = matrix(runif(n*n_var,-pi,pi),ncol=n_var)
  X_pred = matrix(runif(n_pred*n_var,-pi,pi),ncol=n_var)
  y = runif(n)
  
  s = "optimal"
  p = plasso::cv.plasso(X,y)
  pred_p = predict(p,newx=X_pred,s=s)
  coef_p = predict(p,type="coefficients",s=s)
  
  df = data.frame(cbind(y,X))
  colnames(df) = c("y",rownames(coef_p[["plasso"]])[2:(n_var+1)])
  df_new = as.data.frame(X_pred)
  colnames(df_new) = rownames(coef_p[["plasso"]])[2:(n_var+1)]
  
  vars = rownames(coef_p[["plasso"]])[as.vector(coef_p[["plasso"]] != 0)]
  formula = as.formula(paste("y ~", gsub(r"(Intercept)",1,paste(vars, collapse = " + "))))
  
  model = stats::lm(formula, data=df)
  pred_lm = matrix(predict(model, newdata=df_new), dimnames=list(NULL,"s1"))
  colnames(pred_lm) = "optimal(0)"
  
  placeholder = rep(0,n_var+1)
  names(placeholder) = rownames(coef_p[["plasso"]])
  coef_lm_init = coef(model)
  placeholder[vars] = coef_lm_init
  coef_lm = as.matrix(placeholder)
  colnames(coef_lm) = "optimal(0)"
  
  # check fitted values for plasso
  expect_equal(pred_p[["plasso"]],pred_lm)
  
  # check coefficients for plasso
  expect_equal(as.matrix(coef_p[["plasso"]]),coef_lm)
  
  
  pred_g = predict(p$lasso_full,newx=X_pred,s=p$lasso_full$lambda[p$ind_min_l])
  coef_g = coef(p$lasso_full,s=p$lasso_full$lambda[p$ind_min_l])
  colnames(pred_g) = "optimal(0)"
  colnames(coef_g) = "optimal(0)"
  
  # check fitted values for lasso
  expect_equal(pred_p[["lasso"]],pred_g)
  
  # check coefficients for lasso
  expect_equal(coef_p[["lasso"]],coef_g)
  
  # check format of plasso fitted values
  expect_identical(dimnames(pred_p[["plasso"]]),dimnames(pred_g))
  expect_identical(dimnames(pred_p[["lasso"]]),dimnames(pred_g))
  
  # check format of plasso coefficients
  expect_identical(dimnames(coef_p[["plasso"]]),dimnames(coef_g))
  expect_identical(dimnames(coef_p[["lasso"]]),dimnames(coef_g))
  
  
})