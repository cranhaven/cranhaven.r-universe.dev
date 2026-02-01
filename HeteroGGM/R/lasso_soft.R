lasso_soft <- function(z,lambda,a=3){

  ## -----------------------------------------------------------------------------------------------------------------
  ## The name of the function: lasso_soft
  ## -----------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Define the lasso threshold operator.
  ## -----------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages: No
  ## -----------------------------------------------------------------------------------------------------------------
  ## Input:
  ## @ z: a float value or a vector, the independent variable in the lasso.
  ## @ lambda: a float value, the tuning parameter in the lasso.
  ## @ a: a float value, regularization parameter in the lasso, the default setting is 3.
  ## -----------------------------------------------------------------------------------------------------------------
  ## Output:
  ## @ The result of the lasso threshold operator.
  ## -----------------------------------------------------------------------------------------------------------------

  return( S_soft(z,lambda) * a/a )
}
