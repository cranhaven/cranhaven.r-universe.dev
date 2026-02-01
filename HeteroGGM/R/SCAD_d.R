SCAD_d <- function(x,lambda,a=3.7){

  ## -----------------------------------------------------------------------------------------------------------------
  ## The name of the function: SCAD_d
  ## -----------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Calculating the derivative of the SCAD
  ## -----------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages: No
  ## -----------------------------------------------------------------------------------------------------------------
  ## Input:
  ## @ x: a float value or a vector, the independent variable in the SCAD.
  ## @ lambda: a float value, the tuning parameter in the SCAD.
  ## @ a: a float value, regularization parameter in the SCAD, the default setting is 3.7.
  ## -----------------------------------------------------------------------------------------------------------------
  ## Output:
  ## @ rho: the derivative of the SCAD.
  ## -----------------------------------------------------------------------------------------------------------------

  if(lambda!=0){
    rho <- lambda*(   (abs(x) <= lambda) +
                      (abs(x) > lambda) * ( lambda*a > abs(x) )*( lambda*a - abs(x) )/((a-1)*lambda)   )
  } else{
    rho=0
  }
  return(rho)
}
