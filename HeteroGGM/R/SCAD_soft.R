SCAD_soft <- function(z,lambda,a=3.7){

  ## -----------------------------------------------------------------------------------------------------------------
  ## The name of the function: SCAD_soft
  ## -----------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Define the SCAD threshold operator.
  ## -----------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages: No
  ## -----------------------------------------------------------------------------------------------------------------
  ## Input:
  ## @ z: a float value or a vector, the independent variable in the SCAD.
  ## @ lambda: a float value, the tuning parameter in the SCAD.
  ## @ a: a float value, regularization parameter in the SCAD, the default setting is 3.7.
  ## -----------------------------------------------------------------------------------------------------------------
  ## Output:
  ## @ The result of the SCAD threshold operator.
  ## -----------------------------------------------------------------------------------------------------------------

  return( S_soft(z,lambda) * (abs(z) - lambda/2 <= 0) +
            S_soft(z,lambda)/(1-1/a) * (abs(z) - lambda/2 > 0 & abs(z) - a*lambda <= 0) +
            z * (abs(z) - a*lambda > 0) )
}
