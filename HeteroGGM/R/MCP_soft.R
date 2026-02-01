MCP_soft <- function(z,lambda,a=3){

  ## -----------------------------------------------------------------------------------------------------------------
  ## The name of the function: MCP_soft
  ## -----------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Define the MCP threshold operator.
  ## -----------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages: No
  ## -----------------------------------------------------------------------------------------------------------------
  ## Input:
  ## @ z: a float value or a vector, the independent variable in the MCP.
  ## @ lambda: a float value, the tuning parameter in the MCP.
  ## @ a: a float value, regularization parameter in the MCP, the default setting is 3.
  ## -----------------------------------------------------------------------------------------------------------------
  ## Output:
  ## @ The result of the MCP threshold operator.
  ## -----------------------------------------------------------------------------------------------------------------

  return( S_soft(z,lambda)/(1-1/a) * (abs(z) - a*lambda <= 0) + z * (abs(z) - a*lambda > 0) )
}
