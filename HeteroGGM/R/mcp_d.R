mcp_d <- function(x,lambda,a=3){

  ## -----------------------------------------------------------------------------------------------------------------
  ## The name of the function: mcp_d
  ## -----------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Calculating the derivative of the MCP
  ## -----------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages: No
  ## -----------------------------------------------------------------------------------------------------------------
  ## Input:
  ## @ x: a float value or a vector, the independent variable in the MCP.
  ## @ lambda: a float value, the tuning parameter in the MCP.
  ## @ a: a float value, regularization parameter in the MCP, the default setting is 3.
  ## -----------------------------------------------------------------------------------------------------------------
  ## Output:
  ## @ rho: the derivative of the MCP.
  ## -----------------------------------------------------------------------------------------------------------------

  if(lambda!=0){
    rho <- lambda*( 1 > abs(x)/( lambda*a ) )*( 1 - abs(x)/( lambda*a ))
  } else{
    rho=0
  }
  return(rho)
}
