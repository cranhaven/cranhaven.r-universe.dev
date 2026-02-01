f.den.vec <- function(data, mu, Theta){

  ## -----------------------------------------------------------------------------------------------------------------
  ## The name of the function: f.den.vec
  ## -----------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            calculate the density function values at each sample point.
  ## -----------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages: No
  ## -----------------------------------------------------------------------------------------------------------------
  ## Input:
  ## @ data: n * p matrix, the design matrix.
  ## @ mu1: p * 1 vector, the mean vector.
  ## @ Omega1: p * p matrix, the precision matrix.
  ## -----------------------------------------------------------------------------------------------------------------
  ## Output:
  ## @ fdensity: The density function values at each sample point.
  ## -----------------------------------------------------------------------------------------------------------------

  p = length(mu)
  fden = as.numeric( (2*pi)^(-p/2) * (det(Theta))^(1/2) * exp(-1/2*diag(t(t(data) - as.numeric(mu)) %*% Theta %*% (t(data) - as.numeric(mu)))) )
  return(fden)
}
