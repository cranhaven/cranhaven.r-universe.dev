# Internal functions for the derivation of the t copula for the PIOSRn and 
# PIOSTn test statistics. Called by .Rn and .Tn, the internal functions for
# their test statistics.

# 2d density
.t.12.dens <- function(x, u, nu) {
  if (is.null(dim(u))) {
    u1 <- u[1]
    u2 <- u[2]
  } else {
    u1 <- u[, 1]
    u2 <- u[, 2]
  }
  res.f <- -((nu * sqrt(1 - (x^2)) * (((nu + (u1^2)) * (nu + (u2^2))) / 
              (nu^2))^((1 + nu) / 2) * gamma(nu / 2) * gamma((2 + nu) / 2)) / 
               ((nu * (-1 + (x^2)) - (u1^2) + 2 * x * u1 * u2 - (u2^2)) * 
                ((nu - nu * (x^2) + (u1^2) - 2 * x * u1 * u2 + (u2^2)) / 
                  (nu - nu * (x^2)))^(nu / 2) * (gamma((1 + nu) / 2))^2))
  return(res.f)
}

# 2d first derivative wrt theta of the log-density
.V.t <- function(u, rho, nu) {
  res.f <- (-rho^3 + (2 + nu) * u[1] * u[2] + nu * (rho^2) * u[1] * u[2] - 
              rho * (-1 + (1 + nu) * u[1]^2 + (1 + nu) * u[2]^2)) / 
    ((-1 + rho^2) * (-1 + rho^2 - u[1]^2 + 2 * rho * u[1] * u[2] - u[2]^2))
  return(res.f)
}

# 2d second derivative wrt theta of the log-density
.S.t <- function(u, rho, nu) {
  res.f <- (1 + (rho^6) - (u[1]^4) - 2 * nu * (rho^5) * u[1] * u[2] + 2 * 
              (u[1]^2) * (u[2]^2) - (u[2]^4) - nu * ((u[1]^2) + (u[1]^4) + 
              (u[2]^2) + (u[2]^4)) + 4 * (rho^3) * u[1] * u[2] * (-2 + 
              (u[1]^2) + (u[2]^2) + nu * (-1 + (u[1]^2) + (u[2]^2))) + 2 * 
              rho * u[1] * u[2] * (2 * (2 + (u[1]^2) + (u[2]^2)) + nu * 
              (3 + 2 * (u[1]^2) + 2 * (u[2]^2))) - (rho^2) * (1 + (1 + nu) * 
              (u[1]^4) + 2 * (2 + nu) * (u[2]^2) + (1 + nu) * (u[2]^4) + 2 * 
              (u[1]^2) * (2 + nu + 7 * (u[2]^2) + 5 * nu * (u[2]^2))) + 
              (rho^4) * (-1 + (4 + 3 * nu) * (u[2]^2) + (u[1]^2) * (4 + nu * 
              (3 - 2 * (u[2]^2))))) / ((-1 + (rho^2))^2 * (1 - (rho^2) + 
              (u[1]^2) - 2 * rho * u[1] * u[2] + (u[2]^2))^2)
  return(res.f)
}

