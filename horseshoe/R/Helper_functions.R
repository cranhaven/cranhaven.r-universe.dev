#' Helper function for computing the posterior mean, posterior variance
#' @keywords internal
Basic.integrand <- function(u, y, k, tau, data.var){ #integrand
  u^k*(1-u)^(-1/2)*(1 + (tau^2 - 1)*u)^(-1)*exp( -y^2*u / (2*data.var) )
} #Blows up at 1


#' Helper function for computing the posterior mean, posterior variance
#' @keywords internal
Basic.y <- function(y, tau, k, data.var){ #integrate Basic.integrand
  stats::integrate(Basic.integrand, 0, 1, tau = tau, y = y, k = k, data.var = data.var, rel.tol = 1e-8)$value
}

#' Helper function for computing the posterior mean, posterior variance
#' @keywords internal
Basic.y.vec <- Vectorize(Basic.y) #can handle a vector as input

## Helper functions for estimating the MMLE
## Can handle very sparse situations only for n <= 425
#' @keywords internal
Basic.MMLE <- function(u, y, t, data.var){ #integrand
  (1-u)^(-1/2)*(1 - (1-t^2)*u)^(-1)*exp(-u*y^2 / (2*data.var))
}

## Helper functions for estimating the MMLE
## Can handle very sparse situations only for n <= 425
#' @keywords internal
Term.MMLE <- function(y, t, data.var){ #integrate the integrand and take the log
  log( stats::integrate(Basic.MMLE, 0, 1, y = y,  t = t, data.var = data.var, rel.tol = 1e-8)$value ) + log(t)
}

## Helper functions for estimating the MMLE
## Can handle very sparse situations only for n <= 425
#' @keywords internal
Term.MMLE.vec <- Vectorize(Term.MMLE) #handles a vector as input

## Helper functions for estimating the MMLE
## Can handle very sparse situations only for n <= 425
#' @keywords internal
MMLE.M <- function(t, data, data.var){ #the quantity to be maximized
  sum(Term.MMLE.vec(data, t, data.var))
}
