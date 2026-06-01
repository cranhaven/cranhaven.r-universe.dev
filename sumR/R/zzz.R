# Conway-Maxwell-Poisson normalizing constant
COMP <- function(k, Theta) k * log(Theta[1]) - Theta[2] * lfactorial(k)

# Double poisson normalizing constant
dbl_poisson <- function(k, Theta)
  ifelse(k == 0, 0.5 * log(Theta[2]) - Theta[1] * Theta[2],
    0.5 * log(Theta[2]) - (Theta[1] - k) * Theta[2] - k +
    k * (1 - Theta[2]) * log(k) - lfactorial(k) + Theta[2] * k * log(Theta[1]))

# Modified bessel function of the first kind
bessel_I <- function(k, Theta)
  (2 * k + Theta[2]) * (log(Theta[1]) - log(2)) - lgamma(k + 1) -
  lgamma(Theta[2] + k + 1)

# Modified bessel function of the first kind with log argument
bessel_I_logX <- function(k, Theta)
  (2 * k + Theta[2]) * (Theta[1] - log(2)) - lgamma(k + 1) -
  lgamma(Theta[2] + k + 1)

# Determining logL based on character logFunction to name the method used in C
determineLogL_ <- function(lF, p) {
  if (lF == "COMP") return(-Inf)
  if (lF == "double_poisson") return(-Inf)
  if (lF == "bessel_I") return(-Inf)
  if (lF == "bessel_I_logX") return(-Inf)
  if (lF == "negbin_marginal") return(log(p[1]) - log(p[1] + p[2]) + log1p(-p[3]))
  if (lF == "noObs") return(log1p(-p[1]))
  if (lF == "dR0") return(log(p[1]) + log1p(-p[4]) + (1 + p[2]) * (log1p(p[2]) - log(p[1] + p[2])))
  if (lF == "powerLawDiff") return(log(0.9999))
  if (lF == "negbin_sentinel") return(log(p[1]) - log(p[1] + p[2]) + log1p(-p[3]))
  if (lF == "poisson_sentinel") return(-Inf)
  if (lF == "weird_series_constL") return(log(p[1]))
  if (lF == "weird_series") return(-1)
  if (lF == "poisson_fact_moment") return(-Inf)
}