ar1sim = function(n = 50, rho, u0 = 0, var.e = 1, details = FALSE, seed = NULL){
  set.seed(seed)
  e = rnorm(n, 0, var.e) # random error
  u = numeric(n) # Generiere Nullvektor
  u[1] = u0 # Erste Beobachtung
  for (t in 2:(n)){ # Restliche Beobachtungen
    u[t] = rho * u[t-1] + e[t]
  }
  out = list(u.sim = u, n = n, rho = rho, e.sim = e)

  attr(out, "details") = if (details) {TRUE} else {FALSE}
  attr(out, "type") = "ar1sim"
  class(out) = c("desk")
  return(out)
}
