# internal function for margin estimation. Is called by the all the individual 
# tests 
.margins <- function(x, margins) {
  if (length(margins) == 1) {
    b <- apply(x, 2, .one.mar, margins = margins)
  } else {
    b <- list()
    for (i in seq_len(NCOL(x))) {
      b[[i]] <- .one.mar(x[, i], margins[i])
    }
  }
  return(b)
}

# internal function to estimate one particular margin. Is called by .margins
.one.mar <- function(x, margins) {
  n <- NROW(x)
  if (margins == "ranks") {
    return(list(ecdf(x)(x) * n / (n + 1)))
  } else {
    boundary <- 10000
    if ((any(x <= 0)) & ((margins == "beta") | (margins == "chisq") | 
                        (margins == "f") | (margins == "gamma") | 
                        (margins == "lnorm") | (margins == "weibull"))) {
stop(paste(
"Cannot fit", margins, "marginal distribution to non-positive values.", 
sep = " "))
    }
    if ((margins == "beta") | (margins == "cauchy") | (margins == "chisq") |
      (margins == "f") | (margins == "gamma") | (margins == "lnorm") |
      (margins == "norm") | (margins == "t") | (margins == "weibull")) {
      loglik <- function(par, x) {
        sum(log(eval(do.call(paste("d", margins, sep = ""),
          args = list(x = x, par[1], par[2])
        ))))
      }
      op <- constrOptim(
        theta = c(1, 1), f = loglik, grad = NULL,
        ui = matrix(c(1, 0, -1, 0, 0, 1), nrow = 3, byrow = TRUE),
        ci = c(-rep(boundary, 2), 0), x = x, control = list(fnscale = -1),
        hessian = FALSE
      )
      return(list(c(op$par[1], op$par[2]), 
                  eval(do.call(paste("p", margins, sep = ""), args = list(
        q = x,
        op$par[1], op$par[2]
      )))))
    } else {
      if ((margins == "exp")) {
        if (any(x < 0)) {
stop(
"Cannot fit exponential marginal distribution to negative values."
)
        }
        op <- 1 / mean(x)
        return(list(op, eval(do.call(paste("p", margins, sep = ""), args = list(
          q = x,
          op
        )))))
      }
    }
  }
}
