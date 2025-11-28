fn.solnp <- function(par0, vY, FUN, LB, UB, ...) {

  solver.ctr <- list(trace = 0, rho = 1, outer.iter = 400, inner.iter = 800, delta = 1e-07, tol = 1e-08)

  optimiser = suppressWarnings(solnp(par0, FUN, vY = vY, LB = LB, UB = UB, control = solver.ctr, ...))

  out = list(pars = optimiser$pars,
             value = tail(optimiser$values, 1),
             hessian = optimiser$hessian,
             convergence = optimiser$convergence)

  return(out)

}

fn.optim <- function(par0, vY, FUN, LB, UB, ...) {

  solver.ctr <- list(trace = 0, abstol = 1e-8, reltol = 1e-8)

  optimiser = suppressWarnings(optim(par0, FUN, vY = vY,
                                     method = "L-BFGS-B", lower = LB, upper = UB,
                                     control = solver.ctr,  hessian = TRUE, ...))

  out = list(pars = optimiser$par,
             value = optimiser$value,
             hessian = optimiser$hessian,
             convergence = optimiser$convergence)

  return(out)

}

fn.DEoptim <- function(par0, vY, FUN, LB, UB, ...) {

  foo = list(...)
  if (!is.null(foo$cluster)) {
    cluster = foo$cluster
    clusterEvalQ(cluster, library(DMQ))
  } else {
    cluster = NULL
  }

  if (!is.null(foo$itermax)) {
    itermax = foo$itermax
  } else {
    itermax = 500
  }

  if (!is.null(foo$NP)) {
    NP = foo$NP
  } else {
    NP = 50 * length(LB)
  }

  initialpop = NULL
  for (i in 1:length(UB)) {
    initialpop = cbind(initialpop, runif(NP, LB[i], UB[i]))
  }

  initialpop[1, ] = par0

  control = DEoptim.control(initialpop = initialpop, cluster = cluster, NP = NP,
                            itermax = itermax, reltol = 1e-6, steptol = 20)
  optimizer = DEoptim(fn = function(vPar, vNames, LossFun, vY, ...) {

    names(vPar) = vNames
    LossFun(vPn = vPar, vY = vY, ...)

  }, lower = LB, upper = UB,  control = control, vNames = names(par0), LossFun = FUN, vY = vY, ...)

  par0[1:length(par0)] = optimizer$optim$bestmem
  dLoss = optimizer$optim$bestval
  
  mHessian = try(optimHess(par0, fn = function(vPar, vNames, LossFun, vY, ...) {

    LossFun(vPn = vPar, vY = vY, hessian_computation = TRUE, ...)

  }, lower = LB, upper = UB, method =  "L_BFGS-B",  LossFun = FUN, vY = vY, ...))

  lOut = list(pars = par0,
              value = dLoss,
              hessian = mHessian,
              convergence = 1)

  return(lOut)
}



