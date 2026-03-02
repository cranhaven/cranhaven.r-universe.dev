rcplot <- function(data, colX, colY, qmarg1, qmarg2, constrainedshape, lengthw, 
                   p, method, q, qalphas1, qalphas2, k, constrained, tol, parinit){
  data <- cbind(data[[colX]], data[[colY]])
  constrainedshape <- as.logical(constrainedshape)
  parainit <- as.numeric(parinit)
  margdata <- margtransf(data, qmarg = c(qmarg1, qmarg2), constrainedshape = constrainedshape)
  w <- seq(0, 1, length.out = lengthw)
  constrained <- as.logical(constrained)
  par_init <- rep(parinit, k - 1)
  rc_data <- rc_est(margdata = margdata, w = w, p = p, method = method,
                    q = q, qalphas = c(qalphas1, qalphas2), k = k, constrained = constrained,
                    tol = tol, par_init = par_init)
  return(rc_data)
}

uncrcplot <- function(retcurve, blocksize, nboot, nangles, alpha){
  rcunc <- rc_unc(retcurve = retcurve, blocksize = blocksize, 
                  nboot = nboot, nangles = nangles, alpha = alpha)
  plot(rcunc)
}

gofrcplot <- function(retcurve, blocksize, nboot, nangles, alpha){
  rcgof <- rc_gof(retcurve = retcurve, blocksize = blocksize,
                  nboot = nboot, nangles = nangles, alpha = alpha)
  plot(rcgof)
}