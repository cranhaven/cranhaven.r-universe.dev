adfplot <- function(data, colX, colY, qmarg1, qmarg2, constrainedshape, lengthw, method, q, qalphas1, qalphas2, k, constrained, tol, parinit){
  data <- cbind(data[[colX]], data[[colY]])
  constrainedshape <- as.logical(constrainedshape)
  parainit <- as.numeric(parinit)
  margdata <- margtransf(data, qmarg = c(qmarg1, qmarg2), constrainedshape = constrainedshape)
  w <- seq(0, 1, length.out = lengthw)
  constrained <- as.logical(constrained)
  par_init <- rep(parinit, k - 1)
  adf_data <- adf_est(margdata = margdata, w = w, method = method, q = q, 
                      qalphas = c(qalphas1, qalphas2), k = k, constrained = constrained,
                      tol = tol, par_init = par_init)
  return(adf_data)
}

gofadfplot <- function(adf, ray, blocksize, nboot, alpha){
  adfgof <- adf_gof(adf = adf, ray = ray, blocksize = blocksize, 
                    nboot = nboot, alpha = alpha)
  plot(adfgof)
}


