envsig <- function(envlist, index = NULL, method = c("eb", "fisher", "min")){
  if (!is.list(envlist)) stop(
    "The method 'envsig' must be applied to an object from the output of envelope()")
  if (is.null(envlist$variogram0)) stop(
    "The method 'envsig' must be applied to an object from the output of envelope()")
  switch(class(envlist$variogram0)[1],
         "gstatVariogram"={
           dims <- dim(envlist$variogram)
           pvals <- sapply(1:dims[1], function(i){
             # number of more extreme semivariances
             min(table(envlist$variogram0$gamma[i] < envlist$variogram[i,]))/dims[2]
           })
         },
         "variogram"={
           dims <- dim(envlist$variogram)
           pvals <- sapply(1:dims[1], function(i){
             min(table(envlist$variogram0$v[i] < envlist$variogram[i,]))/dims[2]
           })
         })
  if (is.null(index)){
    index <- length(pvals)
  }
  # minimum p-value is 1/nsim
  pvals[pvals == 0] <- 1/dims[2]
  # p-value combination method
  rep <- ncol(envlist$variogram) - 1
  Fxs <- matrix(NA, nrow = rep, ncol = index)
  for (i in 1:index){
    Fxs[,i] <- ecdf(envlist$variogram[i,])(envlist$variogram[i,-1])
  }
  del <- unique(unlist(sapply(1:index, function(x) which(Fxs[,x] == 1))))
  covs <- matrix(NA, nrow = index, ncol = index)
  for (i in 1:(index-1)){
    for (j in (i+1):(index)){
      covs[i,j] <- stats::cov(-2 * log(1- Fxs[-del,i]), -2 * log(1- Fxs[-del,j]))
    }
  }
  pvals <- apply(envlist$variogram, 1, function(x){1 - mean(x[1] < x[-1])})[1:index]
  pvals[pvals == 0] <- 1/rep
  switch(method,
         "eb" = {
           sum_covs <- sum(covs[upper.tri(covs)])
           E <- 2 * index
           Var <- 4 * index + 2*sum_covs
           f = 2*E^2 / Var
           c = Var/(2*E)
           p.overall <- pchisq(-2*sum(log(pvals))/c, df = f, lower.tail = F)
         },
         "fisher" = {
           p.overall <- pchisq(-2*sum(log(pvals)), df = 2*length(pvals), lower.tail = F)
         },
         "min" = {
           p.overall <- min(pvals)
         }
  )
  return(list(p.pointwise = pvals, p.overall = p.overall))
}
