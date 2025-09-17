bootstrap <- function(obj, ...) UseMethod("bootstrap")

# bootstrap.ilse <- function(obj,repTimes=100){
#   data <- obj$data
#   formula <- obj$formula
#   bw <- obj$inargs$bw
#   k.type<- obj$inargs$k.type
#   method <- obj$inargs$method
#   max.iter <- obj$inargs$max.iter
#   peps <- obj$inargs$peps
#   feps  <- obj$inargs$feps
#   arma  <- obj$inargs$arma
#   infor_output <- F
#   form <- terms(formula, data=data)
#   real_p <- ncol(data)-1
#
#
#   n <- nrow(data)
#   message('===================Start bootstrapping================\n')
#   res.par <- matrix(NA,nrow=repTimes, ncol=real_p)
#   for(k in 1:repTimes)
#   {
#     # k <- 1
#     set.seed(k)
#     ind <- sample(1:n, n, replace = T)
#     data1 <- data[ind, ]
#     disProBar(k, repTimes)
#     try(
#       {
#         coef.par <- ilse(formula, data1, bw, k.type, method, max.iter=max.iter,
#                          peps=peps, feps=feps,verbose=infor_output, arma=arma)$beta
#         res.par[k, ] <- coef.par
#       }, silent=TRUE
#     )
#
#   }
#   message('===================Finish bootstrapping================\n')
#   return(cov(res.par, na.rm=TRUE))
# }

bootstrap.ilse <- function(obj,repTimes=100){
  data <- obj$data
  formula <- obj$formula
  bw <- obj$inargs$bw
  k.type<- obj$inargs$k.type
  method <- obj$inargs$method
  max.iter <- obj$inargs$max.iter
  peps <- obj$inargs$peps
  feps  <- obj$inargs$feps
  arma  <- obj$inargs$arma
  infor_output <- F
  form <- terms(formula, data=data)
  real_p <- ncol(data)-1


  n <- nrow(data)
  message('===================Start bootstrapping================\n')
  res.par <- pbapply::pbsapply(1: repTimes, function(k){
    set.seed(k)
    ind <- sample(1:n, n, replace = T)
    data1 <- data[ind, ]
    coef1 <- rep(NA, real_p)
    try(
      {
        coef1 <- ilse(formula, data1, bw, k.type, method, max.iter=max.iter,
                         peps=peps, feps=feps,verbose=infor_output, arma=arma)$beta
      }, silent=TRUE)
    return(coef1)
  })
  message('===================Finish bootstrapping================\n')
  return(cov(t(res.par)))
}
bootstrap.fiml <- function(obj, repTimes=100){
  data <- obj$data
  formula <- obj$formula
  n <- nrow(data)
  p <- ncol(data)
  res.par <- matrix(nrow=repTimes, ncol= p)
  message('===================Start bootstrapping================\n')
  res.par <- pbapply::pbsapply(1: repTimes, function(k){
    set.seed(k)
    ind <- sample(1:n, n, replace = T)
    data1 <- data[ind, ]
    coef1 <- rep(NA, p)
    try(
      {
        coef1 <- fimlreg(formula, data1)$beta
      }, silent=TRUE)
    return(coef1)
  })
  # for(k in 1:repTimes)
  # {
  #   set.seed(k)
  #   ind <- sample(1:n, n, replace = T)
  #   data1 <- data[ind, ]
  #   disProBar(k, repTimes)
  #   # try(coef.par <- fimlreg(formula, data1)$beta, silent = T)
  #   coef.par <- fimlreg(formula, data1)$beta
  #   res.par[k, ] <- coef.par
  # }
  message('===================Finish bootstrapping================\n')
  return(cov(t(res.par)))
}
