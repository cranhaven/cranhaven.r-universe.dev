#' @title Model Predictions
#'
#' @description \code{pred.sglmm} is a function for predictions from the results of \code{fsglmm}.
#'
#' @param fit.sglmm a list from \code{fsglmm}
#' @param data a data frame to be predicted by the model.
#' @param coords coordinates for prediction
#' @param ntrial a numeric vector of the total number of trials ( binomial )
#' @param offset a numeric vector indicating a known component to be included in the linear predictor for predictions.
#'
#' @return a vector of predicted mean parameters. (e.g. probabilities for binomial case)
#'
#'
#' @importFrom fields rdist
#'
#' @export
#'
#' @examples
#'
#' ## the result from fsglmm, data to be predicted, and the coordinates for prediction is required.
#'
#' \donttest{
#' result <- fsglmm(Y~-1+X1+X2, kappa=2.5, inits = startinit, data = data,coords = coords,
#' family = "binomial", ntrial = 1, offset = NA,method.optim = "CG",method.integrate = "NR",rank = 50)
#' pred.sglmm(fit.sglmm=result,data=X.pred,)
#'}
#'
#'
pred.sglmm <- function(fit.sglmm,data,coords,ntrial=1,offset=NA){
  n.beta <- dim(data)[2]
  n <- dim(fit.sglmm$coords)[1]
  n.pred <- dim(data)[1]
  est <- fit.sglmm$summary@coef[,1]
  est[(n.beta+1):(length(est))] <- exp(est[(n.beta+1):(length(est))])
  sigma <- est["logsigma2"]
  phi <- est["logphi"]
  beta <- est[1:n.beta]

  kappa <- fit.sglmm$kappa
  X = as.matrix(data)
  Xbeta = X%*%beta
  if(is.na(offset)[1] != TRUE){Xbeta <- cbind(X,log(offset))%*%c(beta,1)}

  UM <- fit.sglmm$U
  DM <- fit.sglmm$D
  Delta <- as.numeric(fit.sglmm$Delta)
  r.e.hat <- UM %*%diag(diag(DM)^0.5) %*% as.numeric(Delta)
  coords.fit <- fit.sglmm$coords
  coords.all <- rbind(coords.fit,coords)

  dist.all <- fields::rdist(coords.all)
  S.hat <-  sigma*matern.cov(phi=phi,kappa=kappa,mat.dist = dist.all)
  r.e.pred <-  (1/sigma)*S.hat[(n+1):(n+n.pred),1:n] %*% UM %*% diag(diag(DM)^-1) %*% t(UM) %*% matrix(r.e.hat)
  r.e.pred <- as.numeric(r.e.pred)
  family <- fit.sglmm$family
  eta <- as.numeric(Xbeta + r.e.pred)
  if(family=="binomial"){
    res = 1/(1+exp(-eta))
  }
  if(family=="negative.binomial"|family=="poisson"){
    res = exp(eta)
  }
  return(res)
}
