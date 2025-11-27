#' Estimation of The Partial Credit Model (PCM)
#'
#' This function computes the parameter estimates of a partial credit model for dichotomous and polytomous responses
#' by using penalized joint maximum likelihood estimation (PJMLE). Inputting a dichotomous responses to this model,
#' will automatically transforms the PCM to the Rasch model.
#'
#' @param X Input dataset as matrix or data frame with ordinal responses (starting from 0);
#' rows represent individuals, columns represent items.
#' @param init_par a vector of initial values of the estimated parameters.
#' @param setting a list of the optimization control setting parameters. See \code{\link[autoRasch:autoRaschOptions]{autoRaschOptions()}.}
#' @param method The implementation option of log likelihood function. \code{fast} using a \code{c++} implementation and \code{novel} using an \code{R} implementation.
#'
#' @return
#' \strong{\code{pcm()} will return a \code{\link[base:list]{list}} which contains:}
#' \item{X}{   The dataset that is used for estimation.}
#' \item{mt_vek}{   A vector of the highest response given to items.}
#' \item{itemName}{   The vector of names of items (columns) in the dataset.}
#' \item{loglik}{   The log likelihood of the estimation.}
#' \item{hessian}{   The hessian matrix. Only when the \code{isHessian = TRUE}.}
#' \item{beta}{   A vector of the difficulty parameter of each categories of items (thresholds).}
#' \item{theta}{   A vector of the ability parameters of each individuals.}
#'
#' @seealso \code{\link{pcm}}, \code{\link{gpcm}}
#'
#' @references
#' Wright, B. D., & Masters, G. N. (1982). Rating Scale Analysis. Chicago: MESA Press.
#'
#' @examples
#' pcm_res <- pcm(shortDIF)
#' summary(pcm_res)
#'
#' #To summarize only for beta parameters
#' summary(pcm_res, par="beta")
#' fit_res <- fitStats(pcm_res)
#' itemfit(fit_res)
#' personfit(fit_res)
#' plot_fitStats(fit_res, toPlot = c("alpha","outfit"), useName = TRUE)
#'
#' @rdname pcm
#' @export
pcm <- function(X, init_par = c(), setting = c(), method = c("fast","novel")){

  if(is.null(setting)){
    settingPar <- autoRaschOptions()
  } else {
    if("aR_opt" %in% class(setting)){
      settingPar <- setting
    } else {
      stop("autoRasch ERROR: The setting used should be a class of `aR_opt'!")
    }
  }

  settingPar$fixed_par <- c("gamma","delta")
  settingPar$isPenalized_gamma <- FALSE
  settingPar$isPenalized_delta <- FALSE

  settingPar$optz_method <- "optim"

  if(!is.null(setting$optim_control)){
    settingPar$optim_control <- setting$optim_control
  }

  result <- pjmle(X = X, init_par = init_par, setting = settingPar, method = method[1])

  class(result) <- c("pcm","armodels","autoRasch",class(result))

  return(result)

}


#' Fit Statistics PCM
#'
#' \code{fitStats} compute the fit statistics (e.g., Outfit and Infit) of the PCM model estimation (items and persons).
#'
#' @param obj The object of class \code{'pcm'}.
#' @param isAlpha Boolean value that indicates whether the discrimination parameters is needed to be estimated or not.
#' The discrimination parameters are estimated using the corresponding models (GPCM or GPCM-DIF).
#'
#' @return
#' \strong{\code{fitStats()} will return a \code{\link[base:list]{list}} which contains:}
#' \item{alpha}{   A vector of estimated discrimination parameters for each items.}
#' \emph{i.fit}{   Item fit statistics.}
#' \itemize{
#'    \item{i.outfitMSQ}{   A vector of Outfit mean square values for each items.}
#'    \item{i.infitMSQ}{   A vector of Infit mean square values for each items.}
#'    \item{i.outfitZ}{   A vector of OutfitZ values for each items.}
#'    \item{i.infitZ}{   A vector of InfitZ values for each items.}
#' }
#' \emph{p.fit}{   Person fit statistics.}
#' \itemize{
#'    \item{p.outfitMSQ}{   A vector of Outfit mean square values for each persons.}
#'    \item{p.infitMSQ}{   A vector of Infit mean square values for each persons.}
#'    \item{p.outfitZ}{   A vector of OutfitZ values for each persons.}
#'    \item{p.infitZ}{   A vector of InfitZ values for each persons.}
#' }
#' \emph{traceMat}{   Some computed matrices in the process.}
#' \itemize{
#'    \item{emat}{   The expected values matrix.}
#'    \item{vmat}{   The variance matrix.}
#'    \item{cmat}{   The curtosis matrix.}
#'    \item{std.res}{   The standardized residual.}
#' }
#'
#'
#' @references
#' Masters, G. N. (1982). A rasch model for partial credit scoring. Psychometrika, 47(2), 149–174. https://doi.org/10.1007/BF02296272. \cr\cr
#' Wright, B. D., & Masters, G. N. (1990). Computation of outfit and infit statistics. Rasch Measurement Transactions, 3(4), 84–85. Retrieved from https://www.rasch.org/rmt/rmt34e.htm
#'
#' @rdname pcm
#' @export
fitStats.pcm <- function(obj, isAlpha = TRUE){

  min.x <- min(obj$X, na.rm = TRUE)
  X <- obj$X - min.x

  # Map the parameters

  mt_vek <- obj$mt_vek
  theta <- obj$theta
  beta <- obj$beta #* obj$real_vek
  # beta <- as.vector(unlist(tapply(beta, rep(1:length(mt_vek),mt_vek), function(x){
  #     y <- x[which(!is.na(x))]
  #     return(y)
  #   })))
  # n.th <- max(mt_vek)

  # mt_vek <- as.vector(tapply(obj$real_vek, rep(seq_along(mt_vek),mt_vek), sum, na.rm = TRUE))
  mt_idx <- rep(seq_along(mt_vek), mt_vek)

  mult_mt_vek <- rep(mt_vek, nrow(X))
  mult_mt_idx <- rep(seq_along(mult_mt_vek), mult_mt_vek)

  xna.mat <- matrix(1,nrow = nrow(X), ncol = ncol(X))
  idx <- which(is.na(X))
  xna.mat[idx] <- NA
  XNA <- xna.mat


  t.diff <- outer((-beta), theta, "+")
  disc.diff <- t.diff

  temp.prob <- matrix(as.vector(unlist(apply(disc.diff, 2, function(x){
    res.temp <- tapply(x, mt_idx, function(y){
      temp <- cumsum((y))
      temp
    })
    res.temp
  }))), nrow = length(beta))


  expTempProb <- exp(temp.prob)

  temp.l2 <- as.vector(tapply(as.vector(expTempProb), mult_mt_idx, function(y){
    temp <- sum((y),na.rm = TRUE)
    temp
  }))

  temp.l1 <- as.vector(unlist(tapply(as.vector(expTempProb), mult_mt_idx, function(y){
    temp <- c(1,y)
    temp
  })))

  l2 <- (temp.l2+1)

  l2 <- rep(l2,mult_mt_vek+1)
  l1 <- temp.l1

  pmat <- l1/l2

  mt_vek0 <- mt_vek + 1
  mt_seq <- sequence(mt_vek0)-1

  Emat <- pmat * mt_seq

  mult_mt_vek0 <- rep(mt_vek0, nrow(X))
  mult_mt_idx0 <- rep(seq_along(mult_mt_vek0), mult_mt_vek0)

  Emat <- as.vector(tapply(as.vector(Emat), mult_mt_idx0, function(y){
    temp <- sum((y),na.rm = TRUE)
    temp
  }))

  Emat.cat <- rep(Emat, mult_mt_vek0)

  Vvect.cat <- ((mt_seq - Emat.cat)^2)*pmat

  Vmat <- as.vector(tapply((Vvect.cat), mult_mt_idx0, function(y){
    temp <- sum((y),na.rm = TRUE)
    temp
  }))

  Cvect.cat <- ((mt_seq - Emat.cat)^4)*pmat

  Cmat <- as.vector(tapply((Cvect.cat), mult_mt_idx0, function(y){
    temp <- sum((y),na.rm = TRUE)
    temp
  }))

  Emat <- t(matrix(Emat, nrow = ncol(X)))
  Vmat <- t(matrix(Vmat, nrow = ncol(X)))
  Cmat <- t(matrix(Cmat, nrow = ncol(X)))

  st.res <- (X-Emat)/sqrt(Vmat)
  sq.res <- st.res^2                            #squared standardized residuals
  ifit <- colSums(sq.res, na.rm = TRUE)
  pfit <- rowSums(sq.res, na.rm = TRUE)

  idf <- apply(X, 2, function(x) {length(na.exclude(x))})
  pdf <- apply(X, 1, function(x) {length(na.exclude(x))})

  i.outfitMSQ <- ifit/idf
  p.outfitMSQ <- pfit/pdf

  qsq.outfitMSQ <- (colSums(Cmat/Vmat^2, na.rm=TRUE)/idf^2) - 1/idf
  q.outfitMSQ <- sqrt(qsq.outfitMSQ)
  p.qsq.outfitMSQ <- (rowSums(Cmat/Vmat^2, na.rm=TRUE)/pdf^2) - 1/pdf
  p.q.outfitMSQ <- sqrt(p.qsq.outfitMSQ)

  isumVmat<-colSums(Vmat*XNA, na.rm = TRUE)
  psumVmat<-rowSums(Vmat*XNA, na.rm = TRUE)
  i.infitMSQ <- colSums(sq.res*Vmat, na.rm = TRUE)/isumVmat
  p.infitMSQ <- rowSums(sq.res*Vmat, na.rm = TRUE)/psumVmat

  qsq.infitMSQ <- colSums(Cmat-Vmat^2, na.rm=TRUE)/isumVmat^2
  q.infitMSQ <- sqrt(qsq.infitMSQ)
  p.qsq.infitMSQ <- rowSums(Cmat-Vmat^2, na.rm=TRUE)/psumVmat^2
  p.q.infitMSQ <- sqrt(p.qsq.infitMSQ)

  i.outfitZ <- (i.outfitMSQ^(1/3) - 1)*(3/q.outfitMSQ)+(q.outfitMSQ/3)
  i.infitZ  <- (i.infitMSQ^(1/3)  - 1)*(3/q.infitMSQ) +(q.infitMSQ/3)

  p.outfitZ <- (p.outfitMSQ^(1/3) - 1)*(3/p.q.outfitMSQ)+(p.q.outfitMSQ/3)
  p.infitZ  <- (p.infitMSQ^(1/3)  - 1)*(3/p.q.outfitMSQ) +(p.q.outfitMSQ/3)

  res_fit <- list("i.fit" = list("i.outfitMSQ" = i.outfitMSQ, "i.infitMSQ" = i.infitMSQ, "i.outfitZ" = i.outfitZ, "i.infitZ" = i.infitZ), "p.fit" = list("p.outfitMSQ" = p.outfitMSQ, "p.infitMSQ" = p.infitMSQ, "p.outfitZ" = p.outfitZ, "p.infitZ" = p.infitZ))

  if(isAlpha){
    setting_par <- autoRaschOptions()
    setting_par$isHessian <- FALSE
    gpcm_res <- gpcm(X = X, setting = setting_par)
    res_fit[["alpha"]] <- exp(gpcm_res$gamma)
  }

  res_fit[["traceMat"]] <- list("emat" = Emat, "vmat" = Vmat, "cmat" = Cmat, "std.res" = st.res)

  class(res_fit) <- c("fit","autoRasch",class(res_fit))

  return(res_fit)
}


#' @param object The object of class \code{'pcm'}.
#' @param ... Further arguments to be passed.
#'
#' @rdname pcm
#' @export
summary.pcm <- function(object, ...){

  obj <- object

  dotdotdot <- list(...)

  if(!is.null(dotdotdot$par)){
    par <- dotdotdot$par
  } else {
    par <- NULL
  }

  if(is.null(par) | "theta" %in% par){
    cat("\n\n")
    cat("The estimated ability scores:")
    cat("\n")
    print(object$theta, ... = ...)
    cat("\n")
    cat("The highest ability score: ",round(max(object$theta,na.rm = TRUE),4))
    cat("\n")
    cat("The lowest ability score: ",round(min(object$theta,na.rm = TRUE),4))
  }

  if(is.null(par) | "beta" %in% par){
    cat("\n\n")
    cat("The estimated difficulty scores:")
    cat("\n")
    reported_beta <- unlist(tapply(obj$beta,rep(seq_along(obj$mt_vek),obj$mt_vek),function(x){
      if(length(x) < max(obj$mt_vek)){
        x <- c(x,rep(NA,(max(obj$mt_vek)-length(x))))
        x
      } else {
        x
      }
    }))
    beta_mat <- matrix(reported_beta, nrow = length(obj$mt_vek), byrow = TRUE)
    beta_mat <- as.data.frame(round(beta_mat,2), row.names = obj$itemName)
    colnames(beta_mat) <- paste("Th_",c(1:max(obj$mt_vek)),sep = "")
    beta_mat[["Item Loc."]] <- temp <- round(apply(beta_mat,1,mean,na.rm=TRUE),2)
    beta_mat$` ` <- apply(beta_mat[,1:max(obj$mt_vek)],1,function(x){if(is.unsorted(na.omit(x))){return("*")}else{return("")}})
    print(beta_mat, quote = FALSE)
    cat("\n")
    cat("The most difficult item: ",obj$itemName[which(temp == max(temp,na.rm = TRUE))])
    cat("\n")
    cat("The easiest item: ",obj$itemName[which(temp == min(temp,na.rm = TRUE))])
    cat("\n")
    ntd_items <- length(which(beta_mat[,ncol(beta_mat)] == "*"))
    cat("There are",ntd_items,"items which have disordered thresholds.")
    cat("\n")
    cat("'*' Item has disordered thresholds.")
    cat("\n")
  }
}

#' @param x The object of class \code{'pcm'}.
#'
#' @rdname pcm
#' @export
print.pcm <- function(x, ...){

  obj <- x

  dotdotdot <- list(...)

  if(!is.null(dotdotdot$par)){
    par <- dotdotdot$par
  } else {
    par <- NULL
  }

  if(is.null(par) | "theta" %in% par){
    cat("\n")
    cat("$theta")
    cat("\n")
    print(x$theta, ... = ...)
    cat("\n")
  }

  if(is.null(par) | "beta" %in% par){
    cat("\n")
    cat("$beta")
    cat("\n")
    print(x$beta, ... = ...)
    cat("\n")
  }

}
