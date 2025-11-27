#' Estimation of The Partial Credit Model with DIF
#'
#' This function computes the parameter estimates of a partial credit model with DIF for dichotomous and polytomous responses
#' by implementing the coordinate descent.
#'
#' @param X A matrix or data frame as an input with ordinal responses (starting from 0);
#' rows represent individuals, columns represent items.
#' @param init_par a vector of initial values of the estimated parameters.
#' @param groups_map Binary matrix. Respondents membership to DIF groups; rows represent individuals, column represent group partitions.
#' @param setting a list of the optimization control setting parameters.See \code{\link[autoRasch:autoRaschOptions]{autoRaschOptions()}}
#' @param method The implementation option of log likelihood function. \code{fast} using a \code{c++} implementation and \code{novel} using an \code{R} implementation.
#'
#' @return
#' \strong{\code{pcm_dif()} will return a \code{\link[base:list]{list}} which contains:}
#' \item{X}{   The dataset that is used for estimation.}
#' \item{mt_vek}{   A vector of the highest response given to items.}
#' \item{itemName}{   The vector of names of items (columns) in the dataset.}
#' \item{loglik}{   The log likelihood of the estimation.}
#' \item{hessian}{   The hessian matrix. Only when the \code{isHessian = TRUE}.}
#' \item{beta}{   A vector of the difficulty parameter of each categories of items (thresholds).}
#' \item{theta}{   A vector of the ability parameters of each individuals.}
#'
#' @seealso \code{\link{pcm}}, \code{\link{pcm_dif}}, \code{\link{gpcm}}, \code{\link{gpcm_dif}}
#'
#' @export
pcm_dif <- function(X, init_par = c(), groups_map = c(), setting = c(), method = c("fast","novel")){

  if(is.null(setting)){
    settingPar <- autoRaschOptions()
  } else {
    if("aR_opt" %in% class(setting)){
      settingPar <- setting
    } else {
      stop("The setting used should be a class of aR_opt!")
    }
  }

  settingPar$fixed_par <- c("gamma")
  if(is.null(setting$isHessian)){
    settingPar$isHessian <- FALSE
  } else {
    settingPar$isHessian <- setting$isHessian
  }
  settingPar$isPenalized_gamma <- FALSE

  if(is.null(setting$optz_method)){
    settingPar$optz_method <- "mixed"
  } else {
    settingPar$optz_method <- setting$optz_method
  }

  if(!is.null(setting$optim_control)){
    settingPar$optim_control <- setting$optim_control
  }

  if(is.null(groups_map)){
    if(is.null(settingPar$groups_map)){
      stop("groups_map must be designed to use the PCM-DIF!")
    }
  } else {
    settingPar$groups_map <- as.matrix(groups_map)
  }

  result <- pjmle(X = X, init_par = init_par, setting = settingPar, method = method)

  class(result) <- c("pcmdif","armodels","autoRasch",class(result))
  return(result)
}

#' Fit Statistics PCM-DIF
#'
#' \code{fitStats} compute the fit statistics (i.e., Outfit and Infit) of the PCM-DIF model estimation (items and persons).
#'
#' @param obj The object of class \code{'pcmdif'}.
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
#' @examples
#' \dontrun{
#' pcmdif_res <- pcm_dif(shortDIF, groups_map = c(rep(1,50),rep(0,50)))
#' fit_res <- fitStats(pcmdif_res)
#' itemfit(fit_res)
#' personfit(fit_res)
#' plot_fitStats(fit_res, toPlot = c("alpha","outfit"), useName = FALSE)
#' }
#'
#' @rdname pcm_dif
#' @export
fitStats.pcmdif <- function(obj, isAlpha = TRUE){

  if(is.null(dim(obj$X))){
    obj$X <- matrix(obj$X,ncol = 1)
  }
  min.x <- min(obj$X, na.rm = TRUE)
  X <- obj$X - min.x
  groups_map <- as.matrix(obj$groups_map)

  if(!is.null(obj$exclResp)){
    X <- X[-c(obj$exclResp),]
    groups_map <- groups_map[-c(obj$exclResp),]
  }

  # Map the parameters
  theta <- obj$theta
  beta <- obj$beta
  delta <- obj$delta
  mt_vek <- obj$mt_vek
  allcat <- sum(mt_vek)
  n.th <- max(mt_vek)


  delta.tot <- 0
  for(i in seq_len(ncol(groups_map))) {
    delta.tot <- delta.tot + outer(delta[(((i-1)*ncol(X))+1):(i*ncol(X))],groups_map[,i],"*")
  }
  delta.tot.rep <- rep.int((delta.tot), rep.int(obj$mt_vek,nrow(groups_map)))       #delta.tot.rep is total delta which has been replicated to every categoory

  xna.mat <- matrix(1,nrow = nrow(X), ncol = ncol(X))

  idx <- which(is.na(X))
  xna.mat[idx] <- NA
  XNA <- xna.mat

  t.diff <- outer((-beta), theta, "+")
  t.diff <- t.diff - delta.tot.rep
  disc.diff <- t.diff

  # per.cat.list <- matrix(disc.diff, nrow = n.th)
  #
  # temp.prob <- as.matrix(per.cat.list[1,])
  # temp.l2 <- exp(temp.prob)
  # temp.l1 <- exp(temp.l2*0)
  # temp.l1 <- cbind(temp.l1,temp.l2)
  # for(i in 2:n.th){
  #   temp.prob <- cbind(temp.prob,(temp.prob[,i-1]+per.cat.list[i,]))
  #   temp.l1 <- cbind(temp.l1,(exp(temp.prob[,i])))
  #   temp.l2 <- temp.l2 + (exp(temp.prob[,i]))
  # }

  mt_idx <- rep(seq_along(mt_vek), mt_vek)

  per.cat.list <- matrix(disc.diff, nrow = allcat)

  temp.l1 <- apply(per.cat.list, 2, function(x){
    l1.peritem <- tapply(x, mt_idx, function(y){
      temp <- (exp(cumsum(y)))
      temp <- c(1,temp)
    })
    l1.temp <- unlist(l1.peritem)
  })

  mt_vek0 <- mt_vek + 1
  allcat0 <- sum(mt_vek0, na.rm = TRUE)

  temp.l1 <- matrix(temp.l1,nrow = allcat0)

  temp.l2 <- apply(per.cat.list, 2, function(x){
    l2.peritem <- tapply(x, mt_idx, function(y){
      temp <- sum(exp(cumsum(y)), na.rm = TRUE)
      temp
    })
    l2.temp <- unlist(l2.peritem)
  })

  l2 <- (temp.l2+1)

  l1 <- as.vector((temp.l1))
  mult.mt_vek <- rep(mt_vek,nrow(X))+1
  l2 <- rep(l2, mult.mt_vek)

  pmat <- l1/l2

  mt_vek0 <- mt_vek + 1
  mt_idx0 <- rep(seq_along(mt_vek0),mt_vek0)
  mt_seq <- sequence(mt_vek0)-1

  # stop()

  Emat <- pmat * mt_seq
  # Emat <- matrix(Emat, nrow = (n.th+1))
  # Emat <- colSums(Emat)
  Emat <- matrix(Emat, nrow = (allcat0))
  Emat <- apply(Emat, 2, function(x){
    Emat.peritem <- tapply(x, mt_idx0, function(y){
      temp <- sum(y, na.rm = TRUE)
      temp
    })
    Emat.temp <- unlist(Emat.peritem)
  })

  Emat.cat <- rep(Emat, mult.mt_vek)

  Vvect.cat <- ((mt_seq - Emat.cat)^2)*pmat
  # Vmat.cat <- matrix(Vvect.cat, nrow = (n.th+1))
  # Vmat <- colSums(Vmat.cat)
  Vmat.cat <- matrix(Vvect.cat, nrow = (allcat0))
  Vmat <- apply(Vmat.cat, 2, function(x){
    Vmat.peritem <- tapply(x, mt_idx0, function(y){
      temp <- sum(y, na.rm = TRUE)
      temp
    })
    Vmat.temp <- unlist(Vmat.peritem)
  })

  Cvect.cat <- ((mt_seq - Emat.cat)^4)*pmat
  # Cmat.cat <- matrix(Cvect.cat, nrow = (n.th+1))
  # Cmat <- colSums(Cmat.cat)
  Cmat.cat <- matrix(Cvect.cat, nrow = (allcat0))
  Cmat <- apply(Cmat.cat, 2, function(x){
    Cmat.peritem <- tapply(x, mt_idx0, function(y){
      temp <- sum(y, na.rm = TRUE)
      temp
    })
    Cmat.temp <- unlist(Cmat.peritem)
  })

  # Emat <- t(matrix(Emat, nrow = ncol(X)))
  # Vmat <- t(matrix(Vmat, nrow = ncol(X)))
  # Cmat <- t(matrix(Cmat, nrow = ncol(X)))

  if(ncol(X) == 1){
    Emat <- matrix(Emat,nrow = 1)
    Vmat <- matrix(Vmat,nrow = 1)
    Cmat <- matrix(Cmat,nrow = 1)
  }

  Emat <- t((Emat))
  Vmat <- t((Vmat))
  Cmat <- t((Cmat))

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
    gpcmdif_res <- gpcm_dif(X = X, groups_map = groups_map)
    res_fit[["alpha"]] <- exp(gpcmdif_res$gamma)
  }

  res_fit[["traceMat"]] <- list("emat" = Emat, "vmat" = Vmat, "cmat" = Cmat, "std.res" = st.res)

  class(res_fit) <- c("fit","autoRasch",class(res_fit))
  return(res_fit)
}

#' @param object The object of class \code{'pcmdif'}.
#' @param ... Further arguments to be passed.
#'
#' @rdname pcm_dif
#' @export
summary.pcmdif <- function(object, ...){

  obj <- object

  dotdotdot <- list(...)

  if(!is.null(dotdotdot$par)){
    par <- dotdotdot$par
  } else {
    par <- NULL
  }

  if(!is.null(dotdotdot$th_dif)){
    th_dif <- dotdotdot$th_dif
  } else {
    th_dif <- 1e-5
  }

  if(is.null(par) | "theta" %in% par){
    cat("\n\n")
    cat("The estimated ability scores (theta):")
    cat("\n")
    print(round(obj$theta,2))
    cat("\n")
    cat("The highest ability score: ",round(max(obj$theta,na.rm = TRUE),2))
    cat("\n")
    cat("The lowest ability score: ",round(min(obj$theta,na.rm = TRUE),2))
    cat("\n")
    cat("Respondent(s) no. ",paste(obj$exclResp,collapse = ",")," are excluded due to missing (incomplete) background information.")
  }

  if(is.null(par) | "beta" %in% par){
    cat("\n\n")
    cat("The estimated difficulty scores (beta):")
    cat("\n")
    # reported_beta <- obj$beta * obj$real_vek
    reported_beta <- unlist(tapply(obj$beta,rep(seq_along(obj$mt_vek), obj$mt_vek),function(x){
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
  }

  if(is.null(par) | "delta" %in% par){
    cat("\n\n")
    cat("The estimated DIF effects (gap of difficulties) (delta):")
    cat("\n")
    # if(is.null(th_dif)){
    #   th_dif <- 1e-5
    # }
    delta_mat <- matrix(round(obj$delta,2), ncol = ncol(obj$groups_map), dimnames = list(c(obj$itemName),colnames(obj$groups_map)))
    delta_mat[which(abs(delta_mat) < th_dif)] <- ""
    remRowIdx <- which(unlist(
      lapply(
        apply(delta_mat,1,function(x){
          which(x != "")
          }),
        function(x){
          if(length(x)!=0){
            return(1)
          } else {
              return(0)
          }
        }
      )
    ) == 1)

    delta_mat <- as.data.frame(delta_mat)[remRowIdx, , drop = FALSE]
    if(nrow(delta_mat) == 0){
      cat("There is no differential functioning in items found.")
    } else {
      print(delta_mat, quote = FALSE)
    }
    cat("\n")
    # cat("DIF effect threshold =",th_dif)
  }
}

#' @param x The object of class \code{'pcmdif'}.
#'
#' @rdname pcm_dif
#' @export
print.pcmdif <- function(x, ...){

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
    print(obj$theta)
    cat("\n")
  }

  if(is.null(par) | "beta" %in% par){
    cat("\n")
    cat("$beta")
    cat("\n")
    print(obj$beta)
    cat("\n")
  }

  if(is.null(par) | "delta" %in% par){
    cat("\n")
    cat("$delta")
    cat("\n")
    print(obj$delta)
    cat("\n")
  }

}
