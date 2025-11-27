#' Fit statistics
#'
#' The goodness-of-fit statistics of Rasch analysis for items and persons. It consists of Outfit (Unweighted) Mean Square,
#' Infit (Weighted) Mean Square, Outfit ZSTD (Standardized Unweighted Mean Square), and Outfit ZSTD (Standardized Weighted Mean Square)
#'
#' @param obj The object of class \code{'pcm'} or \code{'pcmdif'}.
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
#' \emph{traceMat}{   Some computed matrices in the process. Only if \code{isTraced = TRUE}}
#' \itemize{
#'    \item{emat}{   The expected values matrix.}
#'    \item{vmat}{   The variance matrix.}
#'    \item{cmat}{   The curtosis matrix.}
#'    \item{std.res}{   The standardized residual.}
#' }
#'
#' @rdname fit
#' @export
fitStats <- function (obj, isAlpha = TRUE) {

  if(!("pcm" %in% class(obj)) & !("pcmdif" %in% class(obj))){
    stop("autoRasch ERROR: itemfit is only for pcm and pcmdif object.")
  }
  UseMethod("fitStats", obj)
}

#' @param object The object of class \code{'fit'}.
#' @param ... Further arguments to be passed.
#'
#' @rdname fit
#' @export
summary.fit <- function(object, ...){

  obj <- object

  dotdotdot <- list(...)

  if(!is.null(dotdotdot$type)){
    type <- dotdotdot$type
  } else {
    type <- NULL
  }

  if(is.null(type) | "item" %in% type){
    i.mat <- cbind(obj$i.fit$i.outfitMSQ, obj$i.fit$i.infitMSQ, obj$i.fit$i.outfitZ, obj$i.fit$i.infitZ)
    if(!is.null(obj$alpha)){
      i.mat <- cbind(i.mat, obj$alpha)
      dimnames(i.mat) <- list(c(names(obj$i.fit$i.outfitMSQ)),c("OutfitMnSq","InfitMnSq","OutfitZSTD","InfitZSTD","Alpha"))
    } else {
      dimnames(i.mat) <- list(c(names(obj$i.fit$i.outfitMSQ)),c("OutfitMnSq","InfitMnSq","OutfitZSTD","InfitZSTD"))
    }
    cat("\n")
    cat("Item Fit Statistics:")
    cat("\n\n")
    print(round(i.mat,2))
    cat("\n\n")
  }

  if(is.null(type) | "person" %in% type){
    p.mat <- cbind(obj$p.fit$p.outfitMSQ, obj$p.fit$p.infitMSQ, obj$p.fit$p.outfitZ, obj$p.fit$p.infitZ)
    dimnames(p.mat) <- list(c(paste("P", seq_along(obj$p.fit$p.outfitMSQ),sep = "")), c("OutfitMSQ","InfitMSQ","OutfitZ","InfitZ"))
    cat("Person Fit Statistics:")
    cat("\n\n")
    print(round(p.mat,2))
    cat("\n")
  }

}

#' @param objFit The object of class \code{'fit'}.
#'
#' @rdname fit
#' @export
itemfit <- function(objFit){
  summary(objFit, type = "item")
}

#' @rdname fit
#' @export
personfit <- function(objFit){
  summary(objFit, type = "person")
}

#' @param toPlot An array with length two \code{c(x,y)}, to choose what to plot. There are five options to plot, which are alpha, outfit, infit, outfitz, and infitz
#' @param  useName A logical statement whether the name of the variable are going to be used in the plot instead of the variable order.
#'
#' @rdname fit
#' @export
plot_fitStats <- function(objFit, toPlot = c("alpha","infit"), useName = FALSE, ...){

  obj <- x <- objFit

  if(!"fit" %in% class(obj) & !"autoRasch" %in% class(obj)){
    stop("The input should be an object of class 'fit'")
  }

  dotdotdot <- list(...)

  if(!is.null(dotdotdot$use.name)){
    use.name <- dotdotdot$use.name
  } else {
    use.name <- FALSE
  }

  if(!is.null(dotdotdot$font.text)){
    font.text <- dotdotdot$font.text
  } else {
    font.text <- 1
  }

  if(length(toPlot) < 2){
    plotx <- "alpha"
    ploty <- "infit"
  } else {
    plotx <- toPlot[1]
    ploty <- toPlot[2]
  }

  if(plotx == "outfit"){
    plotx <- x$i.fit$i.outfitMSQ
    x.lab <- "OutfitMnSq"
  } else if(plotx == "infit"){
    plotx <- x$i.fit$i.infitMSQ
    x.lab <- "InfitMnSq"
  } else if(plotx == "outfitz"){
    plotx <- x$i.fit$i.outfitZ
    x.lab <- "OutfitZSTD"
  } else if(plotx == "infitz"){
    plotx <- x$i.fit$i.infitZ
    x.lab <- "InfitZSTD"
  } else if(plotx == "alpha"){
    plotx <- obj$alpha
    # x.lab <- expression(paste("alpha (",alpha,")"))
    # x.lab <- expression(paste("estimated ",alpha[i]))
    x.lab <- expression(hat(alpha))
  } else {
    stop("Wrong value toPlot!")
  }
  if(ploty == "outfit"){
    ploty <- x$i.fit$i.outfitMSQ
    y.lab <- "OutfitMnSq"
  } else if(ploty == "infit"){
    ploty <- x$i.fit$i.infitMSQ
    y.lab <- "InfitMnSq"
  } else if(ploty == "outfitz"){
    ploty <- x$i.fit$i.outfitZ
    y.lab <- "OutfitZSTD"
  } else if(ploty == "infitz"){
    ploty <- x$i.fit$i.infitZ
    y.lab <- "InfitZSTD"
  } else if(ploty == "alpha"){
    ploty <- obj$alpha
    y.lab <- expression(hat(alpha))
  } else {
    stop("Wrong value toPlot!")
  }


  if(useName){
    text <- names(obj$i.fit$i.outfitMSQ)
  }


  if(!is.null(dotdotdot$xlab)){
    x.lab <- dotdotdot$xlab
  }

  if(!is.null(dotdotdot$ylab)){
    y.lab <- dotdotdot$ylab
  }



  if(!is.null(dotdotdot$xlab) | !is.null(dotdotdot$ylab)){
    suppressWarnings({plot(plotx, ploty, type = "n", ... = ...)})
  } else {
    suppressWarnings({plot(plotx, ploty, xlab = x.lab, ylab = y.lab, type = "n", ... = ...)})
  }

  if(useName){
    suppressWarnings({text(plotx, ploty, labels = text, ... = ...)})
  } else {
    suppressWarnings({text(plotx, ploty, ... = ...)})
  }

}


#' Residual Correlation
#'
#' Compute the correlation of the standardized residual to check the local dependency status
#'
#' @param objFit object of class "fit", the output of \code{fitStats()}.
#'
#' @return
#' \item{ld_correl}{  Correlation matrix of the standradized residual.}
#' \item{ld_mean}{  The mean of the correlation.}
#' \item{ld_lowertri}{  The lower triangle of the correlation matrix.}
#'
#' @rdname ld
#'
#' @export
residCor <- function(objFit){
  # if(!is.object(objFit$traceMat)){
  #   stop("Please compute Fitness object using isTrace = TRUE!")
  # }
  corLD <- cor(objFit$traceMat$std.res, use = "pairwise.complete.obs")
  class(corLD) <- c("ld",class(corLD))
  return(corLD)
}

#' @rdname ld
#'
#' @export
corResid <- function(objFit){
  corLD <- residCor(objFit)
  return(corLD)
}

#' @param object The object of class \code{'ld'}.
#' @param ... Further arguments to be passed.
#'
#' @rdname ld
#' @export
summary.ld <- function(object, ...){
  dotdotdot <- list(...)
  if(!is.null(dotdotdot$LDth)){
    LDth <- c(dotdotdot$LDth)
  } else {
    LDth <- c(0.3)
  }
  if(length(LDth) == 1){
    idx <- which((object >= LDth & object < 1), arr.ind = TRUE)
    idx <- idx[which(!duplicated(t(apply(idx,1,sort)))),]
    numLD <- nrow(idx)#/2

    cat("\nCorrelation of the standardized residual: \n\n")
    cat("There are",numLD,"pair(s) of variable(s) with correlation >=",LDth,"\n")

    if(numLD > 0){
      # idx <- idx[1:(nrow(idx)/2),]
      for (i in 1:numLD) {
        txt <- paste(i,". ",colnames(object)[idx[i,1]]," and ",colnames(object)[idx[i,2]],sep = "")
        cat(txt,"\n")
      }
    }
  } else {
    idxPos <- which((object >= LDth[1] & object < 1), arr.ind = TRUE)
    idxPos <- idxPos[which(!duplicated(t(apply(idxPos,1,sort)))),]
    numLD <- nrow(idxPos)#/2

    cat("\nCorrelation of the standardized residual: \n\n")
    cat("There are",numLD,"pair(s) of variable(s) with correlation >=",LDth[1],"\n")

    if(numLD > 0){
      # idx <- idx[1:(nrow(idx)/2),]
      for (i in 1:numLD) {
        txt <- paste(i,". ",colnames(object)[idxPos[i,1]]," and ",colnames(object)[idxPos[i,2]],sep = "")
        cat(txt,"\n")
      }
    }

    idxNeg <- which((object <= LDth[2]), arr.ind = TRUE)
    idxNeg <- idxNeg[which(!duplicated(t(apply(idxNeg,1,sort)))),]
    numLD <- nrow(idxNeg)#/2

    cat("\nThere are",numLD,"pair(s) of variable(s) with correlation <=",LDth[2],"\n")

    if(numLD > 0){
      # idx <- idx[1:(nrow(idx)/2),]
      for (i in 1:numLD) {
        txt <- paste(i,". ",colnames(object)[idxNeg[i,1]]," and ",colnames(object)[idxNeg[i,2]],sep = "")
        cat(txt,"\n")
      }
    }
  }

}

# resid_corr <- function(objFit){
#   # if(!is.object(objFit$traceMat)){
#   #   stop("Please compute Fitness object using isTrace = TRUE!")
#   # }
#   corLD <- cor(objFit$traceMat$std.res, use = "pairwise.complete.obs")
#   ld <- mean((corLD[lower.tri(corLD)]), na.rm = TRUE)
#   return(list("ld_correl" = corLD, "ld_mean" = ld, "ld_lowertri" = corLD[lower.tri(corLD)]))
# }
