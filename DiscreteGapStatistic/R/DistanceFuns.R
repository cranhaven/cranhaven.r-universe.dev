#' Calculate categorical distance matrix for discrete data
#'
#' Function invoking discrete distance functions.
#' Available distances: 'bhattacharyya', 'chisquare', 'cramerV', 'hamming' and 'hellinger'
#'
#' @param X Matrix where rows are the observations and columns are discrete features
#' @param d Name of distance. Distances available: bhattacharyya, chisquare, cramerV, hamming and hellinger
#'
#' @return R distance object
#' @export
#'
#' @examples
#' X = rbind(matrix(paste0("a", rpois(7*5, 1)), nrow=5),
#'           matrix(paste0("a", rpois(7*5, 3)), nrow=5))
#' distancematrix(X = X, d = "hellinger")
distancematrix <- function (X, d){

    if(d == 'bhattacharyya')
        return(dissbhattacharyya(X))
    if(d == 'chisquare')
        return(disschisquare(X))
    if (d == "cramerV")
         return(disscramerv(X))
    if (d == "hamming") ## General version
        return(disshamming(X))
    if (d == "hellinger")
        return(disshellinger(X))

    stop("Distance metric ", d, " not available")
}

#' Bhattacharyya distance
#'
#' Bhattacharyya distance core function
#'
#' @param x Matrix
#' @param adj Small quantity added to avoid indefinite log(0) values. DEFAULT=0.001
#'
#' @return Distance R object
#' @export
BhattacharyyaDist <- function(x, adj = 0.01){
    nRow <- nrow(x)
    myP <- ncol(x)
    compInd <- lapply(sort(unique(as.vector(x))),
                      function(k) diag((x == k) %*% t(x == k))/myP)
    names(compInd) <- sort(unique(as.vector(x)))

    myCoords <- utils::combn(1:nRow, m = 2)
    BC <- sapply(compInd,
                 function(x) utils::combn(x, m = 2,
                                   FUN=function(x){
                                       preSq <- sqrt(prod(x))
                                   }))
    if(length(dim(BC)) == 2)
        BC <- -log(apply(BC, 1, sum) + adj)
    else
        BC <- -log(sum(BC) + adj)

    outMat <- matrix(rep(0, nRow^2), nrow = nRow )
    for(k in 1:ncol(myCoords)){
        outMat[myCoords[1, k], myCoords[2, k]] <- BC[k]
    }
    outMat <- outMat + t(outMat)
    outMat[outMat < 0] <- 0 ## remove adjustment from identical distribs
    stats::as.dist(outMat)
}

#' Bhattacharyya's distance (wrapper)
#'
#' Wrapper of `BhattacharyyaDist`
#'
#' @param X Matrix
#'
#' @return Distance R object
#' @export
dissbhattacharyya  <-  function (X) {

    if (!is.matrix(X)) {
        stop(paste(sQuote("X"), "not a matrix"))
    }

    BhattacharyyaDist(x=X)
}

#' Chi-square distance
#'
#' Chi-square distance core function
#'
#' @param x Matrix
#'
#' @return Distance R object
#' @export
ChisqDist <- function(x){
        nRow <- nrow(x)
        myP <- ncol(x)
        ## Count incidences of all categories
        ## List of Frequencies of categories per subject
        compInd <- lapply(sort(unique(as.vector(x))),
                          function(k) diag((x == k) %*% t(x == k)))
        names(compInd) <- sort(unique(as.vector(x)))

        myCoords <- utils::combn(1:nRow, m = 2)
        SqDiff <- sapply(compInd,
                         function(x) utils::combn(x, m = 2,
                                           FUN=function(x){
                                               preSq <- (diff(x)^2)/sum(x)
                                               ifelse(is.nan(preSq), 0, preSq)
                                           }))

        if(is.numeric(SqDiff) & !any(class(SqDiff) == 'matrix'))
            SqDiff <- sum(SqDiff)
        else
            SqDiff <- apply(SqDiff, 1, sum)

        outMat <- matrix(rep(0, nRow^2), nrow =nRow )
        for(k in 1:ncol(myCoords)){
            outMat[myCoords[1, k], myCoords[2, k]] <- SqDiff[k]
        }
        outMat <- outMat + t(outMat)
        stats::as.dist(outMat/myP)
}

#' Chi-square distance (wrapper)
#'
#' Wrapper of `ChisqDist`
#'
#' @param X Matrix
#'
#' @return Distance R object
#' @export
disschisquare  <-  function (X) {

    if (!is.matrix(X)) {
        stop(paste(sQuote("X"), "not a matrix"))
    }
    ChisqDist(X)
}

#' Cramer's V modified pairwise vector function based on the function found in lsr package
#'
#' This is simple wrapper of the usual chisq.test function.
#' This is actually an adjusted version of the pi = sqrt(Chisq2/N)
#' guaranteeing that values are within 0 (no association) and 1 (association)
#'
#' @param x vector of size n
#' @param y vector of size n
#'
#' @return numerical value
#' @export
cramersVmod <- function(x, y){
   if(identical(x, y)){
      return(1)
   }else if(length(unique(x)) == 1 | length(unique(y)) == 1 ){
      return(0)
   }else if(all(is.na(x)) | all(is.na(y))){
      return(NA)
   }else{
      test <- stats::chisq.test(x=x, y=y, correct=FALSE)
      chi2 <- test$statistic
      N <- sum(test$observed)
      k <- min(dim(test$observed))
      V <- sqrt(chi2/(N * (k - 1)))
      names(V) <- NULL
      return(V)
   }
}

#' Cramer's V distance
#'
#' Cramer's V core function
#'
#' @param X matrix
#'
#' @return Distance matrix
#' @export
CramerV <- function(X){
    myR = nrow(X)
    matOut <- matrix(NA, nrow=myR, ncol=myR)

    utils::combn(myR, 2,
          function(x){
              ## aux <- suppressWarnings(lsr::cramersV(X[x[1], ], X[x[2], ]))
              aux <- suppressWarnings(cramersVmod(X[x[1], ], X[x[2], ]))
              matOut[x[1], x[2]] <<- aux
          })
    matOut <- t(matOut)
    stats::as.dist(matOut)
}

#' Cramer's V distance (wrapper)
#'
#' Wrapper of `CramerV`
#'
#' @param X Matrix
#'
#' @return Distance R object
#' @export
disscramerv <-  function (X) {

    if (!is.matrix(X)) {
        stop(paste(sQuote("X"), "not a matrix"))
    }

    1 - CramerV(X)
}

#' Hamming distance wrapper function
#'
#' Function based on cultevo's package implementation
#'
#' @param X matrix
#'
#' @return Distance matrix
#' @export
disshamming <- function (X){

   out <- cultevo::hammingdists(X)/ncol(X)
   return(out)
}

#' Hellinger distance
#'
#' Hellinger distance core function
#' @param x matrix
#'
#' @return Distance matrix
#' @export
HellingerDist <- function(x){
    nRow <- nrow(x)
    myP <- ncol(x)
    compInd <- lapply(sort(unique(as.vector(x))),
                      function(k) rowSums(x == k) )
   names(compInd) <- sort(unique(as.vector(x)))

    myCoords <- utils::combn(1:nRow, m = 2)
    He <- sapply(compInd,
                 function(x) utils::combn(x, m = 2,
                                   FUN=function(x){
                                       preSq <- (diff(sqrt(x)))^2
                                   }))
    if(length(dim(He)) == 2)
        He <- (1/sqrt(2*myP))*sqrt(apply(He, 1, sum))
    else
        He <- (1/sqrt(2*myP))*sqrt(sum(He))

    outMat <- matrix(rep(0, nRow^2), nrow = nRow )
    for(k in 1:ncol(myCoords)){
        outMat[myCoords[1, k], myCoords[2, k]] <- He[k]
    }
    outMat <- outMat + t(outMat)
    outMat[outMat < 0] <- 0 ## remove adjustment from identical distribs
    stats::as.dist(outMat)
}

#' Hellinger distance (wrapper)
#'
#' Wrapper of `HellingerDist`
#'
#' @param X Matrix
#'
#' @return Distance R object
#' @export
disshellinger  <-  function (X) {
    if (!is.matrix(X)) {
        stop(paste(sQuote("X"), "not a matrix"))
    }

    HellingerDist(X)
}
