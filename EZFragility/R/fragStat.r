# Utility functions

calcStat <- function(mat){
    if (nrow(mat) == 0 || ncol(mat) == 0) {
        return(list(mean = numeric(0), sd = numeric(0), sem = numeric(0)))
    }

    list(
        mean = colMeans(mat, na.rm = TRUE),
        sd = apply(mat, 2L, sd, na.rm = TRUE),
        sem = apply(mat, 2L, function(x) sd(x, na.rm = TRUE)/sqrt(length(x)))
    )
}








##############################

#' Compute the normalized fragility row for adjacency matrix A
#' 
#' The matrix A is used for the regression: A * x(t) = x(t+1)
#'
#' @param A Numeric. Adjacency Matrix
#' @param nSearch Integer. Number of eigenvalues tried to find the minimum norm vector
#' @param normalize Logical. If TRUE, the fragility row is normalized
fragilityRow <- function(A, nSearch = 100, normalize = TRUE) {
    elecNum <- nrow(A)
    ek <- diag(elecNum)
    b <- matrix(c(0, -1), 2L)
    fragNorm <- rep(0L, elecNum)
    omvec <- seq(0L, 1L, length.out = nSearch + 1L)[-1L]
    lambdas <- sqrt(1 - omvec^2) + omvec * 1i
    ## (A - (sigma + j * omega)*I)^-1
    iMats <- lapply(lambdas, \(L) solve(A - L * ek))
    for (i in seq_len(elecNum)) {
        minNorm <- 100000
        item <- ek[i, , drop = FALSE]
        for (k in seq_len(nSearch)) {
            argument <- item %*% iMats[[k]]
            B <- rbind(Im(argument), Re(argument))
            ## B^T * (B * B^T)^-1 * b
            prov <- t(B) %*% solve(B %*% t(B)) %*% b
            provNorm <- norm(prov, type = "2")
            if (provNorm < minNorm) minNorm <- provNorm
        }
        fragNorm[i] <- minNorm
    }
    if (!normalize) {
        return(fragNorm)
    }
    maxf <- max(fragNorm)
    ## TODO: find another way to normalize the fragility row
    ## The current implementation reverse the meaning of the fragility
    ## TODO: add both frag and fragNormalized to Fragility
    (maxf - fragNorm) / maxf
}


#' Compute quantiles, mean and standard deviation for two electrodes groups
#'
#' @param frag A Fragility object from \code{calcAdjFrag}
#' @param groupIndex Integer or string. A group of electrodes to mark 
#' @param groupName Character. Name of the group of electrodes, default is "SOZ"
#' @param ranked Logical. If TRUE, use the ranked fragility matrix from Fragility object
#'
#' @return list of 5 items with quantile matrix, mean and sdv from both electrodes groups
#'
#' @examples
#' data("pt01Frag")
#' data("pt01EcoG")    
#' ## sozNames is the name of the electrodes we assume are in the SOZ
#' sozNames <- metaData(pt01EcoG)$sozNames
#' pt01fragstat <- fragStat(frag = pt01Frag, groupIndex = sozNames)
#' @export 
fragStat <- function(frag, groupIndex = NULL, groupName="SOZ", ranked=FALSE) {
    stopifnot(is(frag, "Fragility"))
    groupIndex <- checkIndex(groupIndex, frag$electrodes)
    
    fragMat <- .ifelse(ranked, frag$frag_ranked, frag$frag)
    stopifnot(is.matrix(fragMat))

    steps <- ncol(fragMat)
    refIndex <- setdiff(seq_len(nrow(fragMat)), groupIndex)

    groupMat <- fragMat[groupIndex, , drop = FALSE]
    refMat <- fragMat[refIndex, , drop = FALSE]
    groupStat <- calcStat(groupMat)
    refStat <- calcStat(refMat)

    Q <- seq(.1, 1, by = .1)
    qmatrix <- rbind(
        apply(groupMat, 2, quantile, Q),
        apply(refMat, 2, quantile, Q)
    )
    
    rowPrefix <- rep(c(groupName, "REF"), each = 10)
    dimN <- dimnames(qmatrix)
    dimnames(qmatrix) <- list(
        Quantiles = paste0(rowPrefix, dimN[[1L]]),
        Step      = dimN[[2L]]
    )
    FragStat(
        qmatrix   = qmatrix,
        groupMean  = groupStat$mean,
        refMean = refStat$mean,
        groupSD    = groupStat$sd,
        refSD   = refStat$sd,
        groupSEM   = groupStat$sem,
        refSEM = refStat$sem
    )
}

