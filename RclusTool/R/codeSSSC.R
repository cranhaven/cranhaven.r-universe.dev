# RclusTool: clustering of items in datasets
#
# Copyright 2013 Guillaume Wacquet, Pierre-Alexandre Hebert, Emilie Poisson-Caillault
#                
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

# KwaySSSC (semi-supervised algorithm)
# (Not integrated into GUI -> memory problem)
#' KwaySSSC returns a partition obtained by semi-supervised spectral clustering (according to Wacquet et al., 2013)
#' @title Semi-supervised spectral clustering
#' @description Perform semi-supervised spectral clustering thanks to a similarity matrix and constraints sets (according to Wacquet et al., 2013).
#' @param sim similarity matrix.
#' @param K number of clusters (0 for automatic estimation).
#' @param list.ML list of ML (must-link) constrained pairs.
#' @param list.CNL list of CNL (cannot-link) constrained pairs.
#' @param alphas numeric vector for the weight of constraints considered.
#' @param K.max maximal number of clusters.
#' @return The function returns a list containing:
#' \item{alpha}{chosen alpha.}
#' \item{label}{vector of labels.}
#' \item{medoids}{matrix of cluster centers in the space of the K first normalized eigenvectors.}
#' \item{id.med}{vector containing the medoids indices.}
#' \item{x}{matrix containing, in columns, the eigenvectors of the similarity matrix.}
#' \item{eigen.val}{vector containing the eigenvalues of the similarity matrix.}
#' \item{cluster.info}{some statistics on each cluster.}
#' \item{K}{number of clusters.}
#' @references G. Wacquet, E. Poisson Caillault, D. Hamad, P.-A. Hebert, Constrained spectral embedding for K-way data clustering, Pattern Recognition Letters, 2013, 34 (9), pp.1009-1017.
#' @seealso \code{\link{computeSemiSupervised}}, \code{\link{computeCKmeans}}
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#'            
#' ML <- list()
#' ML[[1]] <- c(sel="10",mem="20")
#' ML[[2]] <- c(sel="60",mem="70")
#' 
#' CNL <- list()
#' CNL[[1]] <- c(sel="30",mem="80")
#' CNL[[2]] <- c(sel="90",mem="120")
#' 
#' sim <- computeGaussianSimilarity(dat, 1)
#' res <- KwaySSSC(sim, K=0, list.ML=ML, list.CNL=CNL)
#' 
#' plot(dat[,1], dat[,2], type = "p", xlab = "x", ylab = "y", 
#'	col = res$label, main = "Constrained spectral clustering")
#'
#' @keywords internal
#' 
KwaySSSC <- function (sim, K=0, list.ML=list(), list.CNL=list(),
                      alphas=seq(from=0, to=1, length=100), K.max=20) {  
    # Variables initialization
    #   Avoid problematic alphas
    alphas[which(alphas<1e-3)] <- 1e-3
    alphas[which(alphas>(1-1e-3))] <- 1-1e-3
    alphas <- sort(unique(alphas))
    res <- list() #clustering results
    measures <- rep(0, length(alphas))
    names(measures) <- alphas
    C <- buildConstraintsMatrix(nrow(sim), list.ML=list.ML, list.CNL=list.CNL) 
    for (i in 1:length(alphas)) {
        W <- (1-alphas[i])*sim + alphas[i]*C
        k <- K
        if (K==0) {
            res.gap <- computeGap(W, K.max)
            k <- res.gap$Kmax
        }

        res[i] <- list(spectralClusteringNg(W,k))
        res[[i]]$K <- k

        #compute Laplacian matrix
        #??  	I <- diag(1, nrow(sim), ncol(sim))
        #??  	L <- I-D %*% W %*% D
        #??	  s <- svd(Z)
        #projection on unit-sphere
        #??  	Zn <- s$u/apply(s$u,MARGIN=1,FUN=function(x) norm(matrix(x),"f"))
        #clustering
        #??  	cl <- kmeans(Zn, centers=K, iter.max = 10, nstart = 10)
        #??   part[i,] <- cl$cluster
        rates.ok <- measureConstraintsOk(label=res[[i]]$label, list.ML=list.ML, list.CNL=list.CNL)
        MNCut <- measureMNCut(sim, label=res[[i]]$label)
        measures[i] <- ((1-MNCut) + rates.ok$ML + rates.ok$CNL)/3
        message(paste("total score =", measures[i], "; 1-MNCut =", 1-MNCut, "; ML ok =", rates.ok$ML,
                    "; CNL ok =", rates.ok$CNL))
        if ((rates.ok$CNL+rates.ok$ML) == 2)
            break
    }
    id <- which.max(measures)
    message(paste("choosen alpha =", alphas[id]))
    list(alpha = alphas[id], label=res[[id]]$label, medoids=res[[id]]$medoids,
         id.med=res[[id]]$id.med, x=res[[id]]$x, eigen.val=res[[id]]$eigen.val,
         cluster.info=res[[id]]$cluster.info, measure=measures[id], K=res[[id]]$K)
}

#' measureConstraintsOk returns the rates of constraints satisfaction
#' @title Rates of constraints satisfaction
#' @description Calculate the rates of ML and CNL constraints satisfaction in a clustering result.
#' @param label vector of labels.
#' @param list.ML list of ML (must-link) constrained pairs.
#' @param list.CNL list of CNL (cannot-link) constrained pairs.
#' @return The function returns a list containing:
#' \item{ML}{rate of ML (must-link) constraints satisfaction.}
#' \item{CNL}{rate of CNL (cannot-link) constraints satisfaction.}
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#'            
#' ML <- list()
#' ML[[1]] <- c(sel="10",mem="20")
#' ML[[2]] <- c(sel="60",mem="70")
#' 
#' CNL <- list()
#' CNL[[1]] <- c(sel="30",mem="80")
#' CNL[[2]] <- c(sel="90",mem="120")
#' 
#' sim <- computeGaussianSimilarity(dat, 1)
#' res <- KwaySSSC(sim, K=0, list.ML=ML, list.CNL=CNL)
#' 
#' measureConstraintsOk(res$label, list.ML=ML, list.CNL=CNL)
#'
#' @keywords internal 
#' 
measureConstraintsOk <- function(label, list.ML=list(), list.CNL=list()) {
    nb.ml.ok <- nb.cnl.ok <- 0

    for (pair in list.ML)
        nb.ml.ok <- nb.ml.ok + as.integer(label[pair[1]] == label[pair[2]])
    for (pair in list.CNL)
        nb.cnl.ok <- nb.cnl.ok + as.integer(label[pair[1]] != label[pair[2]])

    ml.ok <- cnl.ok <- 1

    if (length(list.ML)!=0)
        ml.ok <- nb.ml.ok/length(list.ML)
    if (length(list.CNL)!=0)
        cnl.ok <- nb.cnl.ok/length(list.CNL)

    list(ML=ml.ok, CNL=cnl.ok)
}

# measureMNCut (K-Way Normalized Cut criterion)
#' measureMNCut returns the Multiple Normalized Cut value
#' @title Multiple Normalized Cut
#' @description Calculate the Multiple Normalized Cut value from a similarity matrix.
#' @param sim similarity matrix.
#' @param label vector of labels.
#' @return MNCut Multiple Normalized Cut value.
#' 
#' @examples
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#'            
#' sim <- computeGaussianSimilarity(dat, 1)
#'
#' ML <- list(c("10","20"),  c("60","70"))
#' CNL <- list(c("30","80"), c("90","120"))
#'
#' res <- KwaySSSC(sim, K=0, list.ML=ML, list.CNL=CNL)
#' 
#' measureMNCut(sim, res$label)
#'
#' @keywords internal
#' 
measureMNCut <- function(sim, label) {
    classes <- unique(label)
    d <- rowSums(sim)
    MNCut <- 0
    e0 <- rep(0, length=nrow(sim))
    for (i in classes) {
        e <- e0
        e[label==i] <- 1
        MNCut <- MNCut + (1-((t(e)%*%sim%*%e)/(t(e)%*%(e*d))))
    }
    MNCut <- MNCut/length(classes)
}

# critMNCut (K-Way Normalized Cut criterion)
#' critMNCut returns the Multiple Normalized Cut value
#' @title Multiple Normalized Cut
#' @description Calculate the Multiple Normalized Cut value from a similarity matrix.
#' @param sim similarity matrix.
#' @param label vector of labels.
#' @return MNCut Multiple Normalized Cut value.
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#'            
#' sim <- computeGaussianSimilarity(dat, 1)
#'
#' ML <- list(c("10","20"),  c("60","70"))
#' CNL <- list(c("30","80"), c("90","120"))
#'
#' res <- KwaySSSC(sim, K=0, list.ML=ML, list.CNL=CNL)
#' 
#' critMNCut(sim, res$label)
#'
#' @keywords internal 
#' 
critMNCut <- function(sim, label) {
    classes <- unique(label)
    D <- diag(rowSums(sim))
    MNCut <- 0
    for (i in classes) {
        e <- label==i
        MNCut <- MNCut + (1-((t(e)%*%sim%*%e)/(t(e)%*%D%*%e)))
    }
    as.numeric(MNCut/length(classes))
}

#' buildConstraintsMatrix generate constraints matrix, based on ML and CNL sets
#' @title Constraints matrices
#' @description Compute the constraints matrix C, based on ML and CNL sets.
#' @param n total number of observations.
#' @param list.ML list of ML (must-link) constrained pairs.
#' @param list.CNL list of CNL (cannot-link) constrained pairs.
#' @return C constraints matrix (with 1 for must-link and -1 for cannot-link).
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#'            
#' ML <- list()
#' ML[[1]] <- c(sel="10",mem="20")
#' ML[[2]] <- c(sel="60",mem="70")
#' 
#' CNL <- list()
#' CNL[[1]] <- c(sel="30",mem="80")
#' CNL[[2]] <- c(sel="90",mem="120")
#' 
#' C <- buildConstraintsMatrix(n=nrow(dat), list.ML=ML, list.CNL=CNL)
#'
#' @keywords internal
#' 
buildConstraintsMatrix <- function(n, list.ML=list(), list.CNL=list()) {
    list.ML <- lapply(list.ML, FUN = as.numeric)
    list.CNL <- lapply(list.CNL, FUN = as.numeric)
    C <- matrix(0, nrow=n, ncol=n)
    for (pair in list.ML)
        C[pair[1],pair[2]] <- C[pair[2],pair[1]] <- 1
    for (pair in list.CNL)
        C[pair[1],pair[2]] <- C[pair[2],pair[1]] <- -1
    C
}
