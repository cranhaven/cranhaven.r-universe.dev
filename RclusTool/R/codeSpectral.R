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

#' computeGaussianSimilarity returns a similarity matrix computed thanks a Gaussian kernel
#' @title Gaussian similarity
#' @description Compute a similarity matrix thanks a Gaussian kernel from a data matrix.
#' @param dat numeric matrix of data (point by line).
#' @param sigma smooth parameter of Gaussian kernel.
#' @importFrom stats dist
#' @return sim similarity matrix.
#' @references U. Von Luxburg, A tutorial on spectral clustering, Statist. Comput., 17 (4) (2007), pp. 395-416
#' @seealso \code{\link{computeGaussianSimilarityZP}}
#'
#' @examples 
#' require(grDevices)
#' 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' 
#' sim <- computeGaussianSimilarity(dat, 1)
#' 
#' pal <- colorRampPalette(c("blue", "red"))
#' image(sim, col = pal(10))
#' @keywords internal
#' 
computeGaussianSimilarity <- function(dat, sigma) {
    e <- stats::dist(dat)
    E <- as.matrix(e)
    sim <- exp(-E^2/(2*sigma^2))
}

#' search.neighbour give the id of the Kth nearest neighbour
#' @title Search neighbour
#' @param vdist distance vector of point with other points
#' @param k number of neighbours
#' @return id of the Kth nearest neighbour
#' @seealso \code{\link{computeGaussianSimilarityZP}}
#' @examples 
#'
#' vdist=c(0,2,1,5,7,1,3)
#'
#' id<-search.neighbour(vdist,2)
#' @keywords internal
#' 

search.neighbour<-function(vdist, k){
  ind=rank(vdist,ties.method="first");
  out<-which(ind==k)
}

#' computeGaussianSimilarityZP returns a similarity matrix computed thanks a Gaussian kernel for which the parameters are self-tuned (according to Zelnik-Manor and Perona, 2004)
#' @title Gaussian similarity
#' @description Compute a similarity matrix thanks a Gaussian kernel for which the parameters are self-tuned (according to Zelnik-Manor and Perona, 2004).
#' @param dat numeric matrix of data (point by line).
#' @param k number of neighbour for the computation of local sigma (smooth parameter of Gaussian kernel).
#' @importFrom stats dist
#' @return sim similarity matrix.
#' @references L. Zelnik-Manor, P. Perona, Self tuning spectral clustering, Adv. Neural Inf. Process. Systems (2004), pp. 1601-1608.
#' @seealso \code{\link{computeGaussianSimilarity}}
#' 
#' @examples 
#' require(grDevices)
#' 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' 
#' sim <- computeGaussianSimilarityZP(dat, 10)
#' 
#' pal <- colorRampPalette(c("blue", "red"))
#' image(sim, col = pal(10))
#' @keywords internal 
#' 

computeGaussianSimilarityZP<-function(dat, k=7){
  vois=min(k,nrow(dat))
  e <- stats::dist(dat)
  E <- as.matrix(e)
  #identification du voisin numero vois pour chaque ligne (vois+1 car lui-meme insere)
  sigmaV <- apply(E,1,function(x){ind <- NULL; ind<-search.neighbour(x,k+1); out<-x[ind]+ .Machine$double.eps})
  #matrice des sigma i x sigma j
  sigma <- sapply(sigmaV,function(x){x*sigmaV}) 
  sim <- exp(-E^2/sigma)
}

#' spectralEmbeddingNg returns a spectral space built from a similarity matrix (according to Ng et al., 2002)
#' @title Spectral embedding
#' @description Build a spectral space from a similarity matrix (according to Ng et al., 2002).
#' @param sim similarity matrix.
#' @param K number of clusters.
#' @return The function returns a list containing:
#' \item{x}{matrix containing, in columns, the eigenvectors of the similarity matrix.}
#' \item{eigen.val}{vector containing the eigenvalues of the similarity matrix.}
#' @references A. Ng, M. Jordan, Y. Weiss, On spectral clustering: Analysis and an algorithm, Neural Inf. Process. Systems NIPS14 (2002), pp. 849-856.
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' 
#' sim <- computeGaussianSimilarity(dat, 1)
#' res <- spectralEmbeddingNg(sim, K=3)
#' 
#' plot(res$x[,2], res$x[,3], type = "p", xlab = "2nd eigenvector", ylab = "3rd eigenvector")
#' @keywords internal
#' 
spectralEmbeddingNg <- function(sim, K) {
    ###  diag(sim) <- 0
    #compute degree matrix^(1/2)
    d <- rowSums(abs(sim))   #here: ABS(...) added!
    ds <- d^(-0.5)
    #compute Laplacian matrix
    #D <- diag(ds)
    #L <- D %*% sim %*% D
    L <- ds * t(sim * ds) #acceleration 
    # K largest eigenvectors
    e <- eigen(L, symmetric=TRUE)
    Z <- e$vectors[,1:K]
    val <- e$values[1:K]

    #e <- svd(L, nu=K, nv=0)  
    #Z <- e$u
    #val <- e$d

    #ajout inutile
    #    Z <- t(t(Z)/sqrt(colSums(Z*Z))) #norme ut.D.u

    #projection on unit sphere
    row.sums <- apply(Z, MARGIN=1, FUN=function(x) norm(matrix(x),"f"))
    if (any(row.sums==0))
        stop("SpectralEmbedding error: unit sphere projection impossible; K may be too small.")
    Zn <- Z/row.sums
    rownames(Zn) <- rownames(sim)

    list(x=Zn, eigen.val=val)
}

#' spectralClusteringNg returns a partition obtained by spectral clustering (according to Ng et al., 2002)
#' @title Spectral clustering
#' @description Perform spectral clustering thanks to a similarity matrix (according to Ng et al., 2002).
#' @param sim similarity matrix.
#' @param K number of clusters.
#' @importFrom cluster pam
#' @return The function returns a list containing:
#' \item{label}{vector of labels.}
#' \item{medoids}{matrix of cluster centers in the space of the K first normalized eigenvectors.}
#' \item{id.med}{vector containing the medoids indices.}
#' \item{x}{matrix containing, in columns, the eigenvectors of the similarity matrix.}
#' \item{eigen.val}{vector containing the eigenvalues of the similarity matrix.}
#' \item{cluster.info}{some statistics on each cluster.}
#' @references A. Ng, M. Jordan, Y. Weiss, On spectral clustering: Analysis and an algorithm, Neural Inf. Process. Systems NIPS14 (2002), pp. 849-856.
#' @importFrom stats kmeans
#' @seealso \code{\link{spectralClustering}}
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' 
#' sim <- computeGaussianSimilarity(dat, 1)
#' res <- spectralClusteringNg(sim, K=3)
#' 
#' plot(dat[,1], dat[,2], type = "p", xlab = "x", ylab = "y", 
#'	col = res$label, main = "Initial features space")
#' plot(res$x[,2], res$x[,3], type = "p", xlab = "2nd eigenvector", 
#'	ylab = "3rd eigenvector", col = res$label, main = "Spectral embedding")
#' @keywords internal
#' 
spectralClusteringNg <- function(sim, K) {
    res.se <- spectralEmbeddingNg(sim, K)
    # clustering
    cl <- cluster::pam(res.se$x, K, diss = FALSE)
    # cl <- stats::kmeans(Zn, K, nstart=nruns)
    list(label=cl$clustering, medoids=cl$medoids, id.med=cl$id.med, x=res.se$x, eigen.val=res.se$eigen.val, cluster.info=cl$clusinfo)
}

#' spectralClustering returns a partition obtained by spectral clustering
#' @title Spectral clustering
#' @description Perform spectral clustering thanks to a similarity matrix.
#' @param sim similarity matrix.
#' @param K number of clusters.
#' @param nruns number of random sets.
#' @param test.dim boolean, only used to test dimensions (setting the number of left singular vectors to be computed).
#' @param projection character: final data projection. Must be 'Ng' (default), 'LPP', or 'AFC'.
#' @param post.norm character: final data normalization. Must be 'sphere' (default) or 'Shi'.
#' @param clustering character: clustering method in the spectral space. Must be 'kmeans' (default) or 'pam'.
#' @param max.dim maximal number of dimensions for the computation of left sigular vectors.
#' @param add.1 boolean : if TRUE add a column to the matrix containing the eigenvectors of the similarity matrix equal to 1/sqrt(nrow(similarity matrix))
#' @importFrom stats kmeans
#' @importFrom FactoMineR CA
#' @importFrom cluster pam
#' @return The function returns a list containing:
#' \item{label}{vector of labels.}
#' \item{medoids}{matrix of cluster centers in the space of the K first normalized eigenvectors.}
#' \item{id.med}{vector containing the medoids indices.}
#' \item{x}{matrix containing, in columns, the eigenvectors of the similarity matrix.}
#' \item{eigen.val}{vector containing the eigenvalues of the similarity matrix.}
#' \item{cluster.info}{some statistics on each cluster.}
#' \item{ncp}{number of left singular vectors computed.}
#' @references A. Ng, M. Jordan, Y. Weiss, On spectral clustering: Analysis and an algorithm, Neural Inf. Process. Systems NIPS14 (2002), pp. 849-856.
#' @seealso \code{\link{spectralClusteringNg}}
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' 
#' sim <- computeGaussianSimilarity(dat, 1)
#' res <- spectralClustering(sim, K=3)
#' 
#' plot(dat[,1], dat[,2], type = "p", xlab = "x", ylab = "y", 
#'	col = res$label, main = "Initial features space")
#' plot(res$x[,1], res$x[,2], type = "p", xlab = "2nd eigenvector", 
#' 	ylab = "3rd eigenvector", col = res$label, main = "Spectral embedding")
#'
#' @keywords internal
#' 
spectralClustering <- function(sim, K, nruns=1, test.dim=FALSE,
                               projection="Ng", post.norm="sphere", clustering="kmeans",
                               max.dim=30, add.1=FALSE) {
    ncp <- K
    if (test.dim)
        ncp <- min(ncol(sim), max(max.dim,K))

    if ((projection=="Ng") || (projection=="LPP")) {
        d <- rowSums(abs(sim))
        ds <- d^(-0.5)
        L <- ds * t(sim * ds) 
        e <- svd(L, nu=ncp, nv=0)  
        Zi <- e$u
        val <- e$d
    }

    if (projection=="AFC") {
        afc <- FactoMineR::CA(X=sim, ncp=ncp, graph=FALSE)
        Zi <- afc$row$coord
        val <- afc$eig[,"eigenvalue"]
    }

    if (add.1)
        Zi <- cbind(1/sqrt(nrow(Zi)), Zi)

    if ((projection=="Ng") && (post.norm=="Shi"))
        Zi <- ds*Zi

    if (projection=="Ng")
        Zi <- t(t(Zi)/sqrt(colSums(Zi*Zi))) #norme ut.D.u

    if (!test.dim) {
        if (post.norm=="sphere") {
            row.sums <- apply(Zi, MARGIN=1, FUN=function(x) norm(matrix(x),"f"))
            if (any(row.sums==0))
                stop("SpectralEmbedding error: unit sphere projection impossible; K may be too small.")
            Z <- Zi/row.sums #peut etre accelere (??)
        } else {
            Z <- Zi
        }

        if (clustering=="kmeans")
            cl <- stats::kmeans(Z, K, nstart=nruns)

        if (clustering=="pam") {
            cl <- cluster::pam(Z, K, diss = FALSE)
            cl$cluster <- cl$clustering
        }
    } else {
        cl.sauv <- NULL
        score.sauv <- Inf
        ncp.sauv <- NULL
        for (j in 2:ncp) {

            if (post.norm=="sphere") {
                Z <- Zi[,1:j]/apply(Zi[,1:j], MARGIN=1, FUN=function(x) norm(matrix(x),"f"))
            } else {
                Z <- Zi[,1:j]
            }

            if (clustering=="kmeans")
                cl <- stats::kmeans(Z, K, nstart=nruns)

            if (clustering=="pam") {
                cl <- cluster::pam(Z, K, diss = FALSE)
                cl$cluster <- cl$clustering
            }

            ## BUG with MNCut computation (to check)!!!
            #score <- measureMNCut(cl$cluster, similarite)

            # if (score < score.sauv) {
            #   score.sauv <- score
            #   cl.sauv <- cl
            #   ncp.sauv <- j
            # }
        }
        cl <- cl.sauv
        ncp <- ncp.sauv
    }
    #score <- measureMNCut(cl$cluster, sim)
    names(cl$cluster) <- rownames(sim)

    list(label=cl$cluster, medoids=cl$medoids, id.med=cl$id.med, x=Zi,
         eigen.val=val, cluster.info=cl$clusinfo, ncp=ncp)#, score=score)
}


#' bipartitionShi returns a partition obtained by spectral clustering (according to Shi and Malik, 2000)
#' @title Spectral clustering
#' @description Perform spectral clustering thanks to a similarity matrix (according to Shi and Malik, 2000).
#' @param sim similarity matrix.
#' @return The function returns a list containing:
#' \item{label}{vector of labels.}
#' \item{eigenvector}{matrix containing, in columns, the eigenvectors of the similarity matrix.}
#' \item{eigenvalue}{vector containing the eigenvalues of the similarity matrix.}
#' @references J. Shi, J. Malik, Normalized cuts and image segmentation, IEEE Transactions on Pattern Analysis and Machine Intelligence, 2000, 22(8), 888-905.
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2))
#' 
#' sim <- computeGaussianSimilarity(dat, 1)
#' res <- bipartitionShi(sim)
#' 
#' plot(dat[,1], dat[,2], type = "p", xlab = "x", ylab = "y", 
#'	col = res$label, main = "Initial features space")
#' plot(res$eigenvector, type = "p", xlab = "Indices", ylab = "1st eigenvector", 
#'	col = res$label, main = "Spectral embedding")
#' @keywords internal
#' 
bipartitionShi <- function(sim) {
    d <- rowSums(sim)
    ds <- d^(-0.5)
    D <- diag(ds)
    I <- diag(1, nrow(sim), ncol(sim))
    L <- I-D %*% sim %*% D
    e <- eigen(L, symmetric=TRUE)
    #keep the first smallest eigenvector (no trivial)
    g <- ds*e$vectors[, nrow(sim)-1]
    val <- e$values[nrow(sim)-1]
    u <- sign(g)
    u[u<0] <- 2
    out <- list(label=u, eigenvector=g, eigenvalue=val)
}

#' computeGap returns an estimated number of clusters
#' @title Gap computation
#' @description Estimate the number of clusters thanks to the gap computation.
#' @param sim similarity matrix.
#' @param Kmax maximal number of clusters.
#' @return The function returns a list containing:
#' \item{val}{vector containing the eigenvalues of the similarity matrix.}
#' \item{gap}{vector containing gap values between two successive eigenvalues.}
#' \item{Kmax}{estimated number of clusters.}
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' 
#' sim <- computeGaussianSimilarity(dat, 1)
#' res <- computeGap(sim, Kmax = 20)
#' 
#' plot(res$val[1:20], type = "o", ann = FALSE, axes = FALSE)
#' abline(v = res$Kmax, col = "darkred")
#' abline(h = res$val[res$Kmax], col = "darkred")
#' axis(side = 1, at = c(seq(0,20,by=5), res$Kmax), 
#'      labels = c(seq(0,20,by=5), res$Kmax), cex.axis = .7)
#' axis(side = 2)
#' title("Automatic estimation of number of clusters - Gap method")
#' mtext("Number of clusters", side = 1, line = 3)
#' mtext("Eigenvalue", side = 2, line = 3)
#' box()
#' @keywords internal 
#' 
computeGap <- function(sim, Kmax) {
    d <- rowSums(abs(sim))
    ds <- d^(-0.5)
    L <- ds * t(sim * ds) 
    nbVal <- min(Kmax+1, NROW(sim))
    e <- svd(L, nu=nbVal)  
    Z <- e$u
    val <- e$d
    #extract the Kmax largest eigenvectors
    gap <- rep(0, nbVal-1)
    for(v in 2:(nbVal-1))
        gap[v] <- abs(val[v]-val[v+1])
    K <- which.max(gap)
    out <-list(val=val, gap=gap, Kmax=K)
}

#' computeGap2 returns an estimated number of clusters
#' @title Gap computation
#' @description Estimate the number of clusters thanks to the gap computation.
#' @param sim similarity matrix.
#' @param Kmax maximal number of clusters.
#' @return The function returns a list containing:
#' \item{val}{vector containing the eigenvalues of the similarity matrix.}
#' \item{gap}{vector containing gap values between two successive eigenvalues.}
#' \item{Kmax}{estimated number of clusters.}
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'            matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' 
#' sim <- computeGaussianSimilarity(dat, 1)
#' res <- computeGap2(sim, Kmax = 20)
#' 
#' plot(res$val[1:20], type = "o", ann = FALSE, axes = FALSE)
#' abline(v = res$Kmax, col = "darkred")
#' abline(h = res$val[res$Kmax], col = "darkred")
#' axis(side = 1, at = c(seq(0,20,by=5), res$Kmax), 
#'      labels = c(seq(0,20,by=5), res$Kmax), cex.axis = .7)
#' axis(side = 2)
#' title("Automatic estimation of number of clusters - Gap method")
#' mtext("Number of clusters", side = 1, line = 3)
#' mtext("Eigenvalue", side = 2, line = 3)
#' box()
#' @keywords internal
#' 
computeGap2 <- function(sim, Kmax) {
    d <- rowSums(sim)
    ds <- d^(-0.5)
    D <- diag(ds)
    #compute Laplacian matrix
    L <- D %*% sim %*% D
    #extract the Kmax largest eigenvectors
    e <- eigen(L, symmetric=TRUE)
    Z <- e$vectors[, 1:min(Kmax+1, NROW(sim))]
    val <- e$values[1:min(Kmax+1, NROW(sim))]
    nbVal <- min(Kmax+1, NROW(sim))
    gap <- rep(0,nbVal-1)
    for(v in 2:(nbVal-1))
        gap[v] <- abs(val[v]-val[v+1])
    K <- which.max(gap)
    out <- list(val=val, gap=gap, Kmax=K)
}


#fastSpectralClustering (UNUSED!!!)
#fastSpectralClustering <- function(x, label.init=NULL, K=0,
#                                   nb.max.sample=ceiling(3000/K), vois=7, K.max=20) {
#    #sampling
#    if (is.infinite(nb.max.sample))
#        nb.max.sample <- ceiling(3000/K.max)
#
#    if (is.null(label.init)) {
#        res.kmeans <- computeKmeans(x, K=0, K.max=K.max)
#        label.init <- res.kmeans$cluster
#    }
#    selection.ids <- sort(unlist(by(1:length(label.init), factor(label.init),
#                                    function (x) {sample(x, min(length(x), nb.max.sample))})))
#
#    #SC on sampled data
#    dataSC <- x[selection.ids,,drop=FALSE]
#    print("Similarity computing")
#    sim <- computeGaussianSimilarityZP(dataSC, vois)
#    if (K==0) {
#        print("Cluster number estimating")
#        res.gap <- computeGap(sim, K.max)
#        print(paste("  obtained K=", res.gap$Kmax))
#        K <- res.gap$Kmax
#    }
#    print(paste("Spectral clustering computing with ZP, #neighbours=",vois))
#    res.sc <- spectralClusteringNg(sim, K)
#
#    #generalization by k-nn, works if selection.ids are sorted
#    res.knn <- knn(x[selection.ids,,drop=FALSE], x, cl=factor(selection.ids), k=1) #WARNING #neighbours=1
#    #!!!!! (sinon, prototypes peuvent changer de classes)
#    matching <- as.integer(as.character(res.knn))
#    # replace neighbours ids by 
#    label <- res.sc$label[matching]
#
#    res.pca <- stats::prcomp(res.sc$x, center = TRUE, scale = FALSE)
#    res.fsc <- list(x=res.pca$x, label=label, matching=matching,
#                    selection.ids=selection.ids, prototype.ids=selection.ids[res.sc$id.med],
#                    K=K, sim=sim)
#}
