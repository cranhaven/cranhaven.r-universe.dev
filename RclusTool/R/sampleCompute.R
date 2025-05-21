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

#' computePcaNbDims computes the number of dimensions to keep after a Principal Components Analysis, according to a threshold on the cumulative variance
#' @title Number of dimensions for PCA
#' @description Compute the number of dimensions to keep after a Principal Components Analysis, according to a threshold on the cumulative variance.
#' @param sdev standard deviation of the principal components (returned from prcomp).
#' @param pca.variance.cum.min minimal cumulative variance to retain.
#' @return pca.nb.dims number of dimensions kept.
#' @seealso \code{\link{computePcaSample}}
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf)
#' res.pca <- computePcaSample(x)
#' computePcaNbDims(res.pca$pca$sdev)
#' 
#' 
#'
#' @keywords internal
#' 
computePcaNbDims <- function(sdev, pca.variance.cum.min=0.9) {
    message("PCA number of dimensions computing")
    pca.variance <- sdev^2/sum(sdev^2)
    pca.variance.cum <- cumsum(pca.variance)
    pca.nb.dims <- min(which(pca.variance.cum >= pca.variance.cum.min))
} 

#' computeKmeans performs K-means clustering, dealing with the number of clusters K, automatically or not
#' @title K-means clustering
#' @description Perform K-means clustering, dealing with the number of clusters K, automatically or not.
#' @param x matrix of raw data (point by line).
#' @param K number of clusters. If K=0 (default), this number is automatically computed thanks to the Elbow method.
#' @param K.max maximal number of clusters (K.Max=20 by default).
#' @param kmeans.variance.min elbow method cumulative explained variance > criteria to stop K-search.
#' @param graph boolean: if TRUE, figures for total of within-class inertia and explained variance are plotted.
#' @return res.kmeans results obtained from kmeans algorithm.
#' @seealso \code{\link{computeUnSupervised}}, \code{\link{computeEM}}
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf)
#' res.kmeans <- computeKmeans(x$features$initial$x, K=0, graph=TRUE)
#' plot(dat[,1], dat[,2], type = "p", xlab = "x", ylab = "y", 
#'	col = res.kmeans$cluster, main = "K-means clustering")
#' 
#' 
#' 
#' @keywords internal
#' 
computeKmeans <- function(x, K=0, K.max=20, kmeans.variance.min=0.95, graph=FALSE) {
    K.max <- min(nrow(x), K.max)
    if (K.max <= 2)
        K <- K.max
    if (K==0) { # #KMeans Auto Method Elbow : includes selection of K, with rule of thumb
        message("K-means computing and K estimation: method Elbow")
        kmax <- min(ceiling(sqrt(nrow(x)/2)), K.max)
        res <- KmeansAutoElbow(x, Kmax=kmax, kmeans.variance.min, graph)
        res.kmeans <- res$res.kmeans
        message(paste("  obtained K=", res$K))
    } else {
        message("K-means computing: method Quick")
        res.kmeans <- KmeansQuick(x, K=K)
    }
    res.kmeans
}

#' computeEM performs Expectation-Maximization clustering, dealing with the number of clusters K, automatically or not
#' @title Expectation-Maximization clustering
#' @description Perform Expectation-Maximization clustering, dealing with the number of clusters K, automatically or not.
#' @param x matrix of raw data (point by line).
#' @param K number of clusters. If K=0 (default), this number is automatically computed thanks to the Elbow method.
#' @param K.max maximal number of clusters (K.Max=20 by default).
#' @param kmeans.variance.min elbow method cumulative explained variance > criteria to stop K-search.
#' @param graph boolean: if TRUE, figures for total of within-class inertia and explained variance are plotted.
#' @param Mclust.options list of default parameters values for the function Mclust.
#' @importFrom mclust Mclust mclustBIC
#' @return res.EM results obtained from Mclust algorithm.
#' @seealso \code{\link{computeUnSupervised}}, \code{\link{computeKmeans}}
#'
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf)
#' res.em <- computeEM(x$features$initial$x, K=0, graph=TRUE)
#' plot(dat[,1], dat[,2], type = "p", xlab = "x", ylab = "y", 
#'	col = res.em$classification, main = "EM clustering")
#' 
#' 
#' @keywords internal
#' 
computeEM <- function(x, K=0, K.max=20, kmeans.variance.min=0.95, graph=FALSE, Mclust.options=list()) {
	Within <- NULL
    if (K==0) { # #KMeans Auto Method Elbow : includes selection of K, with rule of thumb
        message("EM computing and K estimation: method Elbow")
        kmax <- min(ceiling(sqrt(nrow(x)/2)), K.max)
        res <- KmeansAutoElbow(x, Kmax=kmax, kmeans.variance.min, graph)
        Within <- res$res.kmeans[["Withins_tot"]]
        K <- res$K
        message(paste("  obtained K=", res$K))
    }

    if (K<2)
        stop("EM computing requires several features (nb.cols >= 2).")

    res.EM <- do.call(mclust::Mclust, c(list(x, G=K), Mclust.options))
    if (is.null(res.EM$classification)){
    	stop("No clustering found for this sample with EM, try with another K")
    } 
    res.EM[["Withins_tot"]]=Within
    res.EM
}

#' guessFileEncoding guess file encoding.
#' @title File Encoding Identification.
#' @description Guess file encoding.
#' @param file.name a character vector, describing one file.
#' @importFrom stringi stri_enc_detect
#' @return file character encoding.
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' 
#' csv.file <- tempfile()
#' write.table(dat, csv.file, sep=",", dec=".")
#' 
#' guessFileEncoding(csv.file)
#' 
#' 
#' @keywords internal
guessFileEncoding <- function(file.name)
{
    stringi::stri_enc_detect(rawToChar(readBin(file.name, "raw")))[[1]][1,1]
}

#' sortCharAsNum performs sorting of a vector of characters numerically, if possible
#' @title Character vector numeric sorting
#' @description Performs sorting of a vector of characters numerically, if possible
#' @param char.vec a character vector, describing numbers.
#' @param ... other parameters controlling the order treatment of the character vector.
#' @return the character vector sorted, if possible. 
#' @seealso \code{\link{computeSemiSupervised}}, \code{\link{KwaySSSC}}
#' 
#' @examples 
#' x <- c("12", "8", "101")
#' 
#' sortCharAsNum(x)
#' 
#' @keywords internal
sortCharAsNum <- function(char.vec, ...)
{
    ok <- TRUE
    vec <- suppressWarnings(as.numeric(char.vec))
    if (any(is.na(vec)))
    {
        vec <- char.vec
        ok <- FALSE
    }
    res <- order(vec, ...)
    char.vec[res]
}


#' convNamesToIndex performs conversion of an element name to its index inside a set of named objects
#' @title Conversion of element names to indexes
#' @description Performs conversion of an element name to its index inside a set of named objects
#' @param name.sample a vector of element names.
#' @param full.name.set a vector of unique names.
#' @return the vector of indexes of the element named.
#' @seealso \code{\link{computeSemiSupervised}}, \code{\link{KwaySSSC}}
#'
#' @examples 
#' x <- c("a"=10, "b"=20, "c"=30 )
#' 
#' convNamesToIndex(c("b","a","c"),names(x))
#' 
#' @keywords internal
convNamesToIndex <- function(name.sample, full.name.set)
{
    sapply(name.sample, function(name) which(full.name.set == name))
}

#' convNamesPairsToIndexPairs performs conversion of list of names pairs to a matrix of index pairs.
#' @title Conversion of a set of names pairs to matrix of index pairs (2 columns)
#' @description Performs conversion of list of names pairs to a matrix of index pairs
#' @param pair.list list of names pairs (vector of 2 names).
#' @param full.name.set a vector of unique names.
#' @return the 2-column matrix of pairs of indexes.
#' @seealso \code{\link{computeSemiSupervised}}, \code{\link{KwaySSSC}}
#'
#' @examples 
#' x <- c("a"=10, "b"=20, "c"=30 )
#' 
#' ML <- list()
#' ML[[1]] <- c("a","c")
#' ML[[2]] <- c("c","d")
#' 
#' mustLink<-convNamesPairsToIndexPairs(ML,names(x))
#' 
#' @keywords internal
convNamesPairsToIndexPairs <- function(pair.list, full.name.set)
{
    pair.matrix <- NULL
    if (!length(pair.list)){
        pair.matrix <- matrix(0,nrow=0,ncol=2)
    } else {
        pair.list <- sapply(pair.list, function(ligne) convNamesToIndex(ligne, full.name.set), simplify=FALSE)
        pair.matrix <- do.call(rbind, pair.list)
        colnames(pair.matrix) <- NULL
        pair.matrix <- pair.matrix[apply(pair.matrix,1,function(ligne) all(!is.na(ligne))),,drop=FALSE]
    }
    pair.matrix
}

#' computeCKmeans performs Constrained K-means clustering, dealing with the number of clusters K, automatically or not
#' @title Constrained K-means clustering
#' @description Perform Constrained K-means clustering, dealing with the number of clusters K, automatically or not.
#' @param x matrix of raw data (point by line).
#' @param K number of clusters. If K=0 (default), this number is automatically computed thanks to the Elbow method.
#' @param K.max maximal number of clusters (K.Max=20 by default).
#' @param mustLink list of ML (must-link) constrained pairs.
#' @param cantLink list of CNL (cannot-link) constrained pairs.
#' @param maxIter number of iterations for mpckm algorithm.
#' @param kmeans.variance.min elbow method cumulative explained variance > criteria to stop K-search.
#' @importFrom conclust mpckm
#' @return res.ckmeans results obtained from mpckm algorithm.
#' @seealso \code{\link{computeSemiSupervised}}, \code{\link{KwaySSSC}}
#'
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' x <- importSample(file.features=tf)
#' 
#' ML=list(c(sel="10",mem="20"))
#' CNL=list(c(sel="1",mem="140"))
#'
#' res.ckmeans <- computeCKmeans(x$features$initial$x, K=0, mustLink=ML, cantLink=CNL)
#'
#' plot(dat[,1], dat[,2], type = "p", xlab = "x", ylab = "y", 
#'	col = res.ckmeans$label, main = "Constrained K-means clustering")
#' 
#' 
#' @keywords internal
#' 

computeCKmeans <- function(x, K=0, K.max=20, mustLink=NULL, cantLink=NULL, maxIter=2, kmeans.variance.min=0.95) {
	Within <- NULL
    # conversion names to indexes + to matrix
    mustLink <- convNamesPairsToIndexPairs(mustLink, row.names(x))
    cantLink <- convNamesPairsToIndexPairs(cantLink, row.names(x))

    if (K==0) { # #KMeans Auto Method Elbow : includes selection of K, with rule of thumb
        message("Constrained K-means computing and K estimation: method Elbow")
        kmax <- min(ceiling(sqrt(nrow(x)/2)), K.max)
        res <- KmeansAutoElbow(x, Kmax = kmax, kmeans.variance.min)
        Within <- res$res.kmeans[["Withins_tot"]]
        # COP K-means algorithm (Wagstaff et al., 2001)
        #res.ckmeans <- ckmeans(x, k = res$K, mustLink, cantLink, maxIter)
        # MPC K-means algorithm (Bilenko et al., 2004)
        label <- conclust::mpckm(x, k = res$K, mustLink, cantLink, maxIter)
        message(paste("  obtained K=", res$K))
    } else {
        message("Constrained K-means computing")
        # COP K-means algorithm (Wagstaff et al., 2001)  
        #res.ckmeans <- ckmeans(x, k = K, mustLink, cantLink, maxIter)
        # MPC K-means algorithm (Bilenko et al., 2004)
        label <- conclust::mpckm(x, k = K, mustLink, cantLink, maxIter)
    }
    res.ckmeans <- list(label=label, Within=Within)
    res.ckmeans
}

#' computeCSC performs Constrained Spectral Clustering from a similarity matrix computation
#' @title Constrained Spectral Clustering
#' @description Perform Constrained Spectral Clustering from a similarity matrix computation.
#' @param x matrix of raw data (point by line).
#' @param K number of clusters. If K=0 (default), this number is automatically computed thanks to the Elbow method.
#' @param K.max maximal number of clusters (K.Max=20 by default).
#' @param mustLink list of ML (must-link) constrained pairs.
#' @param cantLink list of CNL (cannot-link) constrained pairs.
#' @param alphas numeric vector for the weight of constraints considered.
#' @return res.csc results obtained from KwaySSSC algorithm.
#' @seealso \code{\link{computeSemiSupervised}}, \code{\link{KwaySSSC}}
#'
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' x <- importSample(file.features=tf)
#' 
#' ML=list(c(sel="10",mem="20"))
#' CNL=list(c(sel="1",mem="140"))
#'
#' res.csc <- computeCSC(x$features$preprocessed$x, K=0, mustLink=ML, cantLink=CNL)
#' plot(dat[,1], dat[,2], type = "p", xlab = "x", ylab = "y", 
#'	col = res.csc$label, main = "Constrained Spectral clustering")
#' 
#' 
#' @keywords internal
#' 
computeCSC <- function(x, K=0, K.max=20, mustLink=list(), cantLink=list(), alphas=seq(from=0, to=1, length=100)) {
    # conversion names to indexes + to matrix
    if (length(mustLink)){
        mustLink <- sapply(mustLink, function(ligne) convNamesToIndex(ligne, row.names(x)), simplify=FALSE)
        mustLink <- mustLink[sapply(mustLink,function(ligne) all(!is.na(ligne)))]
    }

    if (length(cantLink)){
        cantLink <- sapply(cantLink, function(ligne) convNamesToIndex(ligne, row.names(x)), simplify=FALSE)
        cantLink <- cantLink[sapply(cantLink,function(ligne) all(!is.na(ligne)))]
    }

    #mustLink <- lapply(mustLink, function(y) which(row.names(x) %in% y))
    #cantLink <- lapply(cantLink, function(y) which(row.names(x) %in% y))

    sim <- computeGaussianSimilarityZP(x, 7)
    res.csc <- KwaySSSC(sim, K=K, list.ML=mustLink, list.CNL=cantLink,
                        alphas=alphas, K.max=K.max)
    res.csc
}

#' computeSampling computes sampling on raw data matrix to reduce the number of observations, with generalization step.
#' @title Sampling raw data matrix
#' @description computes sampling on raw data matrix to reduce the number of observations, with generalization step.
#' @param x matrix of raw data (point by line).
#' @param label vector of (named) labels.
#' @param K number of clusters. If K=0 (default), this number is automatically computed thanks to the Elbow method.
#' @param toKeep vector of row.names to keep in the sample (for constrained algorithms).
#' @param sampling.size.max maximal number of observations to keep in the sample.
#' @param K.max maximal number of clusters (K.Max=20 by default).
#' @param kmeans.variance.min elbow method cumulative explained variance > criteria to stop K-search.
#' @importFrom class knn
#' @importFrom stats ave
#' @return The function returns a list containing:
#' \item{selection.ids}{vector of selected row.names in the sample.}
#' \item{selection.labs}{vector of selected labels in the sample.}
#' \item{matching}{character specifying the matching for all observations and used for generalization of the clustering result.}
#' \item{size.max}{maximal number of observations kept in the sample.}
#' \item{K}{number of clusters.}
#'
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' x <- importSample(file.features=tf)
#' 
#' res.sampling <- computeSampling(x$features$initial$x)
#' 
#' 
#' @keywords internal
#' 

computeSampling <- function(x, label=NULL, K=0, toKeep=NULL, sampling.size.max=3000, K.max=20, kmeans.variance.min=0.95) {
      if (!sampling.size.max)
        sampling.size.max <- 3000

    nrx <- nrow(x)
    if ((K>0) && (K>nrx))
        K <- nrx
    if (K.max > nrx)
        K.max <- nrx

    if (is.null(label)) {
        res.kmeans <- computeKmeans(x, K=K, K.max=K.max,
                                    kmeans.variance.min=kmeans.variance.min, graph=FALSE)
        label <- res.kmeans$cluster
        label_prob <- stats::ave(label,label,FUN=length)
        label_prob <- 1/(label_prob*max(unname(label)))
    }

    selection <- sample(rownames(x),min(sampling.size.max,nrow(x)),prob=label_prob)
    selection <- by(selection, label[selection], 
                function (x) {x})
    selection <- sapply(selection, as.character)

    # keep constrained items in sampling (only for semi-supervised classification)
    #message(setdiff(toKeep, unlist(selection)))

    if (!is.null(toKeep)) {
        toKeep <- unique(toKeep)
        for (t in setdiff(toKeep, unlist(selection)))
            selection[[label[t]]] <- c(selection[[label[t]]], t)
    }

    selection.ids <- sortCharAsNum(unlist(selection, use.names = FALSE))
    selection.labs <- label[selection.ids]

    #       selection.ids <- sort(unlist(by(label, factor(label), 
    #                                 function (x) {sample(x, min(length(x), nb.max.sample))})))
    #generalization by k-nn, works if selection.ids are sorted
    # res.knn <- class::knn(x[selection.ids,,drop=FALSE], x, cl=selection.ids, k=1) #WARNING #neighbours=1
    res.knn <- class::knn(x[selection.ids,,drop=FALSE], x, cl=selection.ids, k=1)
    #!!!!! (sinon, prototypes peuvent changer de classes)
    matching <- as.character(res.knn) #as.integer(as.character(res.knn))

    list(selection.ids=selection.ids, selection.labs=selection.labs, matching=matching, size.max=sampling.size.max, K=length(levels(label)))
}

#' addIds2Sampling adds some observations to set of sampled observations.
#' @title Adding Ids To a Sampling
#' @description adds some observations to set of sampled observations.
#' @param sampling object of a data sample.
#' @param toAdd vector of ids to add.
#' @return The completed sampling object.
#'
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' x <- importSample(file.features=tf)
#' 
#' res.sampling <- computeSampling(x$features$initial$x)
#' completed.sampling <- addIds2Sampling(res.sampling, rownames(x$features$initial$x[1:5,,drop=FALSE]))
#' 
#' 
#' @keywords internal
#' 

addIds2Sampling <- function(sampling, toAdd=NULL) {
    if (length(toAdd)) {
        toAdd <- setdiff(unique(toAdd),sampling$selection.ids)
        sampling$selection.ids <- c(sampling$selection.ids, toAdd)
        sampling$selection.labs <- c(sampling$selection.labs, rep(NA,length(toAdd))) # Warning !!!
        sampling$matching <- c(sampling$matching, toAdd)
        sampling$size.max <- max(sampling$size.max, length(sampling$selection.ids))
    }
    sampling
}


#' computePcaSample performs Principal Components Analysis, dealing with the number of dimensions, automatically or not
#' @title Principal Components Analysis
#' @description Perform Principal Components Analysis, dealing with the number of dimensions, automatically or not.
#' @param data.sample list containing features, profiles and clustering results.
#' @param pca.nb.dims number of dimensions to keep after PCA. If pca.nb.dims=0 (default), this number is automatically computed.
#' @param selected.var vector of features names to consider for the PCA.
#' @param echo boolean: if FALSE (default), no description printed in the console.
#' @param prcomp.options list of default parameters values for the function prcomp.
#' @param pca.variance.cum.min minimal cumulative variance to retain in PCA.
#' @importFrom stats prcomp var
#' @return features list containing the results of PCA, returned by prcomp.
#' @seealso \code{\link{computeSpectralEmbeddingSample}}
#'
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' x <- importSample(file.features=tf)
#' 
#' res <- computePcaSample(x, echo = TRUE)
#' 
#' plot(res$pca_full$x[,1], res$pca_full$x[,2], main="PCA", xlab="PC1", ylab="PC2")
#' 
#' 
#' @keywords internal
#' 
computePcaSample <- function(data.sample, pca.nb.dims=0, selected.var=NULL, echo=FALSE, prcomp.options=list(center=TRUE, scale=TRUE), pca.variance.cum.min=0.95) {
    features <- list("pca_full"=NULL, "pca"=NULL)

    numFeat <- colnames(data.sample$features[["preprocessed"]]$x)[sapply(data.sample$features[["preprocessed"]]$x, is.numeric)]
    if (length(selected.var))
        selected.var <- intersect(selected.var, numFeat)
    if (!length(selected.var))
        selected.var <- numFeat

    available <- setequal(selected.var, colnames(data.sample$features$pca))

    missing.data <- is.null(data.sample$features$pca)
    if (missing.data || !available) {
        if (echo) message("PCA computing")

        # Compute pca
        df <- as.data.frame(data.sample$features[["preprocessed"]]$x[data.sample$id.clean, selected.var, drop=FALSE])
        df <- df[,apply(df, 2, stats::var, na.rm=TRUE) != 0]

        if (all(dim(df) > 1)) {
            res.pca <- do.call(stats::prcomp, c(list(df), prcomp.options))

            rownames(res.pca$x) <- rownames(data.sample$features[["preprocessed"]]$x)[data.sample$id.clean]
            features$pca$save <- res.pca
            features$pca$x <- as.data.frame(res.pca$x)
            features$pca$rotation <- res.pca$rotation
            features$pca$sdev <- res.pca$sdev
            features$pca$inertia.prop <- cumsum(res.pca$sdev^2)/sum(res.pca$sdev^2)
            #features$pca$call <- this.call #pas hyper necessaire, mais interessant
            colnames(features$pca$x) <- paste("PC", 1:ncol(features$pca$x))
            colnames(features$pca$rotation) <- paste("PC", 1:ncol(features$pca$rotation))
            features$pca$logscale <- rep(FALSE, ncol(features$pca$x))
            names(features$pca$logscale) <- colnames(features$pca$x)
            features[["pca_full"]] <- features$pca
        } else {
            stop("No valid features: pca cannot be computed")
            features <- data.sample$features[c("pca_full","pca")]
        }

        if (pca.nb.dims==0) #attention: pca.nb.dims dissocie de sa boite texte
            pca.nb.dims <- computePcaNbDims(features$pca$sdev, pca.variance.cum.min)

        # one feature component is not enough for scatter plots
        if (pca.nb.dims<2)
            pca.nb.dims <- 2

        if (echo) message(paste("Working on PCA, nb of principal components =", pca.nb.dims))
        if (echo) message(paste("Information lost=", 100-round(features$pca$inertia.prop[pca.nb.dims]*100,2), "%"))

        features[["pca"]]$x <- features$pca$x[,1:pca.nb.dims, drop=FALSE]
        features[["pca"]]$logscale <- features$pca$logscale[1:pca.nb.dims]
        features[["pca"]]$rotation <- features$pca$rotation[1:pca.nb.dims]
        features[["pca"]]$sdev <- features$pca$sdev[1:pca.nb.dims]
        features[["pca"]]$inertia.prop <- features$pca$inertia.prop[1:pca.nb.dims]

        features
    } else
        data.sample$features
}

#' computeSpectralEmbeddingSample performs Spectral embedding for non-linear dimensionality reduction
#' @title Spectral embedding
#' @description Perform spectral embedding for non-linear dimensionality reduction.
#' @param data.sample list containing features, profiles and clustering results.
#' @param use.sampling boolean: if FALSE (default), data sampling is not used.
#' @param sampling.size.max numeric: maximal size of the sampling set.
#' @param scale boolean, if FALSE (default), data scaling is not used.
#' @param selected.var vector of features names to consider for the spectral embedding.
#' @param RclusTool.env environment in which all global parameters, raw data and results are stored.
#' @param echo boolean: if FALSE (default), no description printed in the console.
#' @return features list containing the results of spectral embedding, returned by spectralEmbeddingNg.
#' @seealso \code{\link{computePcaSample}}
#'
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' x <- importSample(file.features=tf)
#' 
#' res <- computeSpectralEmbeddingSample(x)
#' 
#' plot(res$x[,1], res$x[,2], main="Spectral Embedding", xlab="SC1", ylab="SC2")
#' 
#' 
#' @keywords internal
#' 
computeSpectralEmbeddingSample <- function(data.sample, use.sampling = FALSE, sampling.size.max=0, scale=FALSE, selected.var = NULL, echo=FALSE, RclusTool.env=initParameters()) {
    this.call <- list(use.sampling=use.sampling, selected.var=selected.var)
    if (use.sampling)
        this.call[["selection.ids"]] <- data.sample$sampling$selection.ids

    if (length(selected.var))
        selected.var <- intersect(selected.var, colnames(data.sample$features[["preprocessed"]]$x)[sapply(data.sample$features[["preprocessed"]]$x, is.numeric)])
    if (!length(selected.var))
        selected.var <- colnames(data.sample$features[["preprocessed"]]$x)[sapply(data.sample$features[["preprocessed"]]$x, is.numeric)]

    this.call[["selected.var"]] <- selected.var

    # Compute spectral embedding
    x <- data.sample$features[["preprocessed"]]$x[data.sample$id.clean, selected.var, drop=FALSE]
        
    #Scale
	if (scale) {
		x <- scale(x, center = TRUE, scale = TRUE)
	}

    #Sampling
    if (use.sampling) {
        if (is.null(data.sample$sampling) || (!is.null(data.sample$config$sampling.size.max)&&(data.sample$config$sampling.size.max!=sampling.size.max)))
        {
            data.sample$sampling <- computeSampling(x=x, K=RclusTool.env$param$classif$unsup$K.max,
                                                    sampling.size.max=RclusTool.env$param$preprocess$sampling.size.max, kmeans.variance.min=RclusTool.env$param$classif$unsup$kmeans.variance.min)
        }
        x <- x[data.sample$sampling$selection.ids, , drop=FALSE]
    }

    if (echo) message("Similarity computing")
    sim <- computeGaussianSimilarityZP(x, k = RclusTool.env$param$classif$unsup$nb.neighbours)
    if (echo) message("Cluster number estimating")
    res.gap <- computeGap(sim, Kmax = RclusTool.env$param$classif$unsup$K.max)
    if (echo) message(paste("  obtained K = ", res.gap$Kmax))
    K <- res.gap$Kmax

    res.se <- spectralEmbeddingNg(sim, K)
    features <- data.sample$features[["preprocessed"]]
    features$call <- this.call
    features$x <- as.data.frame(res.se$x)
    features$gap <- res.gap$gap
    features$eig <- res.gap$val
    features$sampling <- TRUE
    colnames(features$x) <- paste("SC", 1:ncol(features$x))
    features$logscale <- rep(FALSE, ncol(res.se$x))
    names(features$logscale) <- colnames(features$x)

    features
}

#' removeZeros replaces all zeros-like values (in [-threshold,+threshold])
#' @title Zeros replacement
#' @description Replace all zeros-like values (in [-threshold,+threshold])
#' by +threshold (if positive values are required) or +/-threshold.
#' @param x numeric matrix or data.frame of raw data (points by line).
#' @param threshold numeric value; must be positive.
#' @param positive boolean: if TRUE, zeros-like values are replaced by +threshold
#' @return x numeric matrix or data.frame of raw data (points by line), with no zeros.
#'
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' x <- importSample(file.features=tf)
#' 
#' res <- removeZeros(x$features$initial$x)  
#' 
#' 
#' @keywords internal 
#' 
removeZeros <- function(x, threshold=.Machine$double.eps, positive=FALSE) {
    threshold <- abs(threshold)
    is.dtf <- inherits(x, "data.frame")
    # (fixes the log pb for supervised)
    x <- as.matrix(x)
    w = which(abs(x) <= threshold)
    if (length(w)) {
        s <- sign(x[w])
        s[s==0] <- 1
        if (positive)
            s[s==-1] <- 1
        x[w] <- s*threshold
    }
    if (is.dtf)
        x <- as.data.frame(x)
    x
}

#' computeItemsSample applies a specific predictive model for counting of number of cells in colonies for each cluster
#' @title Prediction of number of cells in colonies
#' @description Apply a specific predictive model for counting of number of cells in colonies for each cluster.
#' @param data.sample list containing features, profiles and clustering results.
#' @param method character vector specifying the name of the clustering result to use.
#' @param cluster character vector specifying the name of the cluster to consider for the application of the specific model.
#' @param modelFile character vector specifying the path and the name of the RData model file.
#' @return data.sample list containing features, profiles and clustering results with the number of cells for each particle.
#' @importFrom stats predict
#' @seealso \code{\link{itemsModel}}, \code{\link{countItems}} 
#'
#' @examples
#' \donttest{ 
#' dat <- rbind(matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 6, sd = 0.3), ncol = 2))
#'
#' colnames(dat) <- c("x","y")
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' x <- importSample(file.features=tf)
#' 
#' x <- computeUnSupervised(x, K=3, method.name="K-means")
#' x <- computeItemsSample(x, method="K-means", cluster="Cluster 1", modelFile=NULL)# to be fixed !
#'
#'
#' }
#' @keywords internal

computeItemsSample <- function (data.sample, method, cluster, modelFile=NULL) {

    if (is.null(modelFile))
        return(data.sample)

    object <- readRDS(modelFile)

    idxCluster <- which(data.sample$clustering[[method]]$label == cluster)
    dat <- data.sample$features$initial$x[idxCluster, 
                                          intersect(names(data.sample$features$initial$x), names(object$model))]

    if (inherits(object, "lm"))
        pred <- stats::predict(object, newdata = dat)
    else {
        if (inherits(object, "lda"))
            pred <- stats::predict(object, newdata = dat)$class
        else {
            if (inherits(object, "mda"))
                pred <- stats::predict(object, newdata = dat)
        }
    }

    ## Return the modified countings
    data.sample$clustering[[method]]$nbItems[names(pred)] <- pred
    data.sample
}

#' computeItemsSampleGUI opens a Graphical User Interface allowing to choose cluster name and model file for the estimation and the saving of the number of cells in colonies
#' @title GUI to estimate the number of cells in colonies for each cluster
#' @description Open a Graphical User Interface allowing to choose cluster name and model file for the estimation and the saving of the number of cells in colonies.
#' @param data.sample list containing features, profiles and clustering results.
#' @param method.select character vector specifying the name of the clustering result to use ('K-means' by default).
#' @param RclusTool.env environment in which all global parameters, raw data and results are stored.
#' @return data.sample with saved count results
#' @import tcltk
#' @return csv file containing the counts.
#' 
#' @examples
#' \donttest{ 
#'
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' x <- importSample(file.features=tf)
#' 
#' x <- computeUnSupervised(x, K=0, pca=TRUE, echo=TRUE)
#' computeItemsSampleGUI(x, method.select="K-means")
#'  
#' }
#' @keywords internal

computeItemsSampleGUI <- function(data.sample, method.select = "K-means", RclusTool.env=initParameters()) {
    if (is.null(data.sample$clustering[[method.select]]$label)){
        warning("computeItemsSampleGUI: no clustering results for the selected clustering method.")
        return(data.sample)
    }

    itemtt <- tktoplevel()
    tktitle(itemtt) <- "Process with items countings"

    items.env <- new.env()
    items.env$export.counts <- TRUE
    items.env$cluster.select <- NULL
    items.env$modelFile <- NULL

    OnExportCounts <- function() {
        items.env$export.counts <- tclvalue(tcl.export.counts) == "1"
    }

    ## Select the cluster
    OnClusterChange <- function() {
        items.env$cluster.select <- tclvalue(tcl.cluster.select)
        tkconfigure(tk.cluster.but, text = items.env$cluster.select)
    }

    ## Build the menu 'Clusters' for the selection of the clusters
    buildMenuClusters <- function() {
        tkconfigure(tk.cluster.but, text = items.env$cluster.select)
        tkdelete(tk.cluster.menu, 0, "end")
        for (name in levels(data.sample$clustering[[method.select]]$label)) 
            tkadd(tk.cluster.menu, "radio", label = name, variable = tcl.cluster.select,
                  command = OnClusterChange)
    }

    # Load model file
    onLoadModel <- function() {
        items.env$modelFile <- tclvalue(tkgetOpenFile(filetypes = "{{RData Files} {.RData}}"))
        if (!nchar(items.env$modelFile)) {
            tkmessageBox(message = "No file selected!")
        } else {
            tkgrid(tklabel(itemtt, text=basename(items.env$modelFile)), row = 2, column = 3, columnspan = 2)
        }
    }

    # Apply items counts model
    onApplyModel <- function() {
        # Call 'computeItemsSample' function
        if (is.character(items.env$modelFile))
            data.sample <- computeItemsSample(data.sample = data.sample,
                                                      method = method.select, cluster = items.env$cluster.select, modelFile = items.env$modelFile)
        # Save counts for this data.sample (csv file)
        if (items.env$export.counts) {
            fileCounts.csv <- paste("counts ", RclusTool.env$operator.name, " ",
                                              method.select, ".csv", sep="")
            saveCounts(fileCounts.csv, data.sample$clustering[[method.select]]$nbItems, dir=data.sample$files$results$clustering)
        }
    }

    ## Positioning the 'Cluster' button
    tkgrid(tklabel(itemtt, text="      "), row = 0, column = 0)
    items.env$cluster.select <- levels(data.sample$clustering[[method.select]]$label)[1]
    tcl.cluster.select <- tclVar(items.env$cluster.select)
    tk.cluster.but <- tkmenubutton(itemtt, text = items.env$cluster.select, relief = "raised", width = 15)
    tk.cluster.menu <- tkmenu(tk.cluster.but, tearoff = FALSE)  
    buildMenuClusters()
    tkconfigure(tk.cluster.but, menu = tk.cluster.menu)
    tkgrid(tk.cluster.but, row = 1, column = 1)

    ## Positioning the 'Load model' button
    butLoad <- tk2button(itemtt, text = "Load model file", image = "csvFile", compound = "left", width = 30, command = onLoadModel)
    tkgrid(butLoad, row = 1, column = 3, columnspan = 2)

    ## Positioning the 'Export counts' checkbox
    tcl.export.counts <- tclVar(as.character(as.integer(items.env$export.counts)))
    tk.export.counts <- tkcheckbutton(itemtt, text="", variable=tcl.export.counts,
                                      command=OnExportCounts)
    tkgrid(tk2label(itemtt, text="Export counts"), row=3, column=3, sticky="e")
    tkgrid(tk.export.counts, row=3, column=4, sticky="w")

    ## Positioning the 'Apply model' button
    tkgrid(tklabel(itemtt, text="      "), row = 2, column = 2)
    butApply <- tk2button(itemtt, text = "Apply model", width = -6, command = onApplyModel)
    tkgrid(butApply, row = 4, column = 3, columnspan = 2)
    tkgrid(tklabel(itemtt, text="      "), row = 5, column = 5)

    tkwait.window(itemtt)
    data.sample
}

#' itemsModel computes and saves specific predictive model from manual countings for the estimation of number of cells in colonies
#' @title Predictive models computation for the number of cells in colonies
#' @description Compute and save specific predictive model from manual countings for the estimation of number of cells in colonies.
#' @param dat matrix or data.frame of raw data (points by line).
#' @param countFile character vector specifying the path and the name of the file containing manual countings.
#' @param method character vector specifying the name of method tu use for the building of predictive models. Must be 'lm', 'lda' or 'mda' (default).
#' @importFrom stats as.formula lm
#' @import MASS
#' @import mda
#' @return RDS file containing the predictive model.
#' @seealso \code{\link{computeItemsSample}}, \code{\link{countItems}} 
#' 
#' @examples
#' \donttest{
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' x <- importSample(file.features=tf)
#' 
#' x <- computeUnSupervised(x, K=0, pca=TRUE, echo=TRUE)
#' 
#' countFile <- countItemsSampleGUI(x)
#' if (file.exists(countFile))
#'      itemsModel(x$features$initial$x, countFile)
#' }
#' @keywords internal


itemsModel <- function (dat, countFile, method = "mda") {
    ## Does 'class' exist?
    if (length(countFile) < 1 && !is.character(countFile))
        stop("'countFile' must be a single (or multiple) character string(s)")

    ## Does 'method' is a valid method?
    if (length(method) != 1 && !is.character(method))
        stop("'method' must be a single character string")
    if (!method %in% c("lm","lda","mda"))
        stop("'method' must be one of 'lm', 'lda' or 'mda'")

    class <- gsub("_itemsCount.RData", "", basename(countFile))
    modelPath <- file.path(dirname(countFile), paste(class, "_itemsModel.RData", sep = ""))

    message("Building predictive model...")
    nbItems <- readRDS(countFile)
    nbCounted <- nrow(nbItems)
    if (nbCounted < 1) {
        warning("Nothing counted for '", class, "'!", sep = "") 
    } else {
        data2 <- dat[rownames(nbItems), sapply(dat, is.numeric)]
        data2 <- data2[,-1]

        form <- stats::as.formula(unlist(nbItems[, "Manual.Number.of.items"]) ~ .)

        if (method == "lm")
            model <- stats::lm(form, data = data2)
        if (method == "lda")
            model <- MASS::lda(form, data = data2)
        if (method == "mda")
            model <- mda::mda(form, data = data2, start.method = "kmeans",
                         keep.fitted = TRUE, method = mda::gen.ridge, iter = 10)

        saveRDS(model, file = modelPath)
    }
    message("Done!")
    modelPath
}

#' countItems displays the profile and the image of each particle and allows the user to manually count the number of cells by simple left-clicking on each of them
#' @title Manually counting the number of cells in colonies
#' @description Display the profile and the image of each particle and allow the user to manually count the number of cells by simple left-clicking on each of them.
#' @param profile matrix of profile data (signals in columns).
#' @param feature vector of features data.
#' @param imgdir character vector specifying the path of the images directory.
#' @param image character vector specifying the name of the considered image in imgdir.
#' @importFrom grDevices dev.new dev.off
#' @importFrom graphics layout par matplot plot text legend locator polygon
#' @importFrom mmand erode
#' @importFrom sp point.in.polygon
#' @importFrom jpeg readJPEG
#' @importFrom png readPNG
#' @return nbItemsTot number of cells manually counted on each image.
#' @seealso \code{\link{itemsModel}}, \code{\link{computeItemsSample}} 
#' 
#' @examples
#' \donttest{ 
#' dat <- rbind(matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 6, sd = 0.3), ncol = 2))
#' colnames(dat) <- c("x","y")
#' tf1 <- tempfile()
#' write.table(dat, tf1, sep=",", dec=".")
#' 
#' sig <- data.frame(ID=rep(1:150, each=30), SIGNAL=rep(dnorm(seq(-2,2,length=30)),150))
#' tf2 <- tempfile()
#' write.table(sig, tf2, sep=",", dec=".")
#' 
#' x <- importSample(file.features=tf1, file.profiles=tf2)
#' 
#' nbItems <- countItems(x$profiles[[1]])
#' 
#' 
#' }
#' @keywords internal

countItems <- function(profile, feature = NULL, imgdir = NULL, image = NULL) {
    opar <- graphics::par(no.readonly=TRUE)
    on.exit(graphics::par(opar))
    nbItemsTot <- NULL
    nc <- ncol(profile)
    # nbFeatures <- names(feature$initial$x)[grep("Number.of.items", names(feature$initial$x))]

    grDevices::dev.new()
    graphics::layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(4,4))
    graphics::par(mar=c(3,3,2,.2))
    if (!is.null(nc)) {
        curve.names <- colnames(profile)
        #curve.colors <- rep("black", length(curve.names))
        curve.colors <- c("black","blue","yellow","orange","red","palevioletred")

        graphics::matplot(profile, pch=1:nc, type="o", col=curve.colors, main=image, ylab="level", cex = 0.8)

        if (!is.null(curve.names))
            graphics::legend("topright", legend=curve.names, col=curve.colors, pch=1:nc, cex=.8)
    } else {
        graphics::plot(1, col="white", axes=FALSE, xlab=NA, ylab=NA)
        graphics::text(1, col = "black", labels = "No signals")
    }

    graphics::par(mar=c(5,.2,5,.2))
    if (!is.null(image) && (!identical(image, ""))) {
        imgFile <- file.path(imgdir, image)
        readImg <- jpeg::readJPEG
        if (!grepl(".jpg", imgFile))
            readImg <- png::readPNG
        if (grepl(".jpg", imgFile) || grepl(".png", imgFile)) {
            img <- readImg(imgFile, native = FALSE)

            ## Mask computation
            gray <- 60 / 255 # Target something like 60
            threshold <- 1 - 2 * gray
            mask <- matrix(1, nrow = nrow(img[,,1]), ncol = ncol(img[,,1]))
            mask[img[,,1] > threshold] <- 0
            mask_erode <- mmand::erode(mask, c(1,1,1))

            dimg <- dim(img)
            h <- dimg[1]
            w <- dimg[2]
            rotate <- function(x) t(apply(x, 2, rev))
            image(rotate(img[,,1]), col = gray((0:255)/255), axes = FALSE)
            #       title(paste0(
            #         "\n\nFWS_NbCells: ",
            #         toString(signif(feature[[nbFeatures[grep("FWS",nbFeatures)][1]]]), digits = 4), 
            #         "\nSWS_NbCells: ",
            #         toString(signif(feature[[nbFeatures[grep("SWS",nbFeatures)][1]]]), digits = 4),
            #         "\nFLR_NbCells: ",
            #         toString(signif(feature[[nbFeatures[grep("FL.Red",nbFeatures)][1]]]), digits = 4), "\n\n\n"),
            #         adj = 0, font.main = 1, cex.main = .8)
            graphics::title(sub = "1. Click 4 polygon vertices (subregion).\n2. Click items and right-click when done.\nClose windows to end.", 
                  adj = 1, font.sub = 2, cex.sub = .8)

            rectCoord <- graphics::locator(4, type = "o", col = "blue", pch = 16, cex = 2)
            graphics::polygon(rectCoord$x, rectCoord$y, border = "blue")
            xSub <- dim(mask_erode)[1] - round(rectCoord$y*dim(mask_erode)[1])
            ySub <- round(rectCoord$x*dim(mask_erode)[2])
            idxTot <- which(mask_erode == 0, arr.ind=TRUE)

            idx <- sp::point.in.polygon(idxTot[,1], idxTot[,2], xSub, ySub, mode.checked = FALSE)
            #       maskSub <- mask_erode
            #       maskSub[idxTot[which(idx == 1), ]] <- 0.5
            pixSub <- length(which(idx==1))
            nb <- graphics::locator(10000, type = "p", col = "red", pch = 16, cex = 2)
            n <- length(nb$x)
            densiteSub <- n/pixSub
            ## Number of dark pixels in whole image
            pixTot <- length(which(mask_erode==0))
            nbItemsTot <- densiteSub * pixTot
        }
    } else {
        graphics::plot(1, col= "white", axes=FALSE, xlab=NA, ylab=NA)
        graphics::text(1, col = "black", labels = "No image")
    }
    grDevices::dev.off()
    nbItemsTot
}

## GUI function for counting items on images
#' countItemsSampleGUI opens a Graphical User Interface allowing to choose the images directory, to count manually the number of cells in colonies and to build specific predictive model
#' @title GUI to manually count the number of cells in colonies
#' @description Open a Graphical User Interface allowing to choose the images directory, to count manually the number of cells in colonies and to build specific predictive model.
#' @param data.sample list containing features, profiles and clustering results.
#' @param RclusTool.env environment in which all global parameters, raw data and results are stored.
#' @return RDS file containing the counts.
#' 
#' @examples
#' \donttest{ 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' x <- importSample(file.features=tf)
#' 
#' x <- computeUnSupervised(x, K=0, pca=TRUE, echo=TRUE)
#' countResult <- countItemsSampleGUI(x)
#' }
#' @keywords internal

countItemsSampleGUI <- function (data.sample, RclusTool.env=initParameters()) {
    ## Get the training set directory
    imgdir <- tk_choose.dir()
    if (is.na(imgdir))
        return("")

    ## Ask to reset (if something is already set)
    countPath <- file.path(dirname(imgdir), paste(basename(imgdir), "_itemsCount.RData", sep = ""))
    reset <- FALSE
    if (file.exists(countPath)) {
        nbItems <- readRDS(countPath)
        ncount <- nrow(nbItems)
        if (ncount > 0) {
            if (ncount == 1) {
                msg <- "There is one image processed. Do you want to keep its count?"
            } else msg <- paste("There are", ncount,
                                "images already processed. Do you want to keep these counts?")
            res <- tkmessageBox(message = msg, icon = "question", type = "yesnocancel", default = "yes")
            if (tclvalue(res) == "cancel") return("")
            reset = (tclvalue(res) == "no")    
        }        
    }

    if (!(file.exists(countPath)) || isTRUE(reset))
        nbItems <- NULL

    ## Numbers (ID) of imaged particles
    img <- list.files(imgdir, pattern = ".jpg")
    for (i in 1:length(img)) {
        imgNum <- gsub(".jpg", "", img[i])
        imgNum <- as.numeric(imgNum)

        if (!(imgNum %in% row.names(nbItems))) {
            ## Keep the initial 'Number.of.items' features + Manual counting
            nb <- c(data.sample$features$initial$x[imgNum,], 
                    countItems(profile = data.sample$profiles[[imgNum]],
                               feature = data.sample$features$initial$x[imgNum,],
                               imgdir = imgdir, image = img[i]))

            nbItems <- rbind(nbItems, nb)
            rownames(nbItems)[nrow(nbItems)] <- imgNum
            colnames(nbItems)[ncol(nbItems)] <- "Manual.Number.of.items"
            message(nbItems)
        }
    }
    ## Save the counts ('Number.of.items' features + Manual counts)
    saveRDS(nbItems, countPath)
    ## Call 'itemsModel' function to build predictive models
    itemsModel(dat = data.sample$features$initial$x, countFile = countPath, method = "lm")
}

#' toStringDataFrame convert dataframe to string to print it in console
#' @title To String Data Frame
#' @description Convert dataframe to string to print it in console.
#' @param object dataframe to convert to string
#' @param digits digits in dataframe
#' @return string to print.
#' @import stringr  
#' @references \url{https://stackoverflow.com/a/45541857}
#' @keywords internal


toStringDataFrame = function (object, digits=NULL) {
    nRows = length(row.names(object));
    if (length(object)==0) {
        return(paste(
                    sprintf(ngettext(nRows, "data frame with 0 columns and %d row", "data frame with 0 columns and %d rows")
                            , nRows)
                    , "\\n", sep = "")
                ); 
    } else if (nRows==0) {
        return(gettext("<0 rows> (or 0-length row.names)\\n")); 
    } else {
        # get text-formatted version of the data.frame
        m = as.matrix(format.data.frame(object, digits=digits, na.encode=FALSE)); 
        # add column headers
        m = rbind(dimnames(m)[[2]], m);
        # max-length per-column
        maxLen = apply(apply(m, c(1,2), stringr::str_length), 2, max, na.rm=TRUE);

        # add right padding
        ##  t is needed because "If each call to FUN returns a vector of length n, then apply returns an array of dimension c(n, dim(X)[MARGIN])"
        m = t(apply(m, 1, stringr::str_pad, width=maxLen, side="right"));
        m = t(apply(m, 1, stringr::str_pad, width=maxLen+3, side="left"));
        # merge columns
        m = apply(m, 1, paste, collapse="");
        # merge rows (and return)
        return(paste(m, collapse="\n"));
    }
}


