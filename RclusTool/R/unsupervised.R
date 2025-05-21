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

#' computeUnSupervised performs unsupervised clustering, dealing with the number of clusters K, automatically or not
#' @title Unsupervised clustering  
#' @description Perform unsupervised clustering, dealing with the number of clusters K, automatically or not.
#' @param data.sample list containing features, profiles and clustering results.
#' @param K number of clusters. If K=0 (default), this number is automatically computed thanks to the Elbow method.
#' @param method.name character vector specifying the constrained algorithm to use. Must be 'K-means' (default), 'EM' (Expectation-Maximization), 'Spectral', 'HC' (Hierarchical Clustering) or 'PAM' (Partitioning Around Medoids).
#' @param pca boolean: if TRUE, Principal Components Analysis is applied to reduce the data space.
#' @param pca.nb.dims number of principal components kept. If pca.nb.dims=0, this number is computed automatically.
#' @param spec boolean: if TRUE, spectral embedding is applied to reduce the data space.
#' @param scaling boolean: if TRUE, scaling is applied.
#' @param use.sampling boolean: if FALSE (default), data sampling is not used.
#' @param sampling.size.max numeric: maximal size of the sampling set.
#' @param RclusTool.env environment in which all global parameters, raw data and results are stored.
#' @param echo boolean: if FALSE (default), no description printed in the console.
#' @return data.sample list containing features, profiles and updated clustering results (with vector of labels and clusters summaries).
#' @importFrom stats dist cutree hclust
#' @importFrom cluster pam
#' @seealso \code{\link{computeKmeans}}, \code{\link{computeEM}}, \code{\link{spectralClustering}}, \code{\link{computePcaSample}}, \code{\link{computeSpectralEmbeddingSample}}
#'
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' tf <- tempfile()
#' write.table(dat, tf, sep=",", dec=".")
#' x <- importSample(file.features=tf)
#' 
#' x <- computeUnSupervised(x, K=0, pca=TRUE, echo=TRUE)
#' label <- x$clustering[["K-means_pca"]]$label
#' plot(dat[,1], dat[,2], type = "p", xlab = "x", ylab = "y", 
#'	    col = label, main = "K-means clustering")
#' 
#'
#' @export 
#' 
computeUnSupervised <- function(data.sample, K=0, method.name="K-means",  pca=FALSE, pca.nb.dims=0, spec=FALSE,
                                use.sampling=FALSE, sampling.size.max=0, scaling=FALSE, RclusTool.env=initParameters(), echo=FALSE) {

    summary <- NULL
    label <- NULL
    x <- NULL
    protos <- NULL
    sim <- NULL
    nbItems <- NULL
    Within <- NULL
	
    operationsRes <- makeFeatureSpaceOperations(pca=pca, pca.nb.dims=pca.nb.dims, spectral=spec,
                                    sampling=use.sampling, sampling.size.max=sampling.size.max, scaling=scaling)

    new.data.sample <- applyPreprocessing(data.sample, operationsRes$instr, RclusTool.env, reset=FALSE)
    if (!is.null(new.data.sample)) {
        data.sample <- new.data.sample
    } else
        stop("Unsupervised classification fails: preprocessing error.")

    space <- operationsRes$space
    featNames <- colnames(data.sample$features[[space]]$x)


    x <- data.sample$features[[space]]$x

    if (grepl("preprocessed", space) || (space=="scaled"))
    {
        selected.var <- intersect(data.sample$config$selectFeat, featNames)  
        if (!length(selected.var))
            selected.var <- featNames

        x <- x[, selected.var, drop=FALSE]
    }

    if ((!spec)&&(!pca))
        x <- x[data.sample$id.clean, , drop=FALSE]
    
    #Sampling
    if (use.sampling) {
        if (!spec)
            x <- x[data.sample$sampling$selection.ids, , drop=FALSE]
    }

    # to correctly call further formatLabelSample
    if (spec && !is.null(data.sample$sampling) && (nrow(x)==length(data.sample$sampling$selection.ids)))
        use.sampling <- TRUE 

    #Clustering
    if (!is.null(x)) {
        if (method.name=="K-means") {
            res.kmeans <- computeKmeans(x, K=K, K.max=RclusTool.env$param$classif$unsup$K.max,
                                        kmeans.variance.min=RclusTool.env$param$classif$unsup$kmeans.variance.min, graph=FALSE)
            label <- res.kmeans$cluster
            Within <- res.kmeans[["Withins_tot"]]
        }

        if (method.name=="EM") {
            res.EM <- computeEM(x, K=K, K.max=RclusTool.env$param$classif$unsup$K.max,
                                kmeans.variance.min=RclusTool.env$param$classif$unsup$kmeans.variance.min, graph=FALSE, Mclust.options=RclusTool.env$param$classif$sup[["Mclust"]])
            label <- res.EM$classification
            Within <- res.EM[["Withins_tot"]]
        }

        if (method.name=="Spectral") {
            message("Similarity computing")
            sim <- computeGaussianSimilarityZP(x, k=RclusTool.env$param$classif$unsup$nb.neighbours)
            if (K==0) {
                message("Cluster number estimating")
                res.gap <- computeGap(sim, Kmax=RclusTool.env$param$classif$unsup$K.max)
                message(paste("  obtained K=", res.gap$Kmax))
                K <- res.gap$Kmax
            }
            message(paste("Spectral clustering computing with ZP, #neighbours=", RclusTool.env$param$classif$unsup$nb.neighbours))
            # Must check this spectral clustering method (bug!)
            #res.sc <- spectralClusteringNg(sim, K)
            res.sc <- spectralClustering(sim, K)

            label <- res.sc$label
            # Only for spectralClusteringNg function (with PAM)...!
            #protos <- label[res.sc$id.med]
            #         data.sample$features[["spectral"]] <- computeSpectralEmbeddingSample(data.sample, 
            #                                                                                    res.sc, use.sampling)
            #         data.sample$features[["spectral"]] <- computeSpectralEmbeddingSample(data.sample,
            #                                                                                        use.sampling)
        }

        if (method.name=="HC") {
            if (K==0) {
                res.kmeans <- computeKmeans(x, K=0, K.max=RclusTool.env$param$classif$unsup$K.max,
                                            kmeans.variance.min=RclusTool.env$param$classif$unsup$kmeans.variance.min, graph=FALSE)
                Within <- res.kmeans[["Withins_tot"]]
                K <- max(res.kmeans$cluster)
            }
            if (echo) message(paste("Hierarchical Clustering computing, K=", K))
            res.hc <- do.call(stats::hclust, c(list(stats::dist(x)), RclusTool.env$param$classif$sup[["hclust"]]))
            label <- stats::cutree(res.hc, K)
        }

        if (method.name=="PAM") {
            if (K==0) {
                res.kmeans <- computeKmeans(x, K=0, K.max=RclusTool.env$param$classif$unsup$K.max,
                                            kmeans.variance.min=RclusTool.env$param$classif$unsup$kmeans.variance.min, graph=FALSE)
                Within <- res.kmeans[["Withins_tot"]]
                K <- max(res.kmeans$cluster)
            }
            if (echo) message(paste("PAM, K=", K))
            res.pam <- cluster::pam(x, K, diss = FALSE)
            label <- res.pam$clustering
            #protos <- label[res.pam$id.med] ????
        }

        label <- formatLabelSample(label, data.sample, use.sampling=use.sampling, noise.cluster=RclusTool.env$param$preprocess$noise.cluster)
        protos <- as.character(protos)

        #visualizing
        summary <- clusterSummary(data.sample, label, summary.functions=RclusTool.env$param$analysis$summary.functions)
		density <- clusterDensity(data.sample, label, space)
		
        nbItems <- rep(1, length(label))
        names(nbItems) <- names(label)
        
        method.space.name <- paste(method.name,space,sep="_")

        data.sample$clustering[[method.space.name]] <- list(label=label, summary=summary, nbItems=nbItems,
                                                      prototypes=protos, sim=sim, K=K, Within=Within, density=density)
        data.sample

    } else {
        message("No space computed...!!!")
        NULL
    }
}
