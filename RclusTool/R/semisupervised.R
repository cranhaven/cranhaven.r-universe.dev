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

#' computeSemiSupervised performs semi-supervised clustering based on pairwise constraints, dealing with the number of clusters K, automatically or not
#' @title Semi-supervised clustering 
#' @description Perform semi-supervised clustering based on pairwise constraints, dealing with the number of clusters K, automatically or not.
#' @param data.sample list containing features, profiles and clustering results.
#' @param ML list of ML (must-link) constrained pairs (as row.names of features).
#' @param CNL list of CNL (cannot-link) constrained pairs (as row.names of features).
#' @param K number of clusters. If K=0 (default), this number is automatically computed thanks to the Elbow method.
#' @param kmax maximum number of clusters.
#' @param method.name character vector specifying the constrained algorithm to use. Must be 'Constrained_KM' (default) or 'Constrained_SC' (Constrained Spectral Clustering).
#' @param maxIter number of iterations for SemiSupervised algorithm
#' @param pca boolean: if TRUE, Principal Components Analysis is applied to reduce the data space.
#' @param pca.nb.dims number of principal components kept. If pca.nb.dims=0, this number is computed automatically.
#' @param spec boolean: if TRUE, spectral embedding is applied to reduce the data space.
#' @param use.sampling boolean: if FALSE (default), data sampling is not used.
#' @param sampling.size.max numeric: maximal size of the sampling set.
#' @param scaling boolean: if TRUE, scaling is applied.
#' @param RclusTool.env environment in which data and intermediate results are stored.
#' @param echo boolean: if FALSE (default), no description printed in the console.
#' @return The function returns a list containing:
#' \item{label}{vector of labels.}
#' \item{summary}{data.frame containing clusters summaries (min, max, sum, average, sd).}
#' \item{nbItems}{number of observations.}
#' @seealso \code{\link{computeCKmeans}}, \code{\link{computeCSC}}, \code{\link{KwaySSSC}}
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
#' pairs.abs <- visualizeSampleClustering(x, selection.mode = "pairs", 
#'			    profile.mode="whole sample", wait.close=TRUE)
#' 
#' res.ckm <- computeSemiSupervised(x, ML=pairs.abs$ML, CNL=pairs.abs$CNL, K=0)
#' plot(dat[,1], dat[,2], type = "p", xlab = "x", ylab = "y",
#'	    col = res.ckm$label, main = "Constrained K-means clustering")
#'
#'
#' }
#' 
#' @export 
#' 
computeSemiSupervised <- function(data.sample, ML, CNL, K=0, kmax=20, method.name = "Constrained_KM", maxIter=2, pca=FALSE, pca.nb.dims=0, spec=FALSE,
								  use.sampling=FALSE, sampling.size.max=0, scaling=FALSE, RclusTool.env=initParameters(), echo=TRUE) {
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
        {
            toAdd=c(unlist(ML),unlist(CNL))
            data.sample$sampling <- addIds2Sampling(data.sample$sampling, toAdd )
            x <- x[data.sample$sampling$selection.ids, , drop=FALSE]
        }
    }

    # to correctly call further formatLabelSample
    if (spec && !is.null(data.sample$sampling) && (nrow(x)==length(data.sample$sampling$selection.ids)))
        use.sampling <- TRUE 

	#Clustering
    label <- NULL
	if (!is.null(x)) { #???
    	if (method.name=="Constrained_KM") {
        	res.CKmeans <- computeCKmeans(x=x, K=K, K.max=RclusTool.env$param$classif$unsup$K.max,
                                      	mustLink=ML, cantLink=CNL, maxIter=maxIter, kmeans.variance.min=RclusTool.env$param$classif$unsup$kmeans.variance.min)
        	label <- res.CKmeans$label
        	Within <- res.CKmeans$Within
    	}

    	if (method.name=="Constrained_SC") {
        	res.CSC <- computeCSC(x=x, K=K, K.max=RclusTool.env$param$classif$unsup$K.max, mustLink=ML, cantLink=CNL, 
                              	alphas=seq(from=0, to=1, length=100))
        	label <- res.CSC$label
    	}

    # Format labels and summaries after semi-sup classification
    label <- as.factor(label)
    label <- formatLabelSample(label, data.sample, new.labels=TRUE, use.sampling=use.sampling, noise.cluster=RclusTool.env$param$preprocess$noise.cluster)
    summary <- clusterSummary(data.sample, label, summary.functions=RclusTool.env$param$analysis$summary.functions)

    nbItems <- rep(1, length(label))
    names(nbItems) <- names(label)
    
    method.space.name <- paste(method.name,space,sep="_")

    data.sample$clustering[[method.space.name]] <- list(label=label, summary=summary, nbItems=nbItems, Within=Within)
    
    data.sample$clustering[[method.space.name]]
    } else {
        message("No space computed...!!!")
        NULL
    }  
}
