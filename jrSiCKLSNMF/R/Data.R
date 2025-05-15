NULL
#' A simulated dataset for use with jrSiCKLSNMF
#' @description A simulated dataset with \eqn{\mathcal{U}(1,1.25)} multiplicative noise for the
#' scRNA-seq variability parameter in SPARSim for the simulated scRNA-seq data and
#' with \eqn{\mathcal{N}(-0.25,0.25)} additive noise to the expression levels of the scATAC-seq data
#' for data simulated via SimATAC. The simulated matrices are located in SimData$Xmatrices
#' and the identities for the cell types are contained in SimData$cell_type. This corresponds
#' to the Xmatrix data found in both
#'
#' XandLmatrices25/XandindividLKNNLmatrices1Sparsity5.RData and
#'
#' XandBulkLmatrix25/XandBulkLKNNmatrices1Sparsity5.RData on our Github
#'
#' \href{https://github.com/ellisdoro/jrSiCKLSNMF_Simulations}{ellisdoro/jrSiCKLSNMF_Simulations}
#'
#' @docType data
#'
#' @usage data(SimData)
#'
#' @format A list made up of a two items. The first is list of 2 simulated sparse matrices
#' and the second is a vector containing cell identities.
#' \describe{
#' \item{Xmatrices}{A list of 2 sparse matrices, each containing a different
#' simulated omics modality measured on the same set of single cells: the first entry in the list
#' corresponds to simulated scRNA-seq data and has 1000 genes and 300 cells; the second entry in
#' the list corresponds to simulated scATAC-seq data and has 5910 peaks and 300
#' cells.}
#' \item{cell_type}{A vector containing the cell-type identities of the simulated data}}
#' @keywords datasets
#' @source \href{https://github.com/ellisdoro/jrSiCKLSNMF_Simulations}{jrSicKLSNMF Simulations}
#'
"SimData"

#' A small SickleJr object containing a subset of data from the
#' \verb{SimData} data object. Contains the completed analysis from the
#' `Getting Started` vignette for a small subset of 10 cells with 150 genes and
#' 700 peaks. The clusters derived from this dataset are not accurate; this dataset
#' is intended for use with code examples.
#'
#' @docType data
#'
#' @usage data(SimSickleJrSmall)
#'
#' @format A SickleJr object containing a completed analysis using jrSiCKLSNMF
#' \describe{
#' \item{count.matrices}{Contains a list of 2 sparse matrices, each containing a different
#' simulated omics modality measured on the same set of single cells}
#' \item{normalized.count.matrices}{The normalized versions of the count matrices
#' contained in slot \code{count.matrices}}
#' \item{graph.laplacian.list}{A list of sparse matrices containing the graph
#' Laplacians corresponding to the KNN feature-feature similarity graphs constructed
#' for each omics modality}
#' \item{rowRegularization}{A string indicating the row regularization: here it
#' is set to \code{"None"}}
#' \item{diffFunc}{A string specifying the function to measure the discrepancy
#' between the normalized data and the fitted matrices: here, it is set to "klp"
#' for the Poisson Kullback-Leibler divergence}
#' \item{lambdaWlist}{A list holding the graph regularization parameters: here,
#' they are 10 and 50}
#' \item{lambdaH}{A numeric indicating the value for the sparsity parameter.
#' Here it is equals 500}
#' \item{Wlist}{A list holding the fitted \eqn{\mathbf{W}^v} matrices}
#' \item{H}{A matrix holding \eqn{\mathbf{H}}}
#' \item{WHinitials}{A list of initial values for \eqn{\mathbf{W}^v} and \eqn{\mathbf{H}}}
#' \item{lossCalcSubsample}{A vector containing a subset on which to calculate the loss}
#' \item{latent.factor.elbow.values}{A data frame holding the loss and the number of latent factor that is used for diagnostic plots}
#' \item{minibatch}{A Boolean indicating whether or not to use the mini-batch algorithm: \code{FALSE} here}
#' \item{clusterdiagnostics}{Diagnostic plots and results}
#' \item{clusters}{A list holding the \code{"kmeans"} clustering results}
#' \item{metadata}{A list holding metadata; here this is just cell type information}
#' \item{loss}{A list holding a vector called "Loss"}
#' \item{umap}{A list holding various UMAP approximations}
#' \item{plots}{A list holding ggplots corresponding to different diagnostics and visualizations}
#' }
#' @keywords datasets
#' @source \href{https://github.com/ellisdoro/jrSiCKLSNMF_Simulations}{jrSicKLSNMF Simulations}
"SimSickleJrSmall"
