#' Components for WormTensor object
#'
#' These are generic methods for WormTensor
#' @name AllGenerics_component
#' @rdname AllGenerics_component
#' @docType methods
#' @aliases worm_membership
#' @aliases worm_clustering
#' @aliases worm_evaluate
#' @aliases worm_visualize
NULL

#' @rdname AllGenerics_component
#' @param object WormTensor object
#' @param k Assumed number of clusters
#' @export
# worm_membership
setGeneric(
    "worm_membership",
    function(object, k) {
        standardGeneric("worm_membership")
    }
)

#' @rdname AllGenerics_component
#' @param object WormTensor object
#' @param num.iter The upper limit of iterations (Default value is 30)
#' @param thr The lower limit of relative change in estimates
#' (Default value is 1E-10)
#' @param verbose Control message
#' @param algorithm Clustering methods
#' @export
# worm_clustering
setGeneric(
    "worm_clustering",
    function(object,
             algorithm = c("MCMI", "OINDSCAL", "CSPA"),
             num.iter = 30,
             thr = 1E-10,
             verbose = FALSE) {
        standardGeneric("worm_clustering")
    }
)

#' @rdname AllGenerics_component
#' @param object WormTensor object
#' @param labels Labels for external evaluation
#' @export
# worm_evaluate
setGeneric(
    "worm_evaluate",
    function(object, labels = NULL) {
        standardGeneric("worm_evaluate")
    }
)

#' @rdname AllGenerics_component
#' @param object WormTensor object
#' @param out.dir Output directory (default: tempdir())
#' @param algorithm Dimensional reduction methods
#' @param seed Arguments passed to set.seed (default: 1234)
#' @param tsne.dims Output dimensionality (default: 2)
#' @param tsne.perplexity Perplexity paramete (default: 15)
#' @param tsne.verbose logical; Whether progress updates should be printed
#' (default: TRUE)
#' @param tsne.max_iter The number of iterations (default: 1000)
#' @param umap.n_neighbors The size of local neighborhood (default: 15)
#' @param umap.n_components The dimension of the space to embed into
#' (default: 2)
#' @param silhouette.summary logical; If true a summary of
#' cluster silhouettes are printed.
#' @export
# worm_visualize
setGeneric(
    "worm_visualize",
    function(object,
             out.dir = tempdir(),
             algorithm = c("tSNE", "UMAP"),
             seed = 1234,
             tsne.dims = 2,
             tsne.perplexity = 15,
             tsne.verbose = FALSE,
             tsne.max_iter = 1000,
             umap.n_neighbors = 15,
             umap.n_components = 2,
             silhouette.summary = FALSE) {
        standardGeneric("worm_visualize")
    }
)
