#' simulated PCs
#' 
#' A dataset containing PCs
#'
#' @format It is a matrix containing 5 PCs
#'
#' the variables are listed as following
#' \describe{
#'   \item{PC1}{The 1th PC}
#'   \item{PC2}{The 2th PC}
#'   ...
#'   \item{PC5}{The 5th PC}
#' }
#' @usage data(PC)
#' @examples
#' ## run the PC with the Gaussian mixture model
#' data(PC)
#' out1 = mclust::Mclust(PC,G = 2)
"PC"



#' A simulated SingleCellExperiment
#' 
#' A dataset of SingleCellExperiment
#'
#' @format It is a SingleCellExperiment object with gene expression and meta information
#' 
#' @usage data(sce)
#' @references Amezquita R A, Lun A T L, Becht E, et al. Orchestrating single-cell analysis with Bioconductor[J]. Nature methods, 2020, 17(2): 137-145.
#' @examples
#' ## find the neighborhood of spots in SingleCellExperiment
#' data(sce)
#' out = find_neighbors2(sce, "ST")
"sce"