#' Geodesic distance between trees
#'
#' For any trimmed matrix produced with
#' \code{\link[=trimHS_maxC]{trimHS_maxC()}} it prunes the host-symbiont
#' phylogenies to conform with the trimmed matrix and computes geodesic
#' distance between the pruned trees.
#' \code{NOTE}: This function can only be used with strictly bifurcating trees.
#'
#' @param ths A trimmed matrix.
#'
#' @param treeH Host phylogeny. An object of class \code{"phylo"}.
#'
#' @param treeS Symbiont phylogeny. An object of class \code{"phylo"}.
#'
#' @param strat Flag indicating whether execution is to be  \code{"sequential"}
#'        or \code{"parallel"}. Default is \code{"sequential"},
#'        resolves \R expressions sequentially in the current \R
#'        process. If \code{"parallel"} resolves \R expressions in parallel in
#'        separate \R sessions running in the background.
#'
#' @param cl Number of cluster to be used for parallel computing.
#'        \code{\link[parallelly:availableCores]{parallelly::availableCores()}}
#'        returns the number of clusters available.
#'        Default is \code{cl = 1} resulting in \code{"sequential"} execution.
#'
#' @section NOTE:
#'       The \code{node.label} object in both trees can not contain NAs or null
#'       values (i.e. no numeric value). All nodes should have a value. Else
#'       remove node labels within the \code{"phylo"} class tree
#'       with \code{tree$node.label <- NULL}. For more details, see
#'       \code{\link[distory:dist.multiPhylo]{distory::dist.multiPhylo()}}.
#'
#'       This function can not be used with the trimmed matrices produced
#'       with \code{\link[=trimHS_maxI]{trimHS_maxI()}} or with the algorithm
#'       \code{\link[=max_incong]{max_incong()}} in datasets with
#'       multiple host-symbiont associations.
#'
#' @return Geodesic distance
#'
#' @examples
#' data(amph_trem)
#' N = 10 #for the example, we recommend 1e+4 value
#' n = 8
#'
#' TAM <- trimHS_maxC(N, am_matrix, n, check.unique = TRUE)
#' GD <- geo_D(TAM, amphipod, trematode, strat = "sequential", cl = 1)
#'
#' @source
#' Balbuena J.A., Perez-Escobar O.A., Llopis-Belenguer C., Blasco-Costa I.
#' (2022). User’s Guide Random Tanglegram Partitions V.1.0.0. Zenodo.
#'
#' @references
#' Schardl C.L., Craven K.D., Speakman S., Stromberg A., Lindstrom A.,
#' Yoshida R. (2008). A Novel Test for Host-Symbiont Codivergence Indicates
#' Ancient Origin of Fungal Endophytes in Grasses. Systematic Biology.
#' 57:483–498.
#'
#' Balbuena J.A., Perez-Escobar Ó.A., Llopis-Belenguer C., Blasco-Costa I.
#' (2020). Random Tanglegram Partitions (Random TaPas): An Alexandrian Approach
#' to the Cophylogenetic Gordian Knot. Systematic Biology. 69:1212–1230.
#'
#' @import ape
#' @import distory
#' @import parallel
#' @importFrom parallelly makeClusterPSOCK
#'
#' @export
#'
geo_D <- function(ths, treeH, treeS,
                  strat = "sequential", cl = 1) {

  if(is.binary(treeH) == FALSE | is.binary(treeS) == FALSE)
    stop("All trees must be strictly binary")

  geoD <- function (ths, treeH, treeS) {
    treeh <- ape::drop.tip(treeH, setdiff(treeH$tip.label, rownames(ths)))
    trees <- ape::drop.tip(treeS, setdiff(treeS$tip.label, colnames(ths)))

    ths <- ths[treeh$tip.label, trees$tip.label]

    ths.lut <- which(ths[treeh$tip.label, trees$tip.label] == 1, arr.ind = TRUE)
    dummy.labels <- rownames(ths.lut)
    trees$tip.label <- dummy.labels
    combo.tree <- list(treeh, trees)
    gd <- distory::dist.multiPhylo(combo.tree)
    return(gd)
  }

  strat.choice <- c("sequential", "parallel")
  if (strat %in% strat.choice == FALSE)
    stop(writeLines("Invalid strategy parameter"))

  if (strat == "sequential") {
    gd_f <- sapply(ths, geoD, treeH = treeH, treeS = treeS)
    return(gd_f)
  } else {
    cores <- parallelly::makeClusterPSOCK(workers = cl)
    gd_f <- parallel::parSapply(cores, ths, geoD, treeH = treeH, treeS = treeS)
    parallel::stopCluster(cores)
    return(gd_f)
  }
}
