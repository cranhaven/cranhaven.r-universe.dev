#' Test of host-symbiont coevolution
#'
#' For any trimmed matrix produced with
#' \code{\link[=trimHS_maxC]{trimHS_maxC()}} or
#' \code{\link[=trimHS_maxI]{trimHS_maxI()}}, it prunes the host (H) and
#' symbiont (S) phylogenies to conform with the trimmed matrix and runs
#' \code{\link[ape:parafit]{ape::parafit()}} (Legendre et al. 2002) to
#' calculate the ParaFitGlobal Statistic.
#'
#' @param ths Trimmed matrix.
#'
#' @param treeH Host phylogeny. An object of class \code{"phylo"}.
#'
#' @param treeS Symbiont phylogeny. An object of class \code{"phylo"}.
#'
#' @param ei.correct Specifies how to correct potential negative eigenvalues
#'        from the conversion of phylogenetic distances into Principal
#'        Coordinates: \code{"none"} (the default) indicates that no correction
#'        is applied, particularly if H and S are ultrametric; \code{"sqrt.D"}
#'        takes the element-wise square-root of the phylogenetic distances;
#'        \code{"lingoes"} and \code{"cailliez"} apply the classical Lingoes and
#'        Cailliez corrections, respectively.
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
#' @return A number object with the ParaFitGlobal Statistic of host-symbiont
#'         test for the N trimmed matrix.
#'
#'
#' @references
#' Legendre P., Desdevises Y., Bazin E. (2002). A Statistical Test for
#' Host–Parasite Coevolution. Systematic Biology. 51:217–234.
#'
#' Balbuena J.A., Perez-Escobar O.A., Llopis-Belenguer C., Blasco-Costa I.
#' (2020). Random Tanglegram Partitions (Random TaPas): An Alexandrian Approach
#' to the Cophylogenetic Gordian Knot. Systematic Biology. 69:1212–1230.
#'
#'
#'
#' @import parallel
#' @importFrom parallelly makeClusterPSOCK
#'
#'
#' @export
#'
#' @examples
#' data(amph_trem)
#' N = 10 #for the example, we recommend 1e+4 value
#' n = 8
#'
#' TAM <- trimHS_maxC(N, am_matrix, n, check.unique = TRUE)
#' PF <- paraF(TAM, amphipod, trematode, ei.correct = "sqrt.D",
#'             strat = "parallel", cl = 8)
#'
paraF <- function (ths, treeH, treeS, ei.correct = "none",
                     strat = "sequential", cl = 1) {

  paraf <- function (ths, treeH, treeS, ...) {

    eigen.choice <- c("none", "lingoes", "cailliez", "sqrt.D")
    if (ei.correct %in% eigen.choice == FALSE)
      stop(writeLines("Invalid eigenvalue correction parameter.\r
               Correct choices are 'none', 'lingoes', 'cailliez' or 'sqrt.D'"))

    treeh <- ape::drop.tip(treeH, setdiff(treeH$tip.label, rownames(ths)))
    trees <- ape::drop.tip(treeS, setdiff(treeS$tip.label, colnames(ths)))
    if(is.null(treeh)==TRUE | is.null(trees)==TRUE) {PF <- NA} else {
    # Reorder ths as per tree labels:
    ths <- ths[treeh$tip.label, trees$tip.label]
    DH <- ape::cophenetic.phylo(treeh)
    DP <- ape::cophenetic.phylo(trees)
    if(ei.correct == "sqrt.D"){DH <- sqrt(DH); DP <- sqrt(DP); ei.correct ="none"}
    PF <- ape::parafit(DH, DP, ths, nperm = 0, silent = TRUE, correction = ei.correct)
    PF <- PF$ParaFitGlobal
    }
    return(PF)
  }

  strat.choice <- c("sequential", "parallel")
  if(strat %in% strat.choice == FALSE)
    stop(writeLines("Invalid strategy parameter"))

  if(strat == "sequential") {
    pfit <- sapply(ths, paraf, treeH, treeS, ei.correct = ei.correct)
    return(pfit)
    } else {
      ths1 <- split(ths, rep(1:cl, each = ceiling(length(ths)/cl)))
      pf <- c()
      cores <- parallelly::makeClusterPSOCK(workers = cl)
      for(i in 1:length(ths1)) {
        pf <- c(pf, parSapply(cores, ths1[[i]], paraf, treeH, treeS,
                              ei.correct = ei.correct))
      }
      parallel::stopCluster(cores)
      return(pf)
  }
}
