#' Procrustes Approach to Cophylogeny (PACo) of the host and symbiont
#' configurations
#'
#' For any trimmed matrix produced with
#' \code{\link[=trimHS_maxC]{trimHS_maxC()}} or
#' \code{\link[=trimHS_maxI]{trimHS_maxI()}}, it prunes the host (H) and
#' symbiont (S) phylogenies to conform with the trimmed matrix and runs
#' Procruste Approach to Cophylogeny (PACo) to produce the squared sum of
#' residuals of the Procrustes superimposition of the host and symbiont
#' configurations in Euclidean space.
#'
#' @param ths Trimmed matrix.
#'
#' @param treeH Host phylogeny. An object of class \code{"phylo"}.
#'
#' @param treeS Symbiont phylogeny. An object of class \code{"phylo"}.
#'
#' @param symmetric Specifies the type of Procrustes superimposition. Default
#'        is \code{FALSE}, indicates that the superposition is applied
#'        asymmetrically (S depends on H). If \code{TRUE}, PACo is applied
#'        symmetrically (dependency between S and H is reciprocal).
#'
#' @param proc.warns Switches on/off trivial warnings returned when treeH and
#'        treeS differ in size (number of tips). Default is \code{FALSE}.
#'
#' @param ei.correct Specifies how to correct potential negative eigenvalues
#'        from the conversion of phylogenetic distances into Principal
#'        Coordinates: \code{"none"} (the default) indicates that no correction
#'        is applied, particularly if H and S are ultrametric; \code{"sqrt.D"}
#'        takes the element-wise square-root of the phylogenetic distances;
#'        \code{"lingoes"} and \code{"cailliez"} apply the classical Lingoes
#'        and Cailliez corrections, respectively.
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
#' @return A sum of squared residuals.
#'
#' @examples
#' data(amph_trem)
#' N = 10 #for the example, we recommend 1e+4 value
#' n = 8
#'
#' TAM <- trimHS_maxC(N, am_matrix, n, check.unique = TRUE)
#' PACO <- paco_ss(TAM, amphipod, trematode, symmetric = TRUE,
#'                 ei.correct = "sqrt.D", strat = "parallel", cl = 8)
#'
#' @source
#' Balbuena J.A., Perez-Escobar O.A., Llopis-Belenguer C., Blasco-Costa I.
#' (2022). User’s Guide Random Tanglegram Partitions V.1.0.0. Zenodo.
#'
#' @references
#' Balbuena J.A., Miguez-Lozano R., Blasco-Costa I. (2013). PACo: A Novel
#' Procrustes Application to Cophylogenetic Analysis. PLOS ONE. 8:e61048.
#'
#' Balbuena J.A., Perez-Escobar Ó.A., Llopis-Belenguer C., Blasco-Costa I.
#' (2020). Random Tanglegram Partitions (Random TaPas): An Alexandrian Approach
#' to the Cophylogenetic Gordian Knot. Systematic Biology. 69:1212–1230.
#'
#' @import ape
#' @import parallel
#' @importFrom vegan procrustes
#' @importFrom parallelly makeClusterPSOCK
#'
#' @export
#'
paco_ss <- function (ths, treeH, treeS, symmetric = FALSE,
                     proc.warns = FALSE, ei.correct = "none",
                     strat = "sequential", cl = 1) {

  pacoss <- function (ths, treeH, treeS, ...) {
  eigen.choice <- c("none", "lingoes", "cailliez", "sqrt.D")
  if (ei.correct %in% eigen.choice == FALSE)
    stop(writeLines("Invalid eigenvalue correction parameter.\r
               Correct choices are 'none', 'lingoes', 'cailliez' or 'sqrt.D'"))
  treeh <- ape::drop.tip(treeH, setdiff(treeH$tip.label, rownames(ths)))
  trees <- ape::drop.tip(treeS, setdiff(treeS$tip.label, colnames(ths)))
  # Reorder ths as per tree labels:
  ths <- ths[treeh$tip.label, trees$tip.label]
  DH <- ape::cophenetic.phylo(treeh)
  DP <- ape::cophenetic.phylo(trees)
  if(ei.correct == "sqrt.D"){DH <- sqrt(DH); DP <- sqrt(DP); ei.correct ="none"}
  D <- paco::prepare_paco_data(DH, DP, ths)
  D <- paco::add_pcoord(D, correction = ei.correct)
  if (proc.warns == FALSE) D <- vegan::procrustes(D$H_PCo, D$P_PCo,
                                                  symmetric = symmetric)
  else
    D <- suppressWarnings(vegan::procrustes(D$H_PCo, D$P_PCo,
                                            symmetric = symmetric))
  return(D$ss)
  }

  strat.choice <- c("sequential", "parallel")
  if (strat %in% strat.choice == FALSE)
    stop(writeLines("Invalid strategy parameter"))

  if (strat == "sequential") {
    paco <- sapply(ths, pacoss, treeH, treeS, symmetric = symmetric,
                            proc.warns = proc.warns, ei.correct = ei.correct)
    return(paco)
  } else {
    cores <- makeClusterPSOCK(workers = cl)
    paco <- parallel::parSapply(cores, ths, pacoss, treeH, treeS, symmetric = symmetric,
                      proc.warns = proc.warns, ei.correct = ei.correct)
    parallel::stopCluster(cores)
    return(paco)
  }
}

