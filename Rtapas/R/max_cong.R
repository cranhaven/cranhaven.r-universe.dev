#' Algortihm for maximizing congruence between two phylogenies
#'
#' Prunes the host (H) and symbiont (S) phylogenies to conform with trimmed
#' matrices and computes the given global fit method, Geodesic distances (GD),
#' Procrustes Approach to Cophylogeny (PACo) or ParaFit (Legendre et al. 2002)
#' between the pruned trees. Then, determines the frequency or corrected
#' residual of each host-symbiont association occurring in a given percentile
#' of cases that maximize phylogenetic congruence.
#'
#' @param HS Host-Symbiont association matrix.
#'
#' @param treeH Host phyolgeny. An object of class "phylo".
#'
#' @param treeS Symbiont phylogeny. An object of class "phylo".
#'
#' @param n Number of unique associations.
#'
#' @param N Number of runs.
#'
#' @param method Specifies the desired global-fit method (GD, PACo or ParaFit).
#'        The default is \code{PACo}. Options are \code{"geoD"} (Geodesic
#'        Distances), \code{"paco"} (PACo) or \code{"paraF"} (ParaFit).
#'
#' @param symmetric Specifies the type of Procrustes superimposition. Default
#'        is \code{FALSE}, indicates that the superposition is applied
#'        asymmetrically (S depends on H). If \code{TRUE}, PACo is applied
#'        symmetrically (dependency between S and H is reciprocal).
#'
#' @param ei.correct Specifies how to correct potential negative eigenvalues
#'        from the conversion of phylogenetic distances into Principal
#'        Coordinates: \code{"none"} (the default) indicates that no correction
#'        is applied, particularly if H and S are ultrametric; \code{"sqrt.D"}
#'        takes the element-wise square-root of the phylogenetic distances;
#'        \code{"lingoes"} and \code{"cailliez"} apply the classical Lingoes
#'        and Cailliez corrections, respectively.
#'
#' @param percentile Percentile to evaluate (\emph{p}). Default is
#'        \code{0.01} (1\%).
#'
#' @param res.fq Determines whether a correction to avoid one-to-one
#'        associations being overrepresented in the percentile evaluated.
#'        If \code{TRUE} (default) a residual frequency value (observed -
#'        expected frequency) is computed for each host-symbiont association.
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
#'
#' @return A dataframe with host-symbiont associations in rows. The first and
#'         second columns display the names of the host and symbiont terminals,
#'         respectively. The third column designates the host-symbiont
#'         association by pasting the names of the terminals, and the fourth
#'         column displays the frequency of occurrence of each host-symbiont
#'         association in \emph{p}. If \code{res.fq = TRUE}, column 5 displays
#'         the corrected frequencies as a residual.
#'
#' @section NOTE:
#'       If the \code{node.label} object in both trees contains NAs or empty
#'       values (i.e. no numeric value). All nodes should have a value. Else
#'       remove node labels within the \code{"phylo"} class tree
#'       with \code{tree$node.label <- NULL}. For more details, see
#'       \code{\link[distory:dist.multiPhylo]{distory::dist.multiPhylo()}}
#'
#' @export
#'
#'
#' @examples
#' data(nuc_pc)
#' N = 10 #for the example, we recommend 1e+4 value
#' n = 15
#' NPc <- max_cong(np_matrix, NUCtr, CPtr, n, N, method = "paco",
#'                 symmetric = FALSE, ei.correct = "sqrt.D",
#'                 percentile = 0.01, res.fq = FALSE,
#'                 strat = "parallel", cl = 10)
#'
#'
max_cong <- function (HS, treeH, treeS, n, N, method = "paco",
                      symmetric = FALSE, ei.correct = "none", percentile = 0.01,
                      res.fq = TRUE, strat = "sequential", cl = 1){

  THS <- trimHS_maxC(N = N, n = n, HS = HS, check.unique = TRUE)
  method.choice <- c("geoD", "paco", "paraF")
  if (method %in% method.choice == FALSE)
    stop(writeLines("Invalid global-fit method.
                    Correct choices are 'geoD', 'paco' or 'parafit'"))
  if (method == "geoD") {
    GD <- geo_D(THS, treeH, treeS, strat = strat, cl = cl)

    LF <- link_freq(THS, GD, HS, percentile = percentile, below.p = TRUE,
                    res.fq = res.fq)
    return(LF)
  }
  if (method == "paco") {
    PACO <- paco_ss(THS, treeH, treeS, symmetric = symmetric,
                    proc.warns = FALSE, ei.correct = ei.correct,
                    strat = strat, cl = cl)
    LF <- link_freq(THS, PACO, HS, percentile = percentile, below.p = TRUE,
                    res.fq = res.fq)
    return(LF)
  }
  if (method == "paraF") {
    PF <- paraF(THS, treeH, treeS, ei.correct = ei.correct,
                strat = strat, cl = cl)
    LF <- link_freq(THS, PF, HS, percentile = percentile, below.p = TRUE,
                    res.fq = res.fq)
    return(LF)
  }
}
