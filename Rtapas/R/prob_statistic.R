#' Frequencies of the associations for the posterior probability trees
#'
#' Computes frequencies (or residual/corrected frequencies) of the
#' host-symbiont associations for pairs (H and S) of posterior  probability
#' trees from the statistics generatedwith \code{GD} (Geodesic Distances),
#' \code{PACo} (PACo) or \code{ParaFit}(ParaFit).
#'
#' @param ths List of trimmed matrices produced by
#'        \code{\link[=trimHS_maxC]{trimHS_maxC()}} or
#'        \code{\link[=trimHS_maxI]{trimHS_maxI()}}.
#'
#' @param HS Host-Symbiont association matrix.
#'
#' @param mTreeH Number of posterior-probability trees of host.
#'
#' @param mTreeS Number of posterior-probability trees of symbiont.
#'
#' @param freqfun The global-fit method to compute using the
#'        posterior probability trees. Options are \code{"geoD"} (Geodesic
#'        Distances), \code{"paco"} (PACo) or \code{"paraF"} (ParaFit).
#'        It should be the same method used to obtain \code{"fx"}.
#'
#' @param fx Vector of statistics produced with
#'         \code{\link[=max_cong]{max_cong()}} or
#'         \code{\link[=max_incong]{max_incong()}} for GD, PACo or
#'         ParaFit.
#'
#' @param percentile Percentile to evaluate (\emph{p}). Default is
#'        \code{0.01} (1\%).
#'
#' @param correction Correction to be assumed. The default value is
#'        \code{"none"}. If \code{= "res.fq"}, a residual frequency value
#'        (observed - expected frequency) is computed for each host-symbiont
#'        association that maximizes phylogenetic congruence. If
#'        \code{= "diff.fq"}, a corrected frequency value
#'        (observed in p - observed in (p-1)) is computed for each
#'        host-symbiont association.
#'        It should be the same correction used to obtain \code{"fx"}.
#'
#' @param algm Only required if \code{correction = "none"}. Specifies the
#'        algorithm that has been used to obtain
#'        \code{"fx"} without correction. Use \code{= "maxcong"} for
#'        \code{\link[=max_cong]{max_cong()}}, and \code{= "maxincong"} for
#'        \code{\link[=max_incong]{max_incong()}}.
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
#' @param proc.warns Switches on/off trivial warnings returned when treeH and
#'        treeS differ in size (number of tips). Default is \code{FALSE}.
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
#' @return A matrix with the value of the statistics for each of the
#'         probability trees.
#' @export
#'
#' @examples
#' data(nuc_pc)
#' N = 10 #for the example, we recommend 1e+4 value
#' n = 15
#' # Maximizing congruence (not run)
#' # NPc <- max_cong(np_matrix, NUCtr, CPtr, n, N, method = "paco",
#' #                 symmetric = FALSE, ei.correct = "sqrt.D",
#' #                 percentile = 0.01, correction = "none",
#' #                 strat = "parallel", cl = 8)
#' # THSc <- trimHS_maxC(N, np_matrix, n)
#' # pp_treesPACOo_cong <- prob_statistic(THSc, np_matrix, NUC_500tr[1:10],
#' #                          CP_500tr[1:10], freqfun = "paco", NPc,
#' #                          percentile = 0.01, correction = "none",
#' #                          algm = "maxcong", symmetric = FALSE,
#' #                          ei.correct = "sqrt.D",
#' #                          strat = "parallel", cl = 8)
#'
#' # Maximizing incongruence
#' NPi <- max_incong(np_matrix, NUCtr, CPtr, n, N, method = "paco",
#'                   symmetric = FALSE, ei.correct = "sqrt.D",
#'                   percentile = 0.99, diff.fq = TRUE,
#'                   strat = "parallel", cl = 8)
#' THSi <- trimHS_maxI(N, np_matrix, n)
#' pp_treesPACOo_incong <- prob_statistic(THSi, np_matrix, NUC_500tr[1:5],
#'                         CP_500tr[1:5], freqfun = "paco", NPi,
#'                         percentile = 0.99, correction = "diff.fq",
#'                         symmetric = FALSE, ei.correct = "sqrt.D",
#'                         strat = "parallel", cl = 8)
#'
#'
prob_statistic <- function (ths, HS, mTreeH, mTreeS, freqfun = "paco", fx,
                       percentile = 0.01, correction = "none",
                       symmetric = FALSE, ei.correct="none",
                       algm = "maxcong", proc.warns = FALSE,
                       strat = "sequential", cl = 1) {

  strat.choice <- c("sequential", "parallel")
  if (strat %in% strat.choice == FALSE)
    stop(writeLines("Invalid strategy parameter"))

  freqfun.choice <- c("geoD", "paco", "paraF")
  if(freqfun %in% freqfun.choice == FALSE)
    stop(writeLines("Invalid freqfun parameter.
    Correct choices are 'geoD','paco' or 'paraF'"))

  corr.choice <- c("none", "res.fq", "diff.fq")
  if(correction %in% corr.choice == FALSE)
    stop(writeLines("Invalid correction parameter.
    Correct choices are 'none', 'res.fq' or 'diff.fq'"))

  if(correction == "none"){
    res.fq = FALSE
    diff.fq = FALSE

    algm.choice <- c("maxcong", "maxincong")
    if(algm %in% algm.choice == FALSE)
      stop(writeLines("Invalid algorithm parameter.
      Correct choices are 'maxcong' or 'maxincong'"))

    if (algm == "maxcong"){
      below.p = TRUE
    } else {below.p = FALSE}
  } else {
    if(correction == "res.fq"){
      res.fq = TRUE
      below.p = TRUE
      diff.fq = FALSE
    } else {
      res.fq = FALSE
      below.p = FALSE
      diff.fq = TRUE
    }
  }

  if(freqfun == "geoD") {
    gd_apply <- function(ths, mTreeH, mTreeS, HS) {
      GD.CI <- geo_D(ths, treeH = mTreeH, treeS = mTreeS)
      LFGD01.CI <- link_freq(ths, GD.CI, HS, percentile = percentile,
                             res.fq = res.fq, below.p = below.p)
      return(LFGD01.CI[, ncol(LFGD01.CI)])
    }
    if(strat == "sequential") {
    GD01 <- t(sapply(1:length(mTreeH), function(x) gd_apply(ths, mTreeH[[x]], mTreeS[[x]], HS)))
    colnames(GD01) <- fx[,3]
    return(GD01)
    } else {
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
      cores <- parallelly::makeClusterPSOCK(workers = cl)
      GD01 <- matrix(NA, length(mTreeH), nrow(fx))
      for(i in 1:length(mTreeH)) {
        GD.CI <- parallel::parSapply(cores, ths, geoD, treeH = mTreeH[[i]],
                           treeS = mTreeS[[i]])
          LFGD.CI <- link_freq(ths, GD.CI, HS,  percentile = percentile,
                               res.fq = res.fq, below.p = below.p)
          GD01[i,] <- LFGD.CI[, ncol(LFGD.CI)]
      }
      parallel::stopCluster(cores)
      colnames(GD01) <- fx[,3]
    }
    return(GD01)
    } else {
      if(freqfun == "paco") {
        if(strat == "sequential") {
          paco_apply <- function(ths, mTreeH, mTreeS, HS) {
            PA.CI <- paco_ss(ths, treeH=mTreeH, treeS=mTreeS,
                             symmetric = symmetric, ei.correct = ei.correct,
                             strat = strat , cl = cl)
            LFPA01.CI <- link_freq(ths, PA.CI, HS, percentile = percentile,
                                   res.fq = res.fq, below.p = below.p)
            return(LFPA01.CI[, ncol(LFPA01.CI)])
          }
          PACO01 <- t(sapply(1:length(mTreeH), function(x) paco_apply(ths, mTreeH[[x]], mTreeS[[x]], HS)))
          colnames(PACO01) <- fx[,3]
          return(PACO01)
          } else {
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
            cores <- parallelly::makeClusterPSOCK(workers = cl)
            PACO01 <- matrix(NA, length(mTreeH), nrow(fx))
            for(i in 1:length(mTreeH)) {
              PA.CI <- parallel::parSapply(cores, ths, pacoss, treeH=mTreeH[[i]],
                                   treeS= mTreeS[[i]], symmetric = symmetric,
                                   proc.warns = proc.warns, ei.correct = ei.correct)
              if (diff.fq == TRUE) {
                LFPA02.CI <- link_freq(ths, PA.CI, HS,  percentile = 0.01,
                                       res.fq = FALSE, below.p = TRUE)
                LFPA03.CI <- link_freq(ths, PA.CI, HS,  percentile = 0.99,
                                       res.fq = FALSE, below.p = FALSE)
                LFr <- LFPA02.CI[, ncol(LFPA02.CI)] - LFPA03.CI[, ncol(LFPA03.CI)]
                LFPA.CI <- cbind(LFPA02.CI[, 1:ncol(LFPA02.CI)], LFr)
              } else {
                LFPA.CI <- link_freq(ths, PA.CI, HS,  percentile = percentile,
                                       res.fq = res.fq, below.p = below.p)
              }
              PACO01[i,] <- LFPA.CI[, ncol(LFPA.CI)]
            }
            parallel::stopCluster(cores)
            colnames(PACO01) <- fx[,3]
          }
        return(PACO01)
        } else {
        pf_apply <- function(ths, mTreeH, mTreeS, HS) {
          PF.CI <- paraF(ths, treeH = mTreeH, treeS = mTreeS,
                           ei.correct = ei.correct,
                           strat = strat , cl = cl)
          LFPF01.CI <- link_freq(ths, PF.CI, HS, percentile = percentile,
                                 res.fq = res.fq, below.p = below.p)
          return(LFPF01.CI[, ncol(LFPF01.CI)])
        }
        if(strat == "sequential") {
          PF01 <- t(sapply(1:length(mTreeH), function(x) pf_apply(ths, mTreeH[[x]], mTreeS[[x]], HS)))
          colnames(PF01) <- fx[,3]
          return(PF01)
        } else {
          paraf <- function (ths, treeH, treeS, ...) {
            eigen.choice <- c("none", "lingoes", "cailliez", "sqrt.D")
            if (ei.correct %in% eigen.choice == FALSE)
              stop(writeLines("Invalid eigenvalue correction parameter.\r
               Correct choices are 'none', 'lingoes', 'cailliez' or 'sqrt.D'"))
            treeh <- ape::drop.tip(treeH, setdiff(treeH$tip.label, rownames(ths)))
            trees <- ape::drop.tip(treeS, setdiff(treeS$tip.label, colnames(ths)))
            if (is.null(treeh)==TRUE | is.null(trees)==TRUE) PF <- NA else {
              # Reorder ths as per tree labels:
              ths <- ths[treeh$tip.label, trees$tip.label]
              DH <- ape::cophenetic.phylo(treeh)
              DP <- ape::cophenetic.phylo(trees)
              if (ei.correct == "sqrt.D"){DH <- sqrt(DH); DP <- sqrt(DP); ei.correct ="none"}
              PF <- ape::parafit(DH, DP, ths, nperm=1, silent=TRUE, correction = ei.correct)
              PF <- PF$ParaFitGlobal
            }
            return(PF)
          }
          cores <- parallelly::makeClusterPSOCK(workers = cl)
          PF01 <- matrix(NA, length(mTreeH), nrow(fx))
          for(i in 1:length(mTreeH)) {
            PF.CI <- parallel::parSapply(cores, ths, paraf, treeH = mTreeH[[i]],
                               treeS = mTreeS[[i]], proc.warns = proc.warns,
                               ei.correct = ei.correct)
            if (diff.fq == TRUE) {
              LFPF02.CI <- link_freq(ths, PF.CI, HS,  percentile = 0.01,
                                     res.fq = FALSE, below.p = TRUE)
              LFPF03.CI <- link_freq(ths, PF.CI, HS,  percentile = 0.99,
                                     res.fq = FALSE, below.p = FALSE)
              LFr <- LFPF02.CI[, ncol(LFPF02.CI)] - LFPF03.CI[, ncol(LFPF03.CI)]
              LFPF.CI <- cbind(LFPF02.CI[, 1:ncol(LFPF02.CI)], LFr)
            } else {
              LFPF.CI <- link_freq(ths, PF.CI, HS,  percentile = percentile,
                                   res.fq = res.fq, below.p = below.p)
            }
            PF01[i,] <- LFPF.CI[, ncol(LFPF.CI)]
          }
          parallel::stopCluster(cores)
          colnames(PF01) <- fx[,3]
        }
        return(PF01)
      }
    }
}

