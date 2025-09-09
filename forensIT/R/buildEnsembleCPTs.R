#' @title buildEnsembleCPTs
#' @description Build ensemble of CPTs from a list of simulations
#' @param lsimu list of simulations
#' @param lminimalProbGenoMOI list of minimal probabilities of genotypes given MOI # nolint
#' @return list of CPTs
#' @export
#' @examples
#' library(forrel)
#' library(mispitools)
#' freqs <- lapply(getfreqs(Argentina)[1:15], function(x) {x[x!=0]})
#' fam  <- linearPed(2)
#' fam  <- addChildren(fam, father =  1, mother =  2)
#' fam  <- pedtools::setMarkers(fam, locusAttributes = freqs)
#' ped  <- profileSim(fam, N = 1, ids = c(6)  , numCores = 1,seed=123)
#' lsimEnsemble  <- simTestIDMarkers(ped,2,numSim=5,seed=123)
#' lensembleIT   <- buildEnsembleITValues(lsimu=lsimEnsemble,ITtab=simME$ITtable,bFullIT = TRUE)
#' lensembleCPTs <- buildEnsembleCPTs(lsimu=lsimEnsemble,lminimalProbGenoMOI=simME$lprobGenoMOI)
buildEnsembleCPTs <- function(lsimu, lminimalProbGenoMOI){ # nolint
  aa <- simplify2array(lsimu, higher = FALSE)
  aa <- data.frame(sample = 1:nrow(lsimu[[1]]),id=rep(colnames(lsimu[[1]]), # nolint
   each = nrow(lsimu[[1]])), aa)

  lz        <- vector("list", length(unique(aa$id)))
  names(lz) <- unique(aa$id)
  for (i in seq_along(lz)) lz[[i]] <- list()
  for (irow in 1:nrow(aa)){ # nolint
    bb <- list()
    for (j in 3:ncol(aa)){
      marker <- colnames(aa)[j]
      geno   <- aa[irow, j]
      if (is.null(lminimalProbGenoMOI[[aa[irow, "id"]]][[marker]][[geno]])){cat(paste(irow), "\n")} # nolint
      bb     <- c(bb,
      list(lminimalProbGenoMOI[[aa[irow, "id"]]][[marker]][[geno]]))
    }
    names(bb)  <- colnames(aa)[-c(1, 2)]
    bb2        <- list(bb)
    names(bb2) <- paste0("sample_", aa[irow, "sample"])
    lz[[aa[irow, "id"]]] <- c(lz[[aa[irow, "id"]]], bb2)
  }
  return(lz)
}
