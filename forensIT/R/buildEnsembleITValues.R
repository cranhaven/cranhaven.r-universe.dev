#' @title buildEnsembleITValues
#' @description Build ensemble of IT values from a list of simulations
#' @param lsimu list of simulations
#' @param ITtab IT table
#' @param bFullIT boolean to return full IT table
#' @return list of IT values
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

buildEnsembleITValues <-function(lsimu = lsimulation,ITtab = sim$ITtable,bFullIT = FALSE){ # nolint
  sim <- lsimulation <- NULL
  fullIT <- ensembleIT <- c() # nolint
  for(icdi in 1:ncol(lsimu[[1]])){ # nolint
    newp <- colnames(lsimu[[1]])[icdi]
    for (imrkr in seq_along(lsimu)){
      tt  <- ITtab[ITtab$marker == names(lsimu)[imrkr], ]
      itt <- match(lsimu[[imrkr]][, icdi], tt[tt$cdi == newp, "allele"])
#     n       <- nrow(lsimu[[imrkr]]) # nolint
      aa <- data.frame(sample = seq_along(itt), id = newp,
      marker = names(lsimu)[imrkr], tt[tt$cdi == newp, ][itt, 4:ncol(tt)])
      fullIT <- rbind(fullIT, aa) # nolint
    }
    aa <- aggregate(fullIT[, 4:ncol(fullIT)],
                    by = list(sample = fullIT$sample, id = fullIT$id),
                    function(x) {
                      sum(x)})
    ep <- aggregate(fullIT[, "pexclusion"],
                    by = list(sample = fullIT$sample, id = fullIT$id),
                    function(x) {
                      1 - prod(1 - (x))})[, 3]
    aa <- cbind(aa[, c(1:8, 11:12)], exclusionPower = ep)
    colnames(aa)[9]  <- "numX"
    aa[, 10]  <- aa[, 10] / length(lsimu)
    ensembleIT  <- rbind(ensembleIT, aa)  # nolint
  }
  if (bFullIT) {
    return(list(ensembleIT = ensembleIT, fullIT = fullIT))
  }else {
    return(list(ensembleIT = ensembleIT))
  }
}
