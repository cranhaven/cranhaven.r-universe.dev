#' Repair problematic meta data entries of spectra
#'
#' @param spec list of MALDIquant::MassSpectrum
#'
#' @return
#' list of MALDIquant::MassSpectrum with NA entries in meta data substituted with empty strings (unproblematic).
#' 
#' @importFrom MALDIquant metaData<-
#' @noRd
.repairMetaData <- function(spec) {
  count <- 0
  for(i in 1:length(spec)) {
    m <- metaData(spec[[i]])

    m_clean <- m

    for(j in 1:length(m)) {
      if(any(is.na(m[[j]]))) {
        count <- count + 1
        m_clean[[j]] <- ""
      }
    }

    metaData(spec[[i]]) <- m_clean
  }

  if(count > 0) {
    warning(count, " spectra had NA meta data entries.\n",
            "All NA entries substituted by empty strings.\n")
  }
  return(spec)
}
