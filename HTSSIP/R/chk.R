#' Checking format of phyloseq object for HTSSIP compatibility
#'
#' @param physeq  Phyloseq object
#' @return phyloseq object
#'
#' @export
#'
#' @examples
#' # this data should be formatted for HTSSIP
#' data(physeq_S2D2)
#' physeq_format(physeq_S2D2)
#'
#' # this data should NOT be correctly formatted for HTSSIP
#' \dontrun{
#' library(phyloseq)
#' data(GlobalPatterns)
#' tryCatch(
#'  physeq_format(GlobalPatterns),
#'  function(e) e
#'  )
#'  }
#'
physeq_format = function(physeq){
  meta = phyloseq::sample_data(physeq)
  meta_coln = colnames(meta)
  is.BD = grepl('Buoyant.density', meta_coln, ignore.case=TRUE)
  if(all(is.BD==FALSE)==TRUE){
    stop('No "Buoyant_density" column found in metadata')
  } else {
    i = which(is.BD==TRUE)
    x = meta_coln[i[1]]
    meta[,'Buoyant_density'] = meta[,x]
    phyloseq::sample_data(physeq) = meta
  }
  return(physeq)
}

