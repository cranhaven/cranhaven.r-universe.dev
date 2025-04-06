#' Initial loading of spectra
#'
#' This function loads and initially normalizes the raw spectra. Output is a list with the raw and initially corrected spectra.
#' @param raw.spec List of files already loaded with read_raw_spec()
#' @param file Vector with file names
#' @param corr.norm Initial correction and normalization parameters
#' @param use.eshift Set TRUE, if using energy shift value, defaults to NULL
#' @keywords normalization, correction
#' @export
#' @examples
#' data(stdmix)
#' corr.spec.standards  <- initial_load(specdat[1:4], 
#'   corr.norm = c(-36, -15, 37, 58))
#' corr.spec.samples    <- initial_load(specdat[5:8], 
#'   corr.norm = c(-36, -15, 37, 58))


initial_load <- function (raw.spec = NULL, file = NULL, corr.norm, use.eshift = NULL) {
  
  if (is.null(raw.spec)) {
    
    if (is.null(file)) {
      
      stop("Please provide either a raw spectrum from the read_raw_spec function or a raw .xmu file")
      
    } else {
      
      ## read the raw spectra from a file list
      raw.specs <- read_raw_spec(file = file, use.eshift = use.eshift)
      
    }
    
  } else {
    
    raw.specs <- raw.spec
    
  }
  
  
  ## loop to process all samples
  for (i in 1:length(raw.specs)) {
    
    ## background correct the sample with the given parameters
    flat.spec <- bkg_corr(raw.spec = raw.specs[[i]], corr.norm = corr.norm)
    
    ## add the corrected spectrum to the spectra list
    raw.specs[[i]]$data$corr.spec <- flat.spec
    
  }
  
  ## return the corrected spectra list
  return(raw.specs)
  
}