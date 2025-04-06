#' Create a data frame of all standards
#'
#' The function creates a data frame with all standards in one data frame. The data frame has the
#' same energy values as the sample that is loaded.
#' @param sample A raw sample
#' @param all.standards List of all standards
#' @keywords approximation, correction, standards
#' @importFrom stats approx
#' @export
#' @examples
#' data(stdmix)
#' corr.spec.standards  <- initial_load(specdat[1:4], 
#'   corr.norm = c(-36, -15, 37, 58))
#' corr.spec.samples    <- initial_load(specdat[5:8], 
#'   corr.norm = c(-36, -15, 37, 58))
#' fit.standards <- std_df(sample = corr.spec.samples[[1]], 
#'   all.standards = corr.spec.standards)
#' print(fit.standards)

std_df <- function (sample, all.standards) {
  
  ## create dummy vectors for standard spectra and names
  temp.std <- NULL
  temp.names <- NULL
  
  ## loop of all standards
  for (j in 1:length(all.standards)) {
    
    ## create linear interpolation of a given standard spectrum 
    ## to approximate for different energy ranges during measurement
    temp.interpol <- approx(x = all.standards[[j]]$data$corr.spec$energy,
                            y = all.standards[[j]]$data$corr.spec$cor.absorption,
                            xout = sample$data$corr.spec$energy,
                            method = "linear")
    
    ## fill spectra data and standard names into dummy vectors
    temp.std <- cbind(temp.std, temp.interpol$y)
    temp.names <- c(temp.names, all.standards[[j]]$name)
    
    ## close loop of all standards
  }
  
  ## transform results to data frame and change its column names
  std.df <- as.data.frame(temp.std)
  colnames(std.df) <- temp.names
  
  ## return resulting data frame
  return(std.df)
  
  ## close function
}