#' Apply normalization factors to spectra
#'
#' @param spec         List of MALDIquant::MassSpectrum or MALDIquant::MassPeaks
#' @param factors      Numeric vector of normalization factors. See getNormFactors().
#'
#' @return             List of normalized Spectra or Peaks
#'
#' @importFrom MALDIquant intensity intensity<-
#' @importFrom purrr map
#' @export
#' @examples
#' #' data(Blank2022peaks)
#' normFactors <-  getNormFactors(peaks2df(Blank2022peaks), 
#'                                targetMz = 760.585, 
#'                                tol = 0.1, 
#'                                tolppm = FALSE)
#' normPeaks <- normalizeByFactor(Blank2022peaks, 
#'                                normFactors$norm_factor)
normalizeByFactor <- function(spec, factors) {
  if (!length(spec) == length(factors)) {
    stop("Number of spectra and normalization factors not equal!\n")
  }

  spec_res <- purrr::map(seq_along(spec), function(i) {
    intensity(spec[[i]]) <- intensity(spec[[i]])/factors[i]
    return(spec[[i]])
  })
  names(spec_res) <- names(spec)

  return(spec_res)
}
