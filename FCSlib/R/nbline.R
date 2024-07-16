#' @title Number & Brightness (Single Image)
#' 
#' @description Performs the Number and Brightness Analysis (N&B) on an image
#' @aliases nbline
#' @usage nbline(img, sigma0 = 0, offset = 0, S = 1, w = 0)
#' @param img The image to analyze.
#' @param sigma0 Variance of the optical system readout noise
#' @param offset Constant number that depends on the optical system configuration. Signal values smaller that the offset should be considered zero.
#' @param S Proportionality factor S. Indicates the ratio between the amount inicident photons in the detector and those converted to an electronic signal.
#' @param w Time window at which the running average is calculated
#' @details
#' The Number and Brightness (N&B) method is a time-independent technique that provides an estimate of molecular concentration and aggregation state (or stoichiometry), based on the statistical moments of the fluorescence intensity fluctuations. In other words, this tool allows to distinguish between two or more homo-oligomeric states of a molecule present in a given region in the sample (Brightness) while also providing a direct indicator of the molecules relative abundance (Number).
#' The intensity of the fluorescence signal is mostly due to the mere presence of fluorophores in the media, affected by the fluorophore quantum yield, the sensitivity of the detector and the photophysical characteristics of the optical instrumentation.
#' The average particle number and brightness are calculated directly from the mean value <k> and variance (sigma^2) of the fluorescence intensity data (image) for a given pixel as follows: 
#' N = (<k>^2)/(sigma^2)
#' and
#' B = (sigma^2)/<k>
#' 
#' @export
#' @importFrom stats var
#' @importFrom graphics axis mtext par
#' @return A list containing two vectors, the Brightness and the Number of the image.
#' @author Raúl Pinto Cámara.
#' 
#' @seealso \code{\link{var}, \link{mean}}
#' @examples
#' 
#' \donttest{
#' ### Load the FCSlib package
#' 
#' library(FCSlib)
#' 
#' # As an example, we will use a data set that corresponds
#' # to a population of Venus dimers and hexamers diffusing in HEK-293 cells.
#' # Use the readFileTiff() function to extract the information from the '.tiff' files.
#' 
#' nbv2 <- nbline(V2)
#' pixelSize = 0.05
#' r<- (1:dim(V2)[1])*pixelSize
#' }


nbline <- function(img, sigma0 = 0, offset = 0, S = 1, w = 0){
  w <- floor(w)
  di <- dim(img)
  if (length(di) != 2) {
    stop("'img' must be two dimensional")
  }
  mn<-vr<-NULL
  if(w == 0){
    mn <- apply(img, MARGIN = 1, mean, na.rm = T)
    vr <- apply(img, MARGIN = 1, var, na.rm = T)
  } else{
    for (r in (w + 1):(di[2] - w)){
      mn <- cbind(mn,apply(img[,(r-w):(r+w)], MARGIN = 1, mean, na.rm = T))
      vr <- cbind(vr, apply(img[,(r-w):(r+w)], MARGIN = 1, var, na.rm = T))
    }
  }
  if(S == 1){
    B <- vr/mn
    N <- (mn^2)/vr
    return(list(AppNumber = N, AppBrightness = B))
  } else{
    epsilon <- (vr -sigma0 -S*(mn - offset))/(S*(mn - offset))
    n <-  ((mn - offset)^2)/(vr -sigma0 -S*(mn - offset))
    return(list(RealNumber = n, RealBrightness = epsilon))
  }
}