#' @title Fluorescence Correlation Spectroscopy
#' 
#' @description Calculates either the auto-correlation or cross-correlation between vectors x and y, returning a correlation function.
#' @aliases fcs
#' @usage fcs(x , y = NULL, nPoints, pcf = FALSE)
#' @param x Numeric vector of length N.
#' @param y Numeric vector of length N.
#' @param nPoints The size of the sub-vectors in which the input vectors will be divided. This number must be less than N/2.
#' @param pcf A boolean parameter to determine if an alternate version of the correlation function is used for the calculation of de pCF and pComb functions.
#' @details Fluorescence correlation spectroscopy (FCS) is a technique with high spatial and temporal resolution used to analyze the kinetics of particles diffusing at low concentrations. The detected fluorescence intensity as a function of time is: F(t).
#' 
#' The correlation function is computed as the normalized autocorrelation function, G(tau) = <deltaF(t)deltaF(t+tau)>/(<F(t)>*<F(t)>), to the collected data set, where t refers to a time point of flourescence acquisition, and tau refers to the temporal delay between acquisitions and <...> indicates average. 
#' 
#' The correlation between deltaF(t) = F(t) - <F(t)> and deltaF(t+tau) = F(t+tau) - <F(t)> is calculated for a range of delay times.
#' For temporal acquisitions as FCS point, x takes the value of F(t) and y = NULL.
#' For cross-correlation experiments between two fluorescent signals x = F1(t) and y = F2(t), as channels, the correlation function is: G(tau) = <deltaF1(t) deltaF2(t+tau)> / (<F1(t)> <F2(t)>).
#' 
#' The function separate the original vector in sub-vectors of same length (n-points), then calculate an autocorrelation function form each sub-vector. The final result will be an average of all the autocorrelation functions.
#' @note The argument nPoints must be smaller than the total number of temporal observations N, it is recommended to set nPoints = 2^n, with n = {2, ..., infinity}.
#' @export
#' @return A numeric vector G containing either the autocorrelation for the input vector x, or the cross-correlation between x and y vectors, with a length of nPoints.
#' @references R.A. Migueles-Ramirez, A.G. Velasco-Felix, R. Pinto-Cámara, C.D. Wood, A. Guerrero. Fluorescence fluctuation spectroscopy in living cells.
#' Microscopy and imaging science: practical approaches to applied research and education, 138-151,2017.
#' @author Raúl Pinto Cámara, Adan O. Guerrero
#' 
#' @seealso \code{\link{gcf}}
#' 
#' @examples
#' \donttest{
#' ### Load the FCSlib package
#' 
#' library(FCSlib)
#' 
#' # As an example, we will use data from experiment adquisition
#' # of free Cy5 molecules diffusing in water at a concentration of 100 nM.
#' # Use readFileTiff() function to read the fcs data in TIFF format.
#' 
#' f<-readFileTiff("Cy5_100nM.tif")
#' 
#' ### Note that $f$ is a matrix of 2048 x 5000 x 1 dimentions.
#' # This is due to the fact that this single-point FCS experimen twas collected
#' # at intervals of 2048 points each, with an acquisition time of 2 mu s.
#' # Let's now create a dataframe with the FCS data wich here-and-after will be called Cy5.
#' 
#' acqTime = 2E-6
#' f<-as.vector(f)
#' time <- (1:length(f))*acqTime
#' Cy5<-data.frame(t = time, f)
#' 
#' ### The first 100 ms of the time series are:
#' 
#' plot(Cy5[1:5000,], type ="l", xlab = "t(s)", ylab ="Fluorescence Intensity", main = "Cy5")
#' 
#' # The fcs() function receives three parameters: 'x' (mandatory),
#' # 'y'(optional) and 'nPoints' (optional), where x is the main signal to analyze,
#' #  y is a secondary signal (for the case of cross-correlation instead of autocorrelation)
#' # and nPoints is the final length of the calculated correlation curve.
#' # This function divides the original N-size signal into sub-vectors with a size of nPoints*2.
#' # Once all the sub-vectors are analyzed, these are then averaged.
#' # To use the fcs() function type
#' 
#' g <- fcs(x = Cy5$f, nPoints = length(Cy5$f)/2)
#' 
#' # The result of the function is assigned to the variable 'g',
#' # which contains the autocorrelation curve
#' 
#' length <- 1:length(g)
#' tau <-Cy5$t[length]
#' G<-data.frame(tau,g)
#' plot(G, log = "x", type = "l", xlab = expression(tau(s)), ylab = expression(G(tau)), main = "Cy5")
#' 
#' # It is important to remove the first point from the data,
#' # where G(\tau=0) it is not properly computed
#' 
#' G<-G[-1,]
#' plot(G, log = "x", type = "l", xlab = expression(tau(s)), ylab = expression(G(tau)), main = "Cy5")
#' 
#' # The variable 'nPoints' can be adjusted to better assess the transport phenomena
#' # in study (i.e. free diffusion in three dimensions in the case of this example) and
#' # for better understanding of the diffusive nature of the molecules.
#' # In this example 'nPoints' will be set to 2048.
#' 
#' g <- fcs(x = Cy5$f,nPoints = 2048)
#' length <- 1:length(g)
#' tau <-Cy5$t[length]
#' G<-data.frame(tau,g)
#' G<-G[-1,]
#' plot(G, log = "x", type = "l", xlab = expression(tau(s)), ylab = expression(G(tau)), main = "Cy5")
#' }

fcs <- function(x , y = NULL, nPoints, pcf = FALSE){
  if(is.null(y)){
    y = x
  }
  if(!is.numeric(nPoints)){
    stop("nPoints must be a numeric type")
  }
  nPointsint = nPoints*2
  if(!(is.vector(x)&&is.vector(y))){
    stop("x and y must be vectors")
  }
  if(!(length(x)==length(y))){
    stop("x and y must be vectors of the same length")
  }
  if(nPointsint > length(x)){
    stop("'nPoints' must be less than the length of x/2")
  }
  nVals <- length(x)
  nSect <- trunc(nVals / nPointsint)
  rest <- nVals%%nPointsint
  if(rest != 0){
    x <- x[-((nVals-rest+1):nVals)]; y <- y[-((nVals-rest+1):nVals)]
  }
  G <- array(data = 0, dim = c(nPointsint, nSect))
  for(i in 1:nSect){
    leftLim <- nPointsint * (i - 1) + 1
    rightLim <- nPointsint * i
    xmean <- mean(x[leftLim:rightLim], na.rm = T)
    ymean <- mean(y[leftLim:rightLim], na.rm = T)
    g <- array(data = 0, dim = nPointsint)
    if(xmean != 0 && ymean != 0){
      dx <- x[leftLim:rightLim]
      dy <- y[leftLim:rightLim]
      if(!pcf){
        dx <- dx - xmean
        dy <- dy - ymean
        g <- gcf(x = dx, y = dy, xmean = xmean, ymean = ymean)
      } else{
        g <- gcf(x = dx, y = dy, xmean = xmean, ymean = ymean)
        g <- g - 1
      }
    }
    G[, i] <- g
  }
  G <- apply(G, MARGIN = 1, mean, na.rm = T); G <- G[1:(nPoints+1)]
  return(G[-1])
}
