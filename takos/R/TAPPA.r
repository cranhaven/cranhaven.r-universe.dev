#' Title  Tangent area proportional method TAPPA
#' @description calculates the background of a thermogram according to Tangent-area-proportional method
#' @param T temperature
#' @param dAlpha the da/dt values
#' @param interval number of points to use for interpolating the two lines that will merge according to the area of the peak
#' @param tol tollerance for the iterative process
#' @return B baseline values
#' @references 1. Svoboda R. Tangential area-proportional baseline interpolation for complex-process DSC data - Yes or no? Thermochim Acta. 2017;658:55-62. doi:10.1016/J.TCA.2017.10.011.2. Svoboda R. Linear baseline interpolation for single-process DSC data-Yes or no? Thermochim Acta. 2017;655:242-250. doi:10.1016/J.TCA.2017.07.008.
#' @export
#'
#' @examples npoints=1000
#' x=seq(1,npoints)
#' y=(dnorm(seq(1,npoints), mean=npoints/2, sd=npoints/10)) #simulated peak
#' y2=y+(dnorm(seq(1,npoints), mean=npoints, sd=npoints/10)) #secondary simulated peak
#' y2[seq(npoints*0.735,npoints)]=y2[763] #flat the curve at the end of first peak
#' ytap=TAPPA(x,y2)
#' plot(x,y2)
#' lines(x,ytap,col="red")

# Tangential area proportional method
TAPPA <- function(T, dAlpha, interval = 10, tol = 0.001) {
  p <- length(T)
  # Tangents at start and end
  zr <- lm(dAlpha[c(1, interval + 1)] ~ T[c(1, interval + 1)])$coef
  zp <- lm(dAlpha[c(p - interval, p)] ~ T[c(p - interval, p)])$coef

  # Baseline and alpha
  Bprev <- alpha <- seq(0, 1, length.out = p)
  for (i in 1:100) {
    B <- (1 - alpha) * (zr[1] + zr[2] * T) +
      alpha * (zp[1] + zp[2] * T) # (T[p]-T))
    # Estimate of baseline corrected dAlpha
    dAB <- dAlpha - B
    dAB <- dAB - min(dAB) # Force positive
    # Estimate of alpha
    alpha <- cumsum(dAB)
    alpha <- alpha - alpha[1] # Starting at 0
    alpha <- alpha / alpha[p] # Ending at 1
    # Test for convergence
    if (sd(B - Bprev) / sd(Bprev) < tol) {
      break
    }
    Bprev <- B
  }
  return(B)
}
