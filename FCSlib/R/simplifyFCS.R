#' @title Simplify FCS
#' 
#' @description Reduces the amount of data in a data set without altering its overall structure
#' @aliases simplifyFCS
#' @usage simplifyFCS(g, tau, step = 1)
#' @param g A vector containing the FCS data analysis
#' @param tau A vector that represents the time frame between data acquisitions
#' @param step A numeric value that affects the final length of the vector
#' @details The simplifyFCS function performs a log10 weighted binning of the autocorrelation function (acf). It balance the weight of the long-time scale trending behavior of the acf curve, which commonly contain G(tau) points that fluctuate around the zero-correlation regime, hence overweighting fitting with ‘noisy data’. simplifyFCS reduce the weight of the long-time scale trending behavior (ms to sec), preserving the structure of the short-time scales.
#' @note the step parameter must be between 0 and 1
#' @export 
#' @return A vector of the FCS data with reduced length
#' @author Adan O. Guerrero
#' 
#' @seealso \code{\link{gcf}}, \code{\link{var}, \link{mean}}
#' 
#' @examples
#' \donttest{
#' f <- Cy5_100nM$f
#' acqTime <- 2E-6
#' f <- as.vector(f)
#' time <- (1:length(f))*acqTime
#' cy5 <- data.frame(t = time, f)
#' 
#' g <- fcs(x = cy5$f)
#' len <- 1:length(g)
#' tau <-cy5$t[len]
#' G <- data.frame(tau,g)
#' 
#' sfcs <- simplifyFCS(G$g, G$tau, step = 0.5)
#' plot(sfcs$g~sfcs$tau, log = "x", type = "l",
#'      xlab = expression(tau(s)),
#'      ylab = expression(G(tau)), main = "Cy5")
#' 
#' # Comparison, original with simplify
#' plot(G, type = 'l', log = 'x')
#' lines(sfcs$g~sfcs$tau, col = "red")
#' }

simplifyFCS <- function (g, tau, step = 1) {
  if (!(length(g) == length(tau))) {
    stop("'g' and 'tau' must have the same length")
  }
  if (!(step > 0 && step <= 1)) {
    stop("'step' must be greater than 0 and smaller or equal to 1")
  }
  df <- data.frame(g, tau)
  t1 <- ceiling(log10(min(df$tau)))
  t2 <- ceiling(log10(max(df$tau)))
  tR <- 10^(t1:t2)
  ts <- NULL
  for (i in tR) {
    ts <- c(ts, (seq(2, 10, step)) * i)
  }
  s <- NULL
  for (i in 1:length(ts)) {
    if (i == 1) {
      idx <- which(df$tau <= ts[i])
      s <- df[idx, ]
    }
    else {
      idx <- which(df$tau > ts[i - 1] & df$tau <= ts[i])
      if (length(idx)) 
        s <- rbind(s, apply(df[idx, ], MARGIN = 2, mean))
    }
  }
  return(s)
}