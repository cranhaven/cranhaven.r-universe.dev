#' Title smooth.loess
#' @description a wrapper for the loess function included in the R base system
#' @param x variable x
#' @param y variable y
#' @param safe.start exclude a the n-th first values from calculation
#' @param safe.end  exclude a the n-th end values from calculation
#' @param myspan span parameter for loess function
#' @import data.table utils
#' @export
#'
#' @examples npoints=1000
#' x=seq(1,npoints)
#' y=(dnorm(x, mean=npoints/2, sd=npoints/10))
#' y.smooth=smooth.loess(x,y)
#' plot(x,y)
#'
smooth.loess <- function(x, y, safe.start=5, safe.end=5, myspan=0.28) {
  shift <- na.omit(x)[1]
  x <- na.omit(x)
  y <- na.omit(y)
  x.end <- tail((x), n = 1)
  x.start <- head((x), n = 1)
  y.end <- tail((y), n = 1)
  y.start <- head((y), n = 1)
  x.safe <- c(rep(x.start, safe.start), x, rep(x.end, safe.end))
  y.safe <- c(rep(y.start, safe.start), y, rep(y.end, safe.end))
  safe.loessFit <- loess(y.safe ~ x.safe, span = myspan)
  y.safe.loess <- safe.loessFit$fitted
  y.loess <- y.safe.loess[(safe.start + 1):((length(y.safe.loess) - safe.end))]
  return(y.loess)
}
