
#' @title Load Shape Score
#'
#'
#'
#'
#' @description \code{lscore} provides a diagnostic score
#' for evaluating the derived load shape in
#' retaining time series properties.
#'
#'
#' @param ls An object of class \code{lslin} or
#' \code{lslog}, created using function \code{\link{lslin}}
#' or \code{\link{lslog}}
#'
#' @param type Type of correlation to be
#' evaluate, either \code{"acf"} or \code{"pacf"}
#'
#' @param output Type of output to be used, either 1 or 2;
#' uses \code{ls$y} if 1 and \code{ls$y2} if 2
#'
#' @param lag Maximum lag at which to calculate the acf or pacf.
#' Same as \code{lag.max} in \code{\link{acf}}.
#' If \code{Null}, then default is used.
#'
#'
#'
#'
#' @details The diagnostic measure is calculated
#' as a weighted mean absolute percent error (MAPE)
#' of auto correlation or partial auto correlation
#' values of the derived series with respect to the original.
#' The values are calculated for given lag. Lag = 0 is omitted
#' from calculation for auto correlation as it would be always 1.
#' If \eqn{o_i} and \eqn{d_i} are the correlation values of
#' original and derived load shape at lag \eqn{i}, then weighted
#' MAPE is calculated as
#'
#'
#'
#'
#' \deqn{wmape = \sum _{i=1}^{lag} { w_i * |(o_i - d_i) / o_i|}}
#' where \eqn{w_i = \frac{|o_i|}{\sum _{i=1}^{lag}|o_i|}}
#'
#'
#' Since  \code{wmape} is a measure of error, lower value
#' indicates better preservation of time
#' series property.
#'
#'
#' @return A list of the followings:
#' \itemize{
#'
#' \item{\code{wmape}: Weighted MAPE.}
#'
#'
#' \item{\code{lag}: Lags at which ACF or PACF
#' values were evaluated and used in calculating \code{wmape}.}
#'
#'
#' \item{\code{type}: Type of Correlation (ACF or PACF)}
#'
#'
#' \item{\code{cor_x}: ACF/PACF values of the original load.}
#'
#'
#' \item{\code{cor_y}: ACF/PACF values of the derived load.}
#'
#'
#' \item{\code{weight}: Weights at different lags used to
#' calculate \code{wmape}}.
#'
#'
#'
#' }
#'
#'
#' @examples
#' loads <- ercot[ercot$Year == 2019, ]$COAST
#' linear_loadshape <- lslin(loads, target_lf = 0.4)
#' # --------------
#' scores_1 <- lscore(linear_loadshape, type = "acf", lag = 20)
#' print(scores_1)
#' # --------------
#' scores_2 <- lscore(linear_loadshape, type = "pacf")
#' print(scores_2)
#'
#'
#'
#'
#'
#'
#'
#' @export
lscore <- function(ls, type = "acf", output = 2,
                   lag = NULL)
{

  # test ls type
  tmp1 <- class(ls) == "lslin"
  tmp2 <- class(ls) == "lslog"
  class_test <- tmp1 || tmp2
  if(!class_test){
    stop("ls must be an object of class 'lslin'
         or 'lslog'")
  }

  # test type
  tmp1 <- type == "acf"
  tmp2 <- type == "pacf"
  type_test <- tmp1 || tmp2
  if(!type_test){
    stop("type must be either 'acf' of 'pacf'")
  }

  # test output
  tmp1 <- output == 1
  tmp2 <- output == 2
  type_test <- tmp1 || tmp2
  if(!type_test){
    stop("type must be either 1 or 2")
  }


  # set x

  x = ls$df$x
  # set y
  if(output == 1){
    y <- ls$df$y
  }else{
    y <- ls$df$y2
  }

  # set lag
  if(is.null(lag)){
    lag <- 10*log10(length(x))
  }

  if(type == "acf"){
    cor_x <- stats::acf(x, plot = FALSE, lag.max = lag)$acf[-1]
    cor_y <- stats::acf(y, plot = FALSE, lag.max = lag)$acf[-1]
  }else{
    cor_x <- stats::pacf(x, plot = FALSE, lag.max = lag)$acf
    cor_y <- stats::pacf(y, plot = FALSE, lag.max = lag)$acf
  }


  wgt <- abs(cor_x)
  wgt <- wgt / sum(wgt)

  ape <- abs((cor_x - cor_y) / cor_x)
  wmape <- sum(wgt * ape)


  return_object <- list(
    wmape = wmape,
    lag = 1:lag,
    type = type,
    cor_x = cor_x,
    cor_y = cor_y,
    weight = wgt
  )

  class(return_object) <- "lscore"
  return(return_object)
}









