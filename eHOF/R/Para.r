#' Curve parameters of eHOF models
#'
#' @aliases Para Para.HOF Para.HOF.list
#'
#' @description
#'   Derive common shape parameters from the different model types.
#'   Calculate a set of parameters (see values below) from eHOF models.
#'
#' @param resp response model results, see [HOF()]
#' @param model response model type. If not specified, the output of [pick.model()] will be used
#' @param newdata vector of gradient values to use
#' @param \dots further arguments passed to or from other methods, e.g. for [pick.model()]
#'
#' @returns
#'  * species Name or abbreviat of the species.
#'  * abund sum Abundance sum, i.e. sum of all response values divided by M.
#' * range Range of x values.
#' * model Model type, if not specified the result of [pick.model].
#' * para Model parameters (a to d).
#' * M Maximum response value, specified in the HOF function call.
#' * mini Location of the minimum, i.e. the gradient value, where the response is lowest, for model VI and VII the lowest response between the two optima.
#' * pess Lowest estimated response value.
#' * top Highest estimated response value(s).
#' * opt Location of the optimum, i.e. the gradient value, where the species response is highest. NA for model I and an optimum interval for model type III.
#' * expect  Expectancy value, i.e. average x value under the model curve).
#' * max slope Highest slope, i.e. maximum of the first derivation of the curve.
#' * centralBorder Following Heegard, the central borders are calculated as the gradient values, where the response reaches \code{"exp(-1/2)"} of the top.
#' * outerBorder Following Heegard, the outer borders of the species niche are calculated as the gradient values, where the response reaches \code{exp(-2)} of the top.
#' * raw mean Average of measured x values.
#'
#' @details
#'   For models VI and VII \code{Para} will give you two expectancy values for the ranges left and right of the pessimum between the model optima. If you want to have the overall expectancy value, use:
#'     \code{
#'       gradient <- seq(min(Para(resp)$range), max(Para(resp)$range), length.out=10000)
#'       weighted.mean(gradient, predict(resp, newdata=gradient))
#'     }
#'
#' @references
#'   Heegard, E. (2002) The outer border and central border for species-environmental relationships estimated by non-parametric generalised additive models. Ecological Modelling, 157, 131-139.
#'
#'   Damgaard, C. (2006) Modelling ecological presence-absence data along an environmental gradient: threshold levels of the environment. Environ Ecol Stat 13:229-236.
#'
#' @author Florian Jansen
#'
#' @keywords models
#'
#' @export
#' @rdname Para
"Para" <-  function(resp, ...) {
  UseMethod("Para")
}
#'
#' @export
#' @rdname Para
"Para.HOF" <- function (
  resp,
  model,
  newdata = NULL,
  ...)
{
  if (missing(model)) model <- pick.model(resp, gam=FALSE, ...)
  x <- if (is.null(newdata)) seq(min(resp$x), max(resp$x), length.out = 10000) else scale01(newdata, ...)
  M <- resp$M
  opt <- Para_opt(resp, model=model, newdata=x, ...)
  border <- Para_niche(resp, newdata=x, model=model, top=opt$top/M, optima=opt$opt, mini=opt$mini, pess=opt$pess, ...)
  slope <- Para_deriv(resp, newdata=x, p=resp$models[[model]]$par, model)
  infl <- try(Para_deriv(resp, newdata=x, p=resp$models[[model]]$par, model, optima=opt$opt, pessima=opt$pess, type='inflection'), silent = TRUE)
  if(!is.numeric(infl)) infl <- NULL

  max.sl <- max(abs(slope))
  out <- list(species = resp$y.name, abund.sum = sum(resp$y/M), range = resp$range, model = model, para = resp$models[[model]]$par, M = M, mini = opt$mini, pess= opt$pess, top = opt$top, opt = opt$opt, max.slope=max.sl,  inflection=infl, expect = opt$expect)
  out$centralBorder <- border$centralBorder
  out$outerBorder <- border$outerBorder
  out$raw.mean <-  mean(resp$x[resp$y>0])
  class(out) <- c("Para.HOF")
  out
}
#'
#' @export
#' @rdname Para
#' @noRd
"Para.HOF.list" <- function (resp, ...)   {
    out <- lapply(resp, Para, ...)
    #   class(out) <- "Para.HOF.frame"
    out
}
