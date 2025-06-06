#' @title
#' Plot regression terms for modreg objects
#'
#' @description 
#' Plots smooth components of a fitted modreg object.
#'
#' @export
#' @exportS3Method plot modreg
#' @importFrom mgcv plot.gam
#' 
#' @param x The object to plot, must be of class modreg.
#' @param ... Additional arguments to pass to \code{\link[mgcv]{plot.gam}}.
#' 
#' @details 
#' This function is a wrapper for \code{\link[mgcv]{plot.gam}}. It displays term plots of smoothed variables. Optionally produces term plots for parametric model components as well. Standard errors will not be displayed but can be estimated by \code{boot_modreg}.
#' 
#' @returns 
#' The functions main purpose is its side effect of generating plots. It also silently returns a list of the data used to produce the plots, which can be used to generate customized plots.
#' 
#' @examples 
#' 
#' data(colcancer)
#' # mode regression with P-splines. Convergence criteria are changed to speed up the function
#' reg <- modreg(Surv(logfollowup, death) ~ sex + s(age, bs = "ps"), data = colcancer[1:70, ], 
#' control = modreg.control(tol_opt = 10^-2, tol_opt2 = 10^-2, tol = 10^-3))
#' plot(reg)
#' 
#' 

plot.modreg <- function(x, ...){
  args    <- list(...)
  args$x  <- x$reg
  args$se <- FALSE
  do.call(mgcv::plot.gam, args)
}
