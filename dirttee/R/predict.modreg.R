#' @title
#' Prediction from a fitted modreg model
#'
#' @description 
#' Takes a fitted modreg object produced by \code{modreg} and produces predictions. New sets of covariates can by supplied through \code{newdata}.
#'
#' @export
#' @importFrom mgcv plot.gam
#' 
#' @param object The object to plot, must be of class modreg.
#' @param ... Additional arguments to pass to \code{\link[mgcv]{predict.gam}}.
#' 
#' @details 
#' This function is a wrapper for \code{\link[mgcv]{predict.gam}}.
#' 
#' @returns 
#' A vector or matrix of predictions. For \code{type = "terms"} this is a matrix with a column per term.
#' 
#' @examples 
#' 
#' data(colcancer)
#' colcancer70 <- colcancer[1:70, ]
#' 
#' mc <- modreg.control(tol_opt = 10^-2, tol_opt2 = 10^-2, 
#' tol = 10^-3)
#' reg <- modreg(Surv(logfollowup, death) ~ sex + s(age, bs = "ps"), data = 
#' colcancer70, control = mc)
#' ndat <- data.frame(sex = rep(colcancer70$sex[1], 200), age = seq(50, 90, length = 200))
#' pr <- predict(reg, newdata = ndat)
#' 
#'

predict.modreg <-
function(object, ...){
  args <- list(...)
  args$object <- object$reg
  do.call(mgcv::predict.gam, args)
}
