#' Plot coefficients and p-value for Tenv object.
#'
#' Plot method for object returned from \code{TRR.fit} and \code{TPR.fit} functions.
#'
#' \code{coef(x)} must be a two-way tensor or a matrix.
#'
#' Since \eqn{p}-value depend on \eqn{\widehat{\mathrm{cov}}^{-1}\{\mathrm{vec}(\mathbf{X})\}} which is unavailable for the ultra-high dimensional \eqn{\mathrm{vec}(\mathbf{X})} in tensor predictor regression (TPR), the \eqn{p}-value plot is not provided for the object returned from \code{TPR.fit}.
#' Therefore, for the object return from \code{TPR.fit}, only the coefficients plot is displayed. And for the object return from \code{TRR.fit}, both the coefficients plot and \eqn{p}-value plot are displayed.
#'
#' \code{main} and \code{main_p} control the titles of coefficient plot and \eqn{p}-value plot separately. Some other arguments used in function \code{graphics::image}, e.g., \code{xlim, ylim, zlim, col, xaxs, yaxs, etc.,} can be passed to \code{...}
#'
#' \code{ask} can be set as \code{FALSE} if the pause before the second plot is not preferred. If \code{x} is an object from \code{TPR.fit}, no pause is enabled.
#'
#'
#' @param x An object of class \code{"Tenv"}, as the ones returned from \code{TPR.fit} or \code{TRR.fit}.
#' @param level The significant level of p-value. Default is 0.05.
#' @param main The title of coefficient plot.
#' @param main_p The title of \eqn{p}-value plot.
#' @param xlab The title of x-axis.
#' @param ylab The title of y-axis.
#' @param axes A logical value specifying whether the axes should be drawn.
#' @param ask A logical value. If it is TRUE (default), user is prompted before the second plot is shown (if exists).
#' @param ... Other parameters to be passed to the plotting functions.
#'
#' @return No return value.
#'
#' @seealso \code{\link{TRR.fit}, \link{TPR.fit}}
#'
#' @examples
#'  data("bat")
#'  x <- bat$x
#'  y <- bat$y
#'  fit <- TRR.fit(x, y, method="standard")
#'  plot(fit)
#'
#'  ## Change the significant level to 0.1
#'  plot(fit, level = 0.1)

#' @export
#' @importFrom stats coef
#' @importFrom grDevices devAskNewPage grey
#' @importFrom graphics image axis box
plot.Tenv <- function(x, level = 0.05, main=paste0("Coefficient plot ", "(", x$method, ")"), main_p = paste0("P value plot ", "(", x$method, ")"), xlab="", ylab="", axes=TRUE, ask = TRUE, ...){
  cf <- coef(x)
  data <- drop(cf@data)
  data <- t(data)
  if(length(dim(data)) != 2){stop("Can only draw a 2-D plot. Check if coefficients is a 2-D matrix.")}
  devAskNewPage(FALSE)
  image(x = 1:nrow(data), y = 1:ncol(data), z=-data, ylim = c(ncol(data), 1), col = grey(seq(0, 1, length = 256)), main=main, axes = axes, xlab = xlab, ylab = ylab, ...)
  box()
  if(x$call[1] == "TRR.fit()"){
    tmp <- Tenv_Pval(x$x, x$y, Bhat = coef(x))
    p_val <- drop(tmp$p_val@data)
    p_val <- t(p_val)
    if(length(dim(p_val)) != 2){stop("Can only draw a 2-D plot. Check if p-value is a 2-D matrix.")}
    oask <- devAskNewPage(ask)
    on.exit(devAskNewPage(oask))
    image(x = 1:nrow(p_val), y = 1:ncol(p_val), z=(p_val > level), ylim = c(ncol(p_val), 1), col = grey(seq(0, 1, length = 256)), main=main_p, axes = axes, xlab = xlab, ylab = ylab, ...)
    box()
  }
}
