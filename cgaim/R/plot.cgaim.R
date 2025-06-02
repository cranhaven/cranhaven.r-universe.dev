#' Plot ridge function
#'
#' Plot method for the ridge and smooth terms of a \code{cgaim} object. If
#'    provided, also plots confidence intervals.
#'
#' @param x A \code{cgaim} object.
#' @param select A numeric or character vector indicating which terms
#'    to plot.
#' @param ci An object returned by a call to \code{\link{confint.cgaim}}. If
#'    \code{NULL}, no confidence interval is drawn.
#' @param ci.plot Whether to plot the confidence intervals as shaded areas
#'    \code{ci.plot = "polygon"} or as lines \code{ci.plot = "lines"}.
#' @param ci.args Additional arguments to be passed to the function used
#'    to draw confidence interval. Either \code{\link[graphics]{polygon}} or
#'    \code{\link[graphics]{lines}}.
#' @param add Logical. If TRUE, adds the function to the current active plot.
#' @param xcenter,xscale Centering and scaling values for the x axis. See
#'    \code{\link[base]{scale}}.
#' @param yshift,yscale Either logical or numeric values to shift and scale 
#'    the ridge functions. See details.
#' @param ... Additional graphical parameters for the drawn function. See
#'    \code{\link[graphics]{par}}.
#'
#' @details The values of \code{yshift} and \code{yscale} determine how
#' ridge functions are shifted and scaled for plotting. This can be used to
#' display the functions over data points for instance. If numeric, a vector
#' can be passed with one value for each plotted function. The vector is 
#' recycled if necessary. This indicate the desired mean and standard deviation 
#' of plotted ridge functions. Note that this is inverse to the parameters
#' in \code{\link[base]{scale}} (and \code{xcenter,xscale}).
#' If TRUE is passed instead, functions are shifted
#' to the intercept and scaled to their corresponding beta coefficients, placing
#' them on the response scale.
#' 
#' @returns The function is called to generate plots and returns no value.
#' 
#' @seealso \code{\link{cgaim}} for the main fitting function and 
#'   \code{\link{confint.cgaim}} for confidence interval computation.
#'
#' @examples 
#' ## Simulate some data
#' n <- 200
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' x3 <- rnorm(n)
#' x4 <- rnorm(n)
#' mu <- 4 * exp(8 * x1) / (1 + exp(8 * x1)) + exp(x3)
#' y <- 5 + mu + rnorm(n)
#' df1 <- data.frame(y, x1, x2, x3, x4)
#' 
#' ## Fit a model
#' ans <- cgaim(y ~ g(x1, x2, label = "foo") + g(x3, x4, label = "bar"), 
#'   data = df1)
#' 
#' ## Default plot method
#' plot(ans)
#' 
#' ## Select variable
#' plot(ans, select = 1)
#' 
#' # Same as
#' plot(ans, select = "foo")
#' 
#' ## Add confidence intervals
#' ci <- confint(ans)
#' plot(ans, select = 1, ci = ci)
#' 
#' ## Change scale and location
#' # On the response scale
#' plot(ans, select = 1, ci = ci, yshift = TRUE, yscale = TRUE)
#' 
#' # Arbitrary scale
#' plot(ans, select = 1, ci = ci, yshift = 1000)
#' 
#' ## Change look
#' 
#' # Main line
#' plot(ans, select = 1, ci = ci, col = 2, lwd = 3)
#' 
#' # Confidence intervals
#' plot(ans, select = 1, ci = ci, col = 2, lwd = 3,
#'   ci.args = list(col = adjustcolor(2, .5)))
#' 
#' # Confidence interval type
#' plot(ans, select = 1, ci = ci, ci.plot = "lines", col = 2, lwd = 3,
#'   ci.args = list(col = 2, lty = 4))
#' 
#' ## Put curves on the same plot (need to shift and scale)
#' plot(ans, select = 1, col = 2, ylim = c(-2, 3))
#' plot(ans, select = 2, col = 4, add = TRUE)
#' 
#' @export
plot.cgaim <- function(x, select = NULL, ci = NULL,
  ci.plot = c("polygon", "lines"), ci.args = list(), add = FALSE,
  xcenter = FALSE, xscale = FALSE, yshift = FALSE, yscale = FALSE, ...)
{
  p <- ncol(x$gfit)
  if (is.null(select)){
    select <- seq_len(p)
  } else {
    if (is.character(select)){
      selmatch <- match(select, colnames(x$gfit))
      nas <- is.na(selmatch)
      if (any(nas)) warning(paste0("Incorrect names removed: ",
        paste(select[nas], collapse = ", ")))
      select <- selmatch[!nas]
    } else {
      inc <- select > p
      if (any(inc)) warning(paste0("Incorrect indices removed: ",
        paste(select[inc], collapse = ", ")))
      select <- select[!inc]
    }
  }
  nsel <- length(select)
  if (nsel > 1) {
    if (add) {
      warning("'add = TRUE' should be used with 'select' to add a single smooth")
    } else {
      grDevices::devAskNewPage(TRUE) 
    }
  }
  plotfun <- ifelse(add, graphics::lines, graphics::plot)
  # Initialize centering and scaling
  if (is.numeric(xcenter)) xcenter <- rep_len(xcenter, nsel)
  if (is.numeric(xscale)) xscale <- rep_len(xscale, nsel)
  if(isTRUE(yshift)) yshift <- x$beta[1]
  if(isFALSE(yshift)) yshift <- 0
  yshift <- rep_len(yshift, nsel)
  if(isTRUE(yscale)) yscale <- x$beta[select + 1]
  if(isFALSE(yscale)) yscale <- 1
  yscale <- rep_len(yscale, nsel)
  # Initialize x
  xs <- cbind(x$indexfit, x$sm_mod$Xcov)
  xs <- scale(xs[, select, drop = FALSE], center = xcenter, scale = xscale)
  # Initialize y
  n <- nrow(x$gfit)
  ys <- x$gfit[, select, drop = FALSE] * 
    matrix(yscale, nrow = n, ncol = nsel, byrow = TRUE) + 
    matrix(yshift, nrow = n, ncol = nsel, byrow = TRUE)
  # Initialize parameters
  defParams <- list(ylab = "g", type = "l")
  dots <- list(...)
  # Initialize cis
  if (!is.null(ci)){
    ci.plot <- match.arg(ci.plot)
    allcis <- ci$g[,select,,drop = FALSE]
    allcis[,,1] <- allcis[,,1] * 
      matrix(yscale, nrow = n, ncol = nsel, byrow = TRUE) + 
      matrix(yshift, nrow = n, ncol = nsel, byrow = TRUE)
    allcis[,,2] <- allcis[,,2] * 
      matrix(yscale, nrow = n, ncol = nsel, byrow = TRUE) + 
      matrix(yshift, nrow = n, ncol = nsel, byrow = TRUE)
    if (ci.plot == "polygon"){
      defArgs <- list(border = NA, col = "grey")      
    }
    if (ci.plot == "lines"){
      defArgs <- list(lty = 2)
    }
    ci.args <- utils::modifyList(defArgs, ci.args)
  }
  # Loop to plot
  for (j in seq_len(nsel)){
    xy <- cbind(xs[,j], ys[,j])
    jord <- order(xy[,1])
    xy <- xy[jord,]
    defParams$xlab <- colnames(xs)[j]
    jpars <- utils::modifyList(dots, defParams)
    jpars$x <- xy[,1]
    jpars$y <- xy[,2]
    if (!is.null(ci)){
      cixy <- cbind(xy[,1], allcis[jord,j,])
      if (is.null(jpars$ylim)){
        jpars$ylim <- range(allcis[,j,])
      }
      do.call(plotfun, jpars)
      if (ci.plot == "polygon"){
        ci.args$x <- c(cixy[,1], rev(cixy[,1]))
        ci.args$y <- c(cixy[,2], rev(cixy[,3]))
        do.call(graphics::polygon, ci.args)
        do.call(graphics::points, jpars)
      }
      if (ci.plot == "lines"){
        ci.args$x <- cixy[,1]
        ci.args$y <- cixy[,2]
        do.call(graphics::lines, ci.args)
        ci.args$y <- cixy[,3]
        do.call(graphics::lines, ci.args)
      }
    } else {
      do.call(plotfun, jpars)
    }
  }
  grDevices::devAskNewPage(FALSE)
  invisible()  
}
