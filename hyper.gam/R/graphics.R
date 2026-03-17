

#' @title Visualize [hyper_gam] object using R package \pkg{graphics}
#' 
#' @description
#' Create \link[graphics]{persp}ective and \link[graphics]{contour}
#' plots of FR-index integrand using R package \pkg{graphics}.
#' 
#' End users are encouraged to use function [integrandSurface()]
#' with \CRANpkg{plotly} work horse.
#' 
#' @param x [hyper_gam] object
#' 
#' @param n \link[base]{integer} scalar, fineness of visualization,
#' default `501L`. See parameter `n.grid` of function \link[mgcv]{vis.gam}.
#' 
#' @param xlab,ylab \link[base]{character} scalars
#' 
#' @param zlab \link[base]{character} scalar, for function [persp.hyper_gam]
#' 
#' @param image_col argument `col` of \link[graphics]{image.default}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns
#' The `S3` method [persp.hyper_gam()], 
#' a method dispatch of S3 generic \link[graphics]{persp},
#' does not have a return value.
#' 
#' @keywords internal
#' @name hyper_gam_graphics
#' @importFrom graphics persp
#' @export persp.hyper_gam
#' @export
persp.hyper_gam <- function(
    x, 
    n = 31L, 
    xlab = 'Percentages',
    ylab = 'Quantiles',
    zlab = 'Integrand of FR-index',
    ...
) {
  
  z <- z_hyper_gam(x, n = n)
  # ?graphics:::persp.default
  persp(x = attr(z, which = 'xy', exact = TRUE),
        z = z, 
        xlab = xlab, ylab = ylab, zlab = zlab,
        ...)
  
  return(invisible()) # ?graphics:::persp.default has an invisible return!
}





#' @rdname hyper_gam_graphics
#' @returns
#' The `S3` method [contour.hyper_gam()],
#' a method dispatch of S3 generic \link[graphics]{contour},
#' does not have a return value
#' 
#' @importFrom graphics contour contour.default image.default
#' @importFrom grDevices topo.colors
#' @export contour.hyper_gam
#' @export
contour.hyper_gam <- function(
    x, 
    n = 501L,
    image_col = topo.colors(20L),
    xlab = 'Percentages',
    ylab = 'Quantiles',
    ...
) {
  
  z <- z_hyper_gam(x, n = n)
  xy <- attr(z, which = 'xy', exact = TRUE)
  
  image.default(
    x = xy, z = z, 
    col = image_col, xlab = xlab, ylab = ylab, ...
  )
  
  contour.default(x = xy, z = z, add = TRUE, ...)
  
  return(invisible())
}


# this is actually another version of [augdata] !!!!!
get_mesh.hyper_gam <- function(x, newdata, n = 501L, ...) {
  
  # I hope to use [get_mesh.hyper_gam()] in [integrandSurface()]
  
  xname <- attr(x, which = 'xname', exact = TRUE)
  X <- x$data[[xname]] 
  x. <- as.double(colnames(X))
  
  if (!missing(newdata)) {
    newX <- newdata[[xname]]
    if (!is.matrix(newX)) stop('`newdata` does not contain a matrix column of functional predictor values')
    newx. <- as.double(colnames(newX))
    if (!all.equal.numeric(newx., x.)) stop('grid of training and test data must be exactly the same')
    yrange <- range.default(X, newX)
  } else yrange <- range.default(X)
  
  # inspired by ?mgcv::vis.gam
  xy <- list(
    x = seq.int(from = min(x.), to = max(x.), length.out = n),
    y = seq.int(from = min(yrange), to = max(yrange), length.out = n)
  )
  
  l <- unique.default(x$data$L)
  if (length(l) != 1L) stop('wont happen')
  
  d_xy <- data.frame(
    expand.grid(xy), # span `x` first, then span `y`
    L = l
  ) # must have nrow() being n*n !!!!
  names(d_xy)[2] <- as.character(xname)
  attr(d_xy, which = 'xy') <- xy
  return(d_xy)
  
}




#' @importFrom mgcv predict.gam
z_hyper_gam <- function(
    x, # returned object from function [hyper_gam()]
    sign_adjusted = TRUE,
    sgn = if (sign_adjusted) x |> cor_xy(probs = .5) |> sign() else 1,
    ...
) {
  
  d <- get_mesh.hyper_gam(x, ...) 
  
  z <- predict.gam(x, newdata = d, se.fit = FALSE, type = 'link') *
    sgn
  n <- sqrt(length(z))
  dim(z) <- c(n, n)
  attr(z, which = 'xy') <- attr(d, which = 'xy', exact = TRUE)
  return(z)
  
}


