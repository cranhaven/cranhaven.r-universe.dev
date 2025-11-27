#' Normalize a vector
#' @param v A vector to be normalized to have a unit norm
#' @param tol The tolerance value for deciding the v is actually 0, cannot be normalized, and thus return just the 0 vector
#' @returns A vector with the same length as `v` but having unit Euclidean norm
#' @export
Normalize <- function(v, tol=1e-10) {
  v <- as.numeric(v)
  d <- length(v)
  vNorm <- as.numeric(sqrt(crossprod(v)))
  res <- if (!is.na(vNorm) && vNorm <= tol) {
    rep(0, d)
  } else {
    v / vNorm
  }

  res
}


## Project p1 onto p2. If rej is TRUE return the rejection instead
Proj <- function(p1, p2, rej=FALSE, tol=1e-10) {
  p1 <- as.numeric(p1)
  p2 <- Normalize(as.numeric(p2), tol)

  if (sum(p2^2) == 0) {
    stop('p2 cannot be 0')
  }

  proj <- c(crossprod(p1, p2)) * p2
  res <- if (!rej) {
    proj
  } else { # Return the rejection
    p1 - proj
  }

  as.numeric(res)
}


## Generate mean function by exponential map. 
# VtList: a list of coordinate functions for V(t). 
# Makemu.Sphere2D <- function(mfd, VtList, p0, pts=seq(0, 1, length.out=50)) {

  # Vmat <- sapply(VtList, function(f) f(pts))
  # res <- rieExp(mfd, p0, t(Vmat))

  # res
# }


# Makemu.Sphere <- function(mfd, VtList, p0, pts=seq(0, 1, length.out=50)) {

  # Vmat <- sapply(VtList, function(f) f(pts))
  # res <- rieExp(mfd, p0, t(Vmat))

  # res
# }


MakeRotMat <- function(p1, p2, tol=1e-10) {
  d <- length(p1)
  if (length(p1) != length(p2)) {
    stop('dimensions of p1 and p2 must agree')
  }
  if (sum(abs(p1 - Normalize(p1))) > tol || 
      sum(abs(p2 - Normalize(p2))) > tol) {
    stop('p1 and p2 needs to be have norm 1')
  }

  if (sum(abs(p1 + p2)) < tol) {
    warning('Rotation between podal points is arbitrary')
  }

  Sphere <- structure(1, class='Sphere')
  psi <- as.numeric(distance(Sphere, matrix(p1), matrix(p2)))
  p1t <- Normalize(Proj(p1, p2, rej=TRUE))
  R <- diag(1, nrow=d) + 
    sin(psi) * (tcrossprod(p2, p1t) - tcrossprod(p1t, p2)) + 
    (cos(psi) - 1) * (tcrossprod(p2, p2) + tcrossprod(p1t, p1t))

  R
}


#' Helper function for simulations
#' 
#' Get the name of the settings from a named list good for saving into a data frame or use as file name
#' @param settings A named list. The names corresponding to the setting parameter names, and the values are the parameter values
#' @param digits How many digits to use to format the numerical values
#' @param display Which version of the setting name to produce
#' @returns A character vector of setting names
#' @export 
GetSettingName <- function(settings, digits=3, display=c('short', 'pretty', 'tiny')) {

  display <- match.arg(display)

  if (is.null(names(settings))) {
    stop("'settings' must be a named list")
  }

  formatType <- sapply(settings, function(x) {
    if (is.list(x) && length(x) == 1) {
      x <- unlist(x)
    }

    switch(class(x),
      'character' = '%s', 
      'numeric' = sprintf('%%.%dg', digits), 
      'logical' = '%s', 
      'integer' = '%d', 
      stop('Not supported')
      )
  })
  val <- sapply(settings, function(x) {
    if (is.list(x) && length(x) == 1) {
      x <- unlist(x)
    }

    switch(class(x),
      'function' = stop('Not supported'), 
      'factor' = stop('Not supported'), 
      'integer' = round(mean(x)), 
      x[1]
      )
  }, simplify=FALSE)

  if (display == 'short') {
    form <- paste(paste0(names(settings), formatType), collapse='_')
  } else if (display == 'pretty') {
    form <- paste(paste0(names(settings), '=', formatType), collapse=', ')
  } else if (display == 'tiny') {
    len <- 3
    form <- paste(paste0(substr(names(settings), 1, len), formatType), 
                  collapse='_')
    val <- lapply(val, function(x) {
                    if (is.character(x)) {
                      substr(x, 1, len)
                    } else {
                      x
                    }
                  })
  }


  fn <- do.call(sprintf, c(list(form), val))
  fn
}

HSToRootDens <- function(x) {

  p <- length(x)

  if (all(x < 0)) {
    warning('A negative function cannot be projected to a density. Return the uniform density instead')
    x <- rep(sqrt(p - 1) / sqrt(p), p)
  } else {
    x[x < 0] <- 0
    x <- x * sqrt((p - 1) / sum(x^2))
  }

  x
}


HSSampToDensities <- function(samp) {

  # n <- dim(samp)[1]
  # p <- dim(samp)[2]
  # m <- dim(samp)[3]
  dn <- dimnames(samp$X)
  samp$X <- aperm(apply(samp$X, c(1, 3), HSToRootDens), c(2, 1, 3))
  samp$XNoisy <- aperm(apply(samp$XNoisy, c(1, 3), HSToRootDens), c(2, 1, 3))
  dimnames(samp$X) <- dimnames(samp$XNoisy) <- dn
  samp

}
