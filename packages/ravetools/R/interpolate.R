find_outliers <- function(x, nsdev = 3, duration = 1, sym = FALSE) {

  if(sym) {
    tmp <- abs(x)
    nsd_ <- stats::sd(tmp, na.rm = TRUE) * nsdev
    idx <- which(is.na(tmp) | tmp > nsd_)
  } else {
    m <- mean(x, na.rm = TRUE)
    nsd_ <- stats::sd(x, na.rm = TRUE) * nsdev
    idx <- which(is.na(x) | abs(x - m) > nsd_)
  }
  outliers <- deparse_svec(idx, max_lag = duration, concatenate = FALSE)

  idx0 <- lapply(outliers, function(out) {
    re <- parse_svec(out)
    if(length(re) < 0.5 * duration) {
      return(NULL)
    } else {
      return(re)
    }
  })
  return(unlist(idx0))
}

smooth_signals <- function(x, time, ord = 4L, nknots = 60, avoid_timepoints = integer(0), regularization = 0.1) {
  is_matrix <- TRUE
  if(!is.matrix(x)) {
    is_matrix <- FALSE
    x <- matrix(x, nrow = 1)
  }
  ntp <- ncol(x)
  if(missing(time)) {
    time <- seq_len(ntp)
  }
  time_idx <- seq_along(time)
  avoid_timepoints <- unique(c(as.integer(avoid_timepoints), which(colSums(is.na(x)) > 0)))
  if(anyNA(avoid_timepoints)) {
    avoid_timepoints <- avoid_timepoints[!is.na(avoid_timepoints)]
  }
  time_idx <- time_idx[!time_idx %in% avoid_timepoints]

  time_slices <- deparse_svec(time_idx, concatenate = FALSE)
  time_range <- max(time) - min(time)
  plan <- sapply(time_slices, function(idx_char) {
    idx <- parse_svec(idx_char)
    time_start <- time[idx[[1]]]
    time_end <- time[idx[[length(idx)]]]
    c(time_start, time_end, (time_end - time_start) / time_range)
  })
  if(ncol(plan) == 1) {
    plan[3,] <- nknots - ord
  } else {
    nknots2 <- ceiling(plan[3,] / sum(plan[3,]) * (nknots - ord))
    while(sum(nknots2) > nknots - ord) {
      tmp <- which.max(nknots2)
      if(nknots2[tmp] >= 2) {
        nknots2[tmp] <- nknots2[tmp] - 1
      } else {
        break
      }
    }
    plan[3,] <- nknots2
    plan <- plan[, nknots2 > 0, drop = FALSE]
  }
  knots <- apply(plan, 2, function(item) {
    time_start <- item[[1]]
    time_end <- item[[2]]
    nknots_ <- item[[3]]
    # Chebychev points
    cos((2 * seq_len(nknots_) - 1) / (nknots_ * 2) * pi) * (time_end - time_start) / 2 + (time_start + time_end) / 2
  })
  knots <- sort(as.double(unlist(knots)))
  if(length(knots) > nknots - ord) {
    knots <- knots[seq_len(nknots - ord)]
  }

  knots <- c(rep(time[1], ord - 1), knots, rep(time[length(time)], nknots - length(knots) - 1))

  B <- t(splines::splineDesign(knots, time, ord = ord, outer.ok = TRUE, sparse = FALSE))
  if(!inherits(B, "matrix")) {
    class(B) <- "matrix"
  }
  bbt <- tcrossprod(B)
  diag(bbt) <- diag(bbt) + regularization
  bbtsolve <- qr.solve(bbt)

  # Initial fit
  if(length(avoid_timepoints)) {
    gamma <- tcrossprod(x[-avoid_timepoints], B[, -avoid_timepoints]) %*% bbtsolve
  } else {
    gamma <- tcrossprod(x, B) %*% bbtsolve
  }
  fitted <- gamma %*% B
  if(!is_matrix) {
    fitted <- as.vector(fitted)
  }
  fitted
}

interp_spline <- function (obj1, obj2, ord = 4L,
                           na.action = stats::na.omit, sparse = FALSE) {
  stopifnot(exprs = {
    (degree <- ord - 1L) >= 0
    length(degree) == 1L
    degree == as.integer(degree)
  })
  frm <- na.action(data.frame(x = as.numeric(obj1), y = as.numeric(obj2)))
  frm <- frm[order(frm$x), ]
  ndat <- nrow(frm)
  x <- frm$x
  if (anyDuplicated(x))
    stop("values of 'x' must be distinct")
  if (length(x) < ord)
    stop(gettextf("must have at least 'ord'=%d points", ord), domain = NA)
  iDeg <- seq_len(degree)

  nu <- ord%/%2L
  knots <- c(x[iDeg] + x[1L] - x[ord], x, x[ndat + iDeg - degree] + x[ndat] - x[ndat - degree])

  derivs <- c(nu, integer(ndat + 2*(nu - 2)), nu)
  x <- c(rep(x[1L], nu - 1), x, rep(x[ndat], nu - 1))
  y <- c(rep(0, nu - 1), frm$y, rep(0, nu - 1))
  des <- splines::splineDesign(knots, x, ord, derivs, sparse = sparse)
  # des <- crossprod(des)
  diag(des) <- diag(des) + 0.001
  coeff <- solve(des, y)
  value <- structure(list(knots = knots, coefficients = coeff, order = ord),
                     formula = do.call("~", list(substitute(obj2), substitute(obj1))),
                     class = c("nbSpline", "bSpline", "spline"))

  value <- splines::polySpline(value)
  coeff <- stats::coef(value)
  coeff[, 1L] <- frm$y
  coeff[1L, degree] <- coeff[nrow(coeff), degree] <- 0
  value$coefficients <- coeff
  return(value)
}


interpolate_missing_signal <- function(x, time, nknots = 100, ord = 4L, ...) {
  # Time
  if(missing(time)) {
    ntp <- length(x)
    time <- seq_len(ntp)
  }
  valid_points <- !is.na(x)

  if(sum(valid_points) > nknots) {
    valid_points <- which(valid_points)
    valid_points <- valid_points[round(seq(1, length(valid_points), length.out = nknots))]
  }

  # ispl <- tryCatch({
  #   splines::interpSpline( obj1 = time[valid_points], obj2 = x[valid_points], ord = 4L, na.action = na.omit, sparse = TRUE, ...)
  # }, error = function(e) {
  #   splines::interpSpline( obj1 = time[valid_points], obj2 = x[valid_points], ord = 4L, na.action = na.omit, sparse = FALSE, ...)
  # })

  ispl <- tryCatch({
    interp_spline( obj1 = time[valid_points], obj2 = x[valid_points], ord = ord, na.action = stats::na.omit, sparse = TRUE, ...)
  }, error = function(e) {
    interp_spline( obj1 = time[valid_points], obj2 = x[valid_points], ord = ord, na.action = stats::na.omit, sparse = FALSE, ...)
  })
  re <- stats::predict( ispl, time )$y
  re[!is.na(x)] <- x[!is.na(x)]
  return(re)
}

#' @title Find and interpolate stimulation signals
#' @param x numerical vector representing a analog signal
#' @param sample_rate sampling frequency
#' @param duration time in second: duration of interpolation
#' @param ord spline order, default is 4
#' @param nknots a rough number of knots to use, default is 100
#' @param nsd number of standard deviation to detect stimulation signals, default is 1
#' @param nstim number of stimulation pulses, default is to auto-detect
#' @param regularization regularization parameter in case of inverting singular matrices, default is 0.5
#' @returns Interpolated signal with an attribute of which sample points are interpolated
#' @examples
#'
#' x0 <- rnorm(1000) / 5 + sin(1:1000 / 300)
#'
#' # Simulates pulase signals
#' x <- x0
#' x[400:410] <- -100
#' x[420:430] <- 100
#'
#' fitted <- interpolate_stimulation(x, 100, duration = 0.3, nknots = 10, nsd = 2)
#'
#' par(mfrow = c(2, 1))
#'
#' plot(fitted, type = 'l', col = 'blue', lwd = 2)
#' lines(x, col = 'red')
#' lines(x0, col = 'black')
#' legend("topleft", c("Interpolated", "Observed", "Underlying"),
#'        lty = 1, col = c("blue", "red", "black"))
#'
#' pwelch(x0, 100, 200, 100, plot = 1, col = 'black', ylim = c(-50, 50))
#' pwelch(x, 100, 200, 100, plot = 2, col = 'red')
#' pwelch(fitted, 100, 200, 100, plot = 2, col = 'blue')
#'
#'
#' @export
interpolate_stimulation <- function(x, sample_rate, duration = 40 / sample_rate,
                               ord = 4L, nknots = 100, nsd = 1, nstim = NULL, regularization = 0.5) {

  # DIPSAUS DEBUG START
  # nknots <- 60
  # ord <- 4
  # nsd <- 3
  # duration <- 4/30000
  # sample_rate <- 30000
  # nstim <- NULL

  ntp <- length(x)

  # Time
  time <- seq_len(ntp)

  # de-trend
  a <- x[[1]]
  b <- x[[ntp]]

  trend <- ((time - 1) / (ntp - 1) * (b - a) + a)
  x <- x - trend

  duration_npts <- duration * sample_rate

  # find outliers
  fitted <- smooth_signals(x = x, time = time, ord = ord, nknots = nknots, regularization = regularization)
  outliers <- find_outliers(x = x - fitted, nsdev= nsd, duration = duration_npts / 4, sym = TRUE)
  if(!length(nstim)) {
    nstim <- length(deparse_svec(outliers, concatenate = FALSE, max_lag = duration_npts / 4))
  }

  # DIPSAUS DEBUG START
  # plot(fitted, type = "l", lty = 1, col = 'red')
  # matlines(x, type = "l", lty = 1, col = 'gray')

  fitted <- smooth_signals(x = x, time = time, ord = ord, nknots = nknots, avoid_timepoints = outliers, regularization = regularization)
  tmp <- (x - fitted)
  tmp[outliers] <- NA_real_
  outliers_new <- find_outliers(x = x - fitted, nsdev = nsd, duration = duration_npts / 4, sym = TRUE)
  outliers <- unique(c(outliers, outliers_new))



  outliers_txt <- deparse_svec(outliers, concatenate = FALSE)
  if(length(outliers_txt) > nstim) {
    outliers_txt <- outliers_txt[order(sapply(outliers_txt, function(idx){
      mean(abs(tmp[parse_svec(idx)]))
      # length(parse_svec(idx))
    }), decreasing = TRUE)]
    outliers_txt <- outliers_txt[seq_len(nstim)]
  }
  outliers <- unlist(lapply(outliers_txt, function(idx) {
    idx <- parse_svec(idx)
    if(length(idx) < duration_npts) {
      pad1 <- floor((duration_npts - length(idx)) / 4)
      if(pad1 > idx[1]) {
        pad1 <- idx[1]
      }
      idx <- (idx[1] - pad1) + seq_len(duration_npts)
    }
    idx
  }))
  # outliers <- parse_svec(idx)

  tmp <- x
  tmp[outliers] <- NA_real_
  # neg <- !is.na(tmp) & tmp < 0
  fitted <- interpolate_missing_signal(tmp, time = time, nknots = nknots, ord = max(2L, ord))
  # fitted[outliers] <- NA_real_
  # outliers_new <- find_outliers(x = x - fitted, nsdev = nsd, duration = duration_npts, sym = TRUE)
  # outliers <- unique(c(outliers, outliers_new))
  #
  # outliers_txt <- deparse_svec(outliers, concatenate = FALSE)
  # if(length(outliers_txt) > nstim) {
  #   outliers_txt <- outliers_txt[order(sapply(outliers_txt, function(idx){
  #     mean(abs(tmp[parse_svec(idx)]))
  #     # length(parse_svec(idx))
  #   }), decreasing = TRUE)]
  #   outliers_txt <- outliers_txt[seq_len(nstim)]
  # }
  # outliers <- unlist(lapply(outliers_txt, function(idx) {
  #   idx <- parse_svec(idx)
  #   if(length(idx) < duration_npts) {
  #     idx <- idx[1] -1 + seq_len(duration_npts)
  #   }
  #   idx
  # }))
  # tmp <- x
  # tmp[outliers] <- NA_real_
  # # neg <- !is.na(tmp) & tmp < 0
  # fitted <- interpolate_missing_signal(tmp, time = time, ord = max(4L, ord), period = NULL)

  # spot outliers again

  # fitted[neg] <- -fitted[neg]

  # DIPSAUSE DEBUG START
  # plot(fitted, type = "l", lty = 1, col = 'red')
  # abline(v = outliers, col = "gray")
  # lines(fitted, type = "l", lty = 1, col = 'red')
  # lines(tmp, type = "l", lty = 1, col = 'black')

  fitted <- fitted + trend

  # fitted <- matrix(fitted, nrow = 1)
  attr(fitted, "interpolated") <- outliers
  fitted
}

