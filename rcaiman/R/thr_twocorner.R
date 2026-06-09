#' Compute two-corner thresholds
#'
#' @description
#' Apply Rosin’s geometric corner detector for unimodal histograms
#' \insertCite{Rosin2001;textual}{rcaiman} to both sides of a bimodal canopy
#' histogram as in Macfarlane’s two-corner approach
#' \insertCite{Macfarlane2011;textual}{rcaiman}. Optional slope-reduction as in
#' Macfarlane is supported. Peak detection can use a prominence-based method or
#' Macfarlane’s original windowed maxima.
#'
#' @details
#'
#' Runs the following pipeline:
#' \enumerate{
#'   \item Build an 8-bit histogram of `x` after min–max normalization to
#'   `[0,255]`.
#'   \item Smooth the histogram with a discrete Gaussian kernel of standard
#'   deviation `sigma` (in DN), using reflective padding to mitigate edge
#'   bias.
#'   \item Detect the two mode peaks according to `method`:
#'     \itemize{
#'       \item `method = "prominence"`: find local maxima via discrete
#'       derivatives with plateau handling; find nearest left/right minima; compute
#'       peak prominence as \eqn{\min(y[p]-y[L],\,y[p]-y[R])}; filter by minimum
#'       prominence and minimum peak separation; select the pair that maximizes
#'       \eqn{\min(\mathrm{prom}_\mathrm{left},\,\mathrm{prom}_\mathrm{right})}.
#'       \item `method = "macfarlane"`: search peaks within fixed DN windows as in
#'       \insertCite{Macfarlane2011;textual}{rcaiman}. Peak search is performed on the
#'       **unsmoothed** histogram, as originally proposed.
#'     }
#'   \item Apply Rosin’s corner construction on each mode. The line end at the
#'   “first empty bin after the last filled bin” is determined on the
#'   **unsmoothed** histogram \insertCite{Rosin2001;textual}{rcaiman}. If
#'   `slope_reduction = TRUE` and the peak height exceeds the mean of the
#'   smoothed histogram, the peak height is reduced to that mean before the
#'   geometric construction (Macfarlane’s variant).
#'   \item Derive thresholds:
#'   \deqn{T_l = DN_{lc} + (DN_{uc} - DN_{lc}) \times 0.25}
#'   \deqn{T_m = DN_{lc} + (DN_{uc} - DN_{lc}) \times 0.50}
#'   \deqn{T_h = DN_{lc} + (DN_{uc} - DN_{lc}) \times 0.75}
#'   where \eqn{DN_{lc}} and \eqn{DN_{uc}} are the lower and upper corners.
#' }
#'
#' When `diagnose = TRUE`, a geometric construction like the one shown below is
#' sent to the active graphics device.
#' \if{html}{\figure{twocorner_method.png}{options: style="display:block;margin:0 auto;max-width:40%;"}}
#' \if{latex}{\figure{twocorner_method.png}}
#' When `diagnose = TRUE` and `adjust_par = TRUE`, the function temporarily
#' adjusts margins to draw the geometric construction in a large square format
#' and restores the previous settings on exit. If `adjust_par = FALSE`, no
#' parameters are changed and the plot respects the current device/layout.
#'
#' @param sigma numeric vector of length one. Standard deviation (DN) of the
#'   Gaussian kernel used to smooth the histogram prior to peak detection and
#'   Rosin’s construction.
#' @param method character vector of length one. Peak detection strategy. One of
#'   `"prominence"` (default) or `"macfarlane"`.
#' @param slope_reduction logical vector of length one. If `TRUE`, apply
#'   Macfarlane’s slope-reduction before Rosin’s construction on each side.
#' @param diagnose logical vector of length one. If `TRUE`, plot the geometric
#'   construction.
#' @param adjust_par logical vector of length one. If `TRUE` and
#'   `diagnose = TRUE`, temporarily adjust and then restore graphical
#'   parameters.
#'
#' @inheritParams thr_isodata
#'
#' @return A list with:
#' \describe{
#'   \item{lp}{Lower peak DN.}
#'   \item{lc}{Lower corner DN (Rosin on the left mode).}
#'   \item{tl}{Low threshold derived from `lc` and `uc`.}
#'   \item{tm}{Mid threshold derived from `lc` and `uc`.}
#'   \item{th}{High threshold derived from `lc` and `uc`.}
#'   \item{uc}{Upper corner DN (Rosin on the right mode).}
#'   \item{up}{Upper peak DN.}
#' }
#'
#' @references \insertAllCited{}
#'
#' @export
#'
#' @examples
#' caim <- conventional_lens_image()
#' # Prominence-based peak detection, Gaussian smoothing with sigma = 2 DN
#' thr <- thr_twocorner(caim$Blue[], sigma = 2, slope_reduction = FALSE,
#'                      method = "prominence")
#' # Original Macfarlane peak search (for comparison)
#' thr2 <- thr_twocorner(caim$Blue[], sigma = 2, slope_reduction = TRUE,
#'                       method = "macfarlane")
#' data.frame(unlist(thr), unlist(thr2))
thr_twocorner <- function(x,
                          sigma = 2,
                          slope_reduction = TRUE,
                          method = "prominence",
                          diagnose = FALSE,
                          adjust_par = TRUE
                          ) {

  # Coerce matrix or data.frame to numeric
  if (is.matrix(x) | is.data.frame(x)) {
    x <- as.numeric(x)
  }
  # Validate that x is numeric
  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector or coercible to numeric.")
  }
  .check_vector(sigma, "numeric", 1, sign = "positive")
  .check_vector(slope_reduction, "logical", 1)
  .assert_choice(method, c("prominence", "macfarlane"))
  .check_vector(diagnose, "logical", 1)
  .check_vector(adjust_par, "logical", 1)

  #hardcoded parameters
  peak_min_sep = 20L
  peak_min_prom = 0.05
  peak_edge_guard = 5L

  # Remove NA values
  x <- x[!is.na(x)]

  .find_dn_of_first_empty_bin <- function(h, peak) {
    # “the line starts at the largest bin and finishes at the first empty bin of
    # the histogram following the last filled bin.” ([Rosin, 2001, p. 2084]
    h[1:peak] <- max(h)
    zero_bins <- h == 0
    if (any(zero_bins)) {
      x2 <- which.max(zero_bins) - 1
    } else {
      x2 <- 256
    }
    x2
  }

  # Gaussian smoothing of a discrete histogram (length 256)
  .smooth_hist_gaussian <- function(h, sigma, pad = c("reflect","replicate")) {
    pad <- match.arg(pad)
    stopifnot(length(h) >= 3, is.finite(sigma), sigma > 0)
    r <- ceiling(3 * sigma)
    x <- -r:r
    k <- exp(-(x*x) / (2 * sigma * sigma))
    k <- k / sum(k)  # normalize kernel to unit area

    # build padded vector
    if (pad == "replicate") {
      padL <- rep(h[1], r); padR <- rep(h[length(h)], r)
    } else { # reflect
      padL <- h[seq.int(r+1, 2, by = -1)]
      padR <- h[seq.int(length(h)-1, length(h)-r, by = -1)]
    }
    hp <- c(padL, h, padR)

    # convolution (centered, same length)
    ys <- as.numeric(stats::filter(hp, k, sides = 2))
    ys[(r + 1):(length(ys) - r)]
  }

  .find_extrema <- function(y, kind = c("peak","pit"), eps = 1e-8,
                            edge_guard = 1L, min_sep = NULL) {
    kind <- match.arg(kind)
    n <- length(y); stopifnot(n >= 3)

    dy <- diff(y)
    s  <- ifelse(dy > eps,  1L, ifelse(dy < -eps, -1L, 0L))

    sL <- s; for (i in 2:(n-1)) if (sL[i] == 0L) sL[i] <- sL[i-1]
    sR <- s; for (i in (n-2):1)  if (sR[i] == 0L) sR[i] <- sR[i+1]

    if (kind == "peak") {
      cand <- which(sL[1:(n-2)] ==  1L & sR[2:(n-1)] == -1L) + 1L
    } else {
      cand <- which(sL[1:(n-2)] == -1L & sR[2:(n-1)] ==  1L) + 1L
    }
    if (!length(cand)) return(integer())

    expand_plateau <- function(i) {
      L <- i; while (L > 1L && abs(y[L] - y[L-1L]) <= eps) L <- L - 1L
      R <- i; while (R < n  && abs(y[R] - y[R+1L]) <= eps) R <- R + 1L
      as.integer((L + R) %/% 2L)
    }
    idx <- vapply(cand, expand_plateau, 0L)
    idx <- idx[idx > edge_guard & idx < (n - edge_guard)]
    idx <- unique(idx)
    if (!length(idx)) return(integer())

    if (!is.null(min_sep) && kind == "peak" && length(idx) > 1L) {
      ord <- idx[order(-y[idx])]
      kept <- integer(0)
      for (p in ord) if (!any(abs(p - kept) < min_sep)) kept <- c(kept, p)
      idx <- sort(kept)
    }
    idx
  }

  .compute_prominence <- function(y, peaks, pits, mode = c("side-min","nearest")) {
    mode <- match.arg(mode); n <- length(y)
    vapply(peaks, function(p) {
      Lc <- pits[pits < p]; Rc <- pits[pits > p]
      L <- if (length(Lc)) max(Lc) else if (mode=="nearest") 1L else if (p>1L) which.min(y[1L:(p-1L)]) else 1L
      R <- if (length(Rc)) min(Rc) else if (mode=="nearest") n  else if (p<n) which.min(y[(p+1L):n]) + p else n
      max(0, min(y[p] - y[L], y[p] - y[R]))
    }, 0.0)
  }

  .select_bimodal_pair <- function(y, peaks, pits, min_prom = 0.05, min_sep = 20L) {
    if (length(peaks) < 2L) return(NULL)
    prom <- .compute_prominence(y, peaks, pits, mode = "side-min")
    keep <- prom >= min_prom * max(y)
    peaks <- peaks[keep]; prom <- prom[keep]
    if (length(peaks) < 2L) return(NULL)

    pr  <- t(utils::combn(seq_along(peaks), 2L))
    sep <- abs(peaks[pr[,2]] - peaks[pr[,1]])
    ok  <- sep >= min_sep
    if (!any(ok)) return(NULL)

    score <- pmin(prom[pr[ok,1]], prom[pr[ok,2]])
    tb2   <- sep[ok]
    tb3   <- (y[peaks[pr[ok,1]]] + y[peaks[pr[ok,2]]]) / 2
    ord   <- order(-score, -tb2, -tb3)
    best  <- pr[ok,,drop=FALSE][ord[1],]
    list(left = min(peaks[best]), right = max(peaks[best]))
  }

  .exact_geometric_construction <- function(h, peak_dn, sigma = NULL,
                                            slope_reduction = FALSE) {
    # Rosin, P. L. (2001). Unimodal thresholding. Pattern Recognition, 34(11),
    # 2083–2096. https://doi.org/10.1016/S0031-3203(00)00136-9

    # p2
    # It is important to apply this before smoothing
    x2 <- .find_dn_of_first_empty_bin(h, peak_dn)
    y2 <- 0

    h <- .smooth_hist_gaussian(h, sigma)
    if (slope_reduction && h[peak_dn + 1] > mean(h)) {
      # Macfarlane, C. (2011). Classification method of mixed pixels does not
      # affect canopy metrics from digital images of forest overstorey.
      # Agricultural and Forest Meteorology, 151(7), 833–840.
      # https://doi.org/10.1016/j.agrformet.2011.01.019

      # p1
      x1 <- peak_dn
      y1 <- mean(h)
    } else {
      # p1
      x1 <- peak_dn
      y1 <- h[peak_dn + 1]
    }

    # Prepara las coordenadas de todos los puntos del histograma
    # (xi, yi) con xi en 0:255
    xs <- 0:255
    ys <- h

    # Parámetros de la línea p1→p2
    dx <- x2 - x1
    dy <- y2 - y1
    denom <- sqrt(dx^2 + dy^2)

    # Función de distancia perpendicular punto→línea
    perp_dist <- function(x0, y0, x1, y1, dx, dy, denom) {
      num <- abs(dy * x0 - dx * y0 + x2 * y1 - y2 * x1)
      num / denom
    }

    # Vectorizamos
    dists <- perp_dist(
      x0 = xs,
      y0 = ys,
      x1 = x1,
      y1 = y1,
      dx = dx,
      dy = dy,
      denom = denom
    )

    # perpendiculares incluidas entre p1 y p2 y sobre la linea p1→p2
    l <- stats::line(c(x1,x2), c(y1, y2))
    if (any(is.na(stats::coef(l)))) stop("Peak detection failed")
    lfn <- function(x) stats::coef(l)[1] + stats::coef(l)[2] * x
    valid <- (xs + 1)[ys < lfn(xs) & xs > x1]

    if (length(valid) == 0) stop("Empty valid range")

    # punto con máxima distancia entre los válidos
    i_max <- valid[which.max(dists[valid])]
    max_dist <- dists[i_max]

    # Proyección escalar t = ((x0-x1)*dx + (y0-y1)*dy) / (dx^2+dy^2)
    # Parametro t de la proyección (p1 + t*(p2-p1) es el pie)
    x0 <- xs[i_max]
    y0 <- ys[i_max]
    t <- ((x0 - x1)*dx + (y0 - y1)*dy) / (dx^2 + dy^2)

    # Coordenadas del punto proyectado (pie de la perpendicular)
    xp <- x1 + t * dx
    yp <- y1 + t * dy

    # corner
    list(
      x = xs,
      y = ys,
      p1 = c(x1, y1),
      p2 = c(x2, y2),
      p0 = c(x0, y0),
      perpendicular_foot = c(xp, yp),
      max_dist = max_dist
    )
  }

  .revert_normalization <- function(x, mn, mx) x * (mx - mn) + mn

  mn <- min(x)
  mx <- max(x)
  vals <- (normalize_minmax(x, mn, mx) * 255) %>% round()
  h <- as.numeric(table(factor(vals, levels = 0:255)))

  if (identical(method, "prominence")) {
    h_s <- .smooth_hist_gaussian(h, sigma)
    peaks <- .find_extrema(h_s, "peak", edge_guard = peak_edge_guard)
    pits  <- .find_extrema(h_s, "pit",  edge_guard = peak_edge_guard)
    pair  <- .select_bimodal_pair(h_s, peaks, pits,
                                  min_prom = peak_min_prom,
                                  min_sep  = peak_min_sep)
    if (is.null(pair)) stop("Unimodal histogram")
    DNMAX_left  <- pair$left
    DNMAX_right <- pair$right

  } else {
    # Find peaks (Macfarlane 2011)
    DNL1 <- 5; DNL2 <- 55; DNR1 <- 200; DNR2 <- 250

    repeat {
      DNMAX_left  <- which.max(h[DNL1:DNL2]) + (DNL1 - 1)
      ok_left  <- (DNL2 - DNMAX_left  >= 10)
      if (ok_left) break
      if (!ok_left && DNL2 < 255) DNL2 <- min(255, DNL2 + 25)
      if (DNL2 >= 255) {
        stop("Left peack close to the maximum bin")
      }
    }

    repeat {
      DNMAX_right <- which.max(h[DNR1:DNR2]) + (DNR1 - 1)
      ok_right <- (DNMAX_right - DNR1 >= 10)
      if (ok_right) break
      if (!ok_right && DNR1 > 0) DNR1 <- max(0,   DNR1 - 25)
      if (DNR1 <= 0) {
        stop("Right peak close to the minimum bin")
      }
    }
    if ((DNMAX_right - DNMAX_left) < 20) stop("Unimodal histogram")
  }

  # To draw in a square canvas
  h <- h / max(.smooth_hist_gaussian(h, sigma)) * 255

  # Rosin (2001) modified by Macfarlane (2011)
  left <- .exact_geometric_construction(h, DNMAX_left,
                                        sigma, slope_reduction)
  right <- .exact_geometric_construction(rev(h), 255 - DNMAX_right,
                                         sigma, slope_reduction)

  if (diagnose) {
    .draw <- function(col = "black") {
      segments(x1, y1, x2, y2, lwd=1, col = col)
      points(x1, y1, pch=20, col = col)
      points(x2, y2, pch=20, col = col)
      points(x0, y0, pch=20, col = col)
      points(xp, yp,  pch=20, col = col)
      segments(x0, y0, xp, yp, lty=2, col = col)
      abline(v = x0, lty = 3, col = col)
    }
    xs <- left$x
    ys <- left$y
    x0 <- left$p0[1]
    y0 <- left$p0[2]
    xp <- left$perpendicular_foot[1]
    yp <- left$perpendicular_foot[2]
    x1 <- left$p1[1]
    y1 <- left$p1[2]
    x2 <- left$p2[1]
    y2 <- left$p2[2]

    if (adjust_par) {
      opar <- par()
      on.exit(par(opar))
      par(mar = c(0,0,0,0))
    }
    xlim <- c(0,255)
    plot(xs, ys, type='l', asp = 1, xlim = xlim, ylim = xlim,
         axes = FALSE, xlab = "", ylab = "")
    segments(0,-5,255,-5)

    .draw()

    xs <- 255- right$x
    ys <- right$y
    x0 <- 255 - right$p0[1]
    y0 <- right$p0[2]
    xp <- 255 - right$perpendicular_foot[1]
    yp <- right$perpendicular_foot[2]
    x2 <- 255 - right$p1[1]
    y2 <- right$p1[2]
    x1 <- 255 - right$p2[1]
    y1 <- right$p2[2]

    .draw("blue")
  }

  lc <- .revert_normalization(left$p0[1]/255, mn, mx)
  uc <- .revert_normalization((255 - right$p0[1])/255, mn, mx)

  if (lc > uc) stop("The lower corner is higher than the upper corner")

  list(
    lp = .revert_normalization(DNMAX_left/255, mn, mx),
    lc = lc,
    tl = lc + (uc - lc) * 0.25,
    tm = lc + (uc - lc) * 0.5,
    th = lc + (uc - lc) * 0.75,
    uc = uc,
    up = .revert_normalization(DNMAX_right/255, mn, mx)
  )
}
