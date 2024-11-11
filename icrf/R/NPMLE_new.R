# y are probability masses (not a density wrt x)
ksmoothR <- function (x, y, bandwidth = 0.5, x.points,
                      sumToOne = TRUE, range.x = range(x), n.points = max(100L, length(x)))
{
  if (missing(y) || is.null(y))
    stop("numeric y must be supplied.\nFor density estimation use density()")
  x.points <- if (missing(x.points))
    seq.int(range.x[1L], range.x[2L], length.out = n.points)
  else {
    n.points <- length(x.points)
    sort(x.points)
  }
  y.points <- double(n.points)
  ord <- order(x)
  n <- length(x)

  x.inf <- is.infinite(x)
  xp.inf <- is.infinite(x.points)

  x[x.inf] <- -1
  x.points[xp.inf] <- -1
  out = .C("ksmoothProb", x[ord], y[ord], n,
           x = x.points, y = y.points, n.points, bandwidth,
           as.integer(x.inf), as.integer(xp.inf), as.integer(sumToOne),
           PACKAGE = "icrf")[4:5]
  out
}
isdSm <-
  function(LR, grid.smooth, btt,
           npmle = NULL,
           time_interest = NULL) {
  ## transform npmle to (smoothed) distribution function
  bandwidth = btt[1]
  t0 = btt[2]
  tau = btt[3]

  if (any(grid.smooth < t0)) {stop("grid.smooth should be always no less than t0.")}
  if (is.null(npmle)) { # if LR is provided, npmle is calculated.
    npmle <- EMICM(LR)
    p = dim(npmle$intmap)[2]
    if (is.infinite(max(LR[, 2]))) {
      # EMICM sometimes incorrectly assigns the remaining probability to the final interval that is finite,
      # even if there are unbounded intervals in the data (so all the mass shouldn't be allocated to a finite interval).
      # See npmle.c lines 183 and 210.
      npmle$intmap[2, p] = Inf
    }
  } else {
    p = dim(npmle$intmap)[2]
  }
  if (is.null(time_interest)) {
    time_interest <- sort(unique(c(t0, unlist(LR), tau)))
    time_interest <- c(time_interest[time_interest <= tau], Inf)
  }
  # Else, npmle is directly used (for compatibility of cox model)
  # only intmap and pf is required.
  #n <- dim(LR)[1]
  #print(npmle)
  t0 = min(t0, npmle$intmap[1, 1])
  t.end = npmle$intmap[2, p]
  # tau = max(tau, if (is.finite(t.end)) {t.end}) # fix tau less than Inf
  if (p == 1) {pf = 1} else {pf = npmle$pf}
  intmap = npmle$intmap
  Fn = function(s) {
    s2 = s
    r1 = suppressWarnings(max(which(s >= intmap[1, ])))
    r2 = suppressWarnings(min(which(s <= intmap[2, ])))
    if (r1 == - Inf) {return (0)}  # if no matches, then it means zero prob.
    if (is.infinite(r2)) {return (1)}  # if no matches, then it means the last interval (F = 1).
    if (r1 < r2 & is.finite(r2)) {return(sum(pf[1:r1]))}  # if time is not in the interval of prob mass, simply return r1_th cum density.

    # look at r1_th column
    p.cum = if (r1 == 1) {0} else {sum(pf[1:(r1-1)])}
    p.int = pf[r1]
    bgn = intmap[1, r1]
    interval = intmap[2, r1] - bgn
    if (is.infinite(interval)| is.infinite(r2)) {
      lambda = - bgn / log(p.int)
      return(1 - exp(-s/lambda)) # exponential tail
    } else {
      return(p.cum + ifelse(interval > 0, p.int * (s-bgn)/interval, 0))
    }
  }
  Fn = Vectorize(Fn)
  if (is.na(bandwidth)) {
    iqr =
      lin.interpolate(0.75, Fn(time_interest), time_interest)["y.interpolate"] -
      lin.interpolate(0.25, Fn(time_interest), time_interest)["y.interpolate"]
    if (iqr > tau/2) iqr = tau/2
    bandwidth = 0.5 * as.numeric(iqr) * dim(LR)[1]^(-0.2)
    # bandwidth = as.numeric(iqr) * dim(LR)[1]^-0.5
  }
  # print(iqr); print(bandwidth)
  tau.grid = tau + bandwidth * 4
  # tau.grid = if (tau == Inf) {intmap[1, p] * (1 + tau.augment)} else tau * (1 + tau.augment)
  t0.aug = time_interest[time_interest - t0 <= bandwidth * 4 & time_interest > t0]
  t0.aug = sort(-(t0.aug - t0))
  tau.aug = seq(from = tau, to = tau.grid, length.out = ifelse(tau == tau.grid, 1, 5))
  #tau.aug = time_interest[time_interest >= tau - bandwidth * 4 & time_interest < tau]
  #tau.aug = tau + sort(tau - tau.aug)
  grid.aug = c(t0.aug, time_interest[time_interest < tau], tau.aug)
  Fhat = data.frame(x = grid.aug, y = Fn(grid.aug))

  phat = Fhat
  phat$y = c(0, Fhat$y[-1] - Fhat$y[-length(grid.aug)])
  if(length(t0.aug) > 0)
    phat$y[1:length(t0.aug)] = phat$y[length(t0.aug) + length(t0.aug):1 + 1] # mirroring
# tmp <<- phat
# tmp.bw <<- bandwidth

  # judging if bandwidth is larger than average grid intervals.
  grid.fin <- grid.smooth[is.finite(grid.smooth)]
  smooth <- bandwidth > (max(grid.fin) - min(grid.fin))/length(grid.fin)
  if (smooth) {
    # Fn$pmf <- c(0, Fn$y[-1] - Fn$y[-length(Fn$y)])
    Fn.smooth <- as.data.frame(ksmoothR(phat$x, phat$y,
                                        bandwidth = bandwidth, x.points = grid.smooth)) #Fhat$x))
    Fn.smooth$y <- cumsum(Fn.smooth$y)

    Fn.smooth$y[is.na(Fn.smooth$y) & grid.smooth > max(phat$x)] <- 1

    Fn.smooth[Fn.smooth$x == t0, "y"] <- 0 # restrict S(t0) = 1
    Fn.smooth[Fn.smooth$x == Inf, "y"] <- 1 # restrict S(Inf) = 1
    if (any(is.na(Fn.smooth$y))) { # when there is still NA, last value carry forward.
      for (i in 2:length(Fn.smooth$y)) {
        if (is.na(Fn.smooth$y[i])) {Fn.smooth$y[i] <- Fn.smooth$y[i-1]}
      }
    }
  } else {
    Fn.smooth = data.frame(x = grid.smooth, y = Fn(grid.smooth))
    Fn.smooth$y[is.na(Fn.smooth$y) & grid.smooth > max(phat$x)] <- 1
    Fn.smooth[Fn.smooth$x == Inf, "y"] <- 1 # restrict S(Inf) = 1
  }
  return(Fn.smooth$y)
}


if (FALSE) {
  l = c(1, 3, 5, 6,7)
  r = c(4, 5, Inf, Inf, Inf)

  isdSm(l, r, seq(0,10,by=.5), tau = 10, bandwidth = 0.4)
  EMICM(cbind(l,r))
}


cdf.mass <- function(L, R, Fn, t0 = Fn$x[1], tau = Fn$x[length(Fn$x)],
                     t.points = sort(unique(c(t0, L, R, tau)))) {
  # data: where monitoring times are extracted from.
  # Fn <- rbind(Fn, c(tau, 1)) # This causes a problem: cdf at tau may not necessarily 1, and if Fn has x values of greater than tau, the order is mixed up.
  t0 <- t0
  tau <- tau
  t.points <- t.points

  cdf <- lin.interpolate.vec(t.points, Fn$x, Fn$y)["y.interpolate", ]
  len.cdf <- length(cdf)
  if (cdf[1] > 0) {
    warning(paste0("cdf at t0 is ", cdf[1], ". This will be forced to be zero."))
    cdf[1] <- 0
  }
  if (cdf[len.cdf] < 1) {
    warning(paste0("cdf at tau is ", cdf[len.cdf], ". This will be forced to be one."))
    cdf[len.cdf] <- 1
  }
  mass <- cdf[-1] - cdf[-length(cdf)]
  data.frame(t.points = t.points, cdf = cdf, mass = c(0, mass))
}
cond.prob <- function(L, R, cdfs) {
  # cdfs: cdf.mass() object
  cond <- ifelse(L < cdfs$t.points & R >= cdfs$t.points, cdfs$mass, 0)
  if (sum(cond) == 0) {
    cond[max(which(R >= cdfs$t.points))] <- 1 #changes made. Feb 2021 (which.max was incorrect which always returns 1, but in fact the last point is needed.)
  } else {
    cond <- cond / sum(cond)
  }
  # if (any(is.na(cond))) cond[] <- 0
  cond
}
cond.prob.vec <- Vectorize(cond.prob, vectorize.args = c("L", "R"))
# cond.prob(34, 42, cdf.mass.tmp)
# cond.prob.vec(rats.isd[, "L"], rats.isd[, "R"], cdfs = cdf.mass.tmp2)

interval2mat <- function(L, R, Fn,
                         t0 = Fn$x[1], tau = Fn$x[length(Fn$x)],
                         t.points = sort(unique(c(t0, L, R, tau)))) {
  t0 <- t0
  tau <- tau
  t.points <- t.points

  cdfs = cdf.mass (L = L, R = R, Fn = Fn, t0 = t0, tau = tau, t.points = t.points)
  out = t(cond.prob.vec(L = L, R = R, cdfs = cdfs))
  colnames(out) = cdfs$t.points
  out
}

