
# sigtests.R -
# Feature significance tests.
#
# c 2024-2025 Greg Kreider, Primordial Machine Vision Systems, Inc.

## To Do:
# - 
#

#### Public Interface

# Determine the quantile/significance level of the local maximum with height(s)
# ht in a sample with n data points and after low-pass filtering the spacing
# with the kernel type filter of size flp, which may be the total window size
# or a fraction of the data.  lower.tail determines whether the probability
# of the null distribution is less than/equal (TRUE) or greater than ht.
# Returns a list of class 'Ditest' with elements:
#    $method            descriptive text
#    $statfn            name (text) of function used to evaluate statistic
#    $statistic         what is tested, the adjusted standardized peak height
#    $statname          text name of the statistic
#    $parameter         inverse Gaussian/Wald parameters corrht, mu, lambda
#    $p.value           probability of height
#    $alternative       direction of test
#    $model             model parameters ht, n, flp, filter from arguments
# The statistic and p.value will have the length of the ht argument.
Dipeak.test <- function(ht, n, flp, filter, lower.tail=TRUE) {

  n <- n[1L]
  flp <- flp[1L]
  filter <- filter[1L]
  
  check.modelargs(n, flp, lower.tail)
  
  if (1 <= flp) {
    flp <- flp / n
  }

  # NULL ht returns empty vector.  Other invalid values to NA, coercing the
  # vector out of its original type in the process.
  if (0 < length(ht)) {
    ht[!is.numeric(ht)] <- NA_real_
    ht <- as.numeric(ht)
  }

  frat <- peak.filterratio(filter)

  corr <- peak.correction(flp)

  x <- 10^(((ht / frat) - corr["badj"]) / corr["madj"])
  names(x) <- NULL

  mu <- peak.qwald.param(1, n, flp)
  lambda <- peak.qwald.param(2, n, flp)
  p <- statmod::pinvgauss(x, mu, lambda, lower.tail=lower.tail)

  p[is.na(ht)] <- NA_real_
  p[is.nan(ht)] <- NaN

  res <- list(method="low-pass spacing height model",
              statfn="pinvgauss", statistic=ht, statname="ht",
              parameter=list(corrht=x, mu=mu, lambda=lambda), p.value=p,
              alternative=ifelse(lower.tail, "less", "greater"),
              model=c(n=n, flp=flp, filter=filter))
  class(res) <- "Ditest"
  res
}

# Return the local maxima height(s) at significance level(s) pval in a sample
# with n data points and after low-pass filtering with kernel filter of size
# flp, which may be the vector length or a fraction of the data.  Invalid pval
# evaluate to NaN.
# NOTE: when building and verifying models based on quantiles, use pval=1-q.
Dipeak.critval <- function(pval, n, flp, filter) {

  n <- n[1L]
  flp <- flp[1L]
  filter <- filter[1L]

  check.modelargs(n, flp, TRUE)
  
  if (1 <= flp) {
    flp <- flp / n
  }

  if (0 < length(pval)) {
    pval[!is.numeric(pval)] <- NA_real_
    pval <- as.numeric(pval)
  }
  # This is the behavior of the quantile functions.
  pgood <- (pval >= 0) & (pval <= 1)
  pval[!pgood] <- NaN

  frat <- peak.filterratio(filter)

  # Model of the Weibull scale=2, shape=4 null distribution.
  mu <- peak.qwald.param(1, n, flp)
  lambda <- peak.qwald.param(2, n, flp)
  htpred <- statmod::qinvgauss(1-pval, mu, lambda)

  # Could also modify prediction for different distributions.
  #   beta    = 5.76 - (4.62 * htpred)
  #   normal  = 0.93 * htpred
  #   gumbel, gamma, chisq, logistic  = -1.40 + (2.25 * htpred)
  corr <- peak.correction(flp)
  cv <- frat * (corr["badj"] + (corr["madj"] * log10(htpred)))
  names(cv) <- NULL
  cv
}

# Return the quantile/significance level of a flat of length(s) len in a
# sample of n data points drawn from distribution basedist, then low-pass
# filtered with kernel filter of size flp (may be fractional or the actual
# size).  lower.tail determines whether the probability of the null
# distribution is less than/equal (TRUE) or greater than (FALSE) the length.
# Returns a list of class 'Ditest' with elements
#    $method             descriptive text
#    $statfn             name (text) of function used to evaluate statistic
#    $statistic          what is tested, the flat length
#    $statname           text name of the statistic
#    $p.value            probability of length in model
#    $alternative        direction of test
#    $model              model parameters n, flp, filter, basedist from args
# The statistic and p.value will have the length of the len argument.  The
# p-value will be NaN if there's a problem setting up the test, otherwise
# a value between 0 and 1.
Diflat.test <- function(len, n, flp, filter, basedist, lower.tail=TRUE) {

  n <- n[1L]
  flp <- flp[1L]
  filter <- filter[1]
  basedist <- basedist[1L]

  check.modelargs(n, flp, lower.tail)

  if (1 <= flp) {
    flp <- flp / n
  }

  if (0 < length(len)) {
    len[!is.numeric(len)] <- NA_real_
    len <- as.numeric(len)
  }

  pcf <- flat.pcoef(n, flp, filter, basedist)

  optfn <- function(p, l, pcf) {
    sum( c(1, p, p^2, log(p/(1-p))) * pcf ) - l
  }
  
  # Needed for optimize, not uniroot.
  sefn <- function(p, l, pcf) {
    (sum( c(1, p, p^2, log(p/(1-p))) * pcf ) - l)^2
  }

  pmin <- flat.pmin(pcf)
  if (is.na(pmin)) {
    # logit blows up at 0 and 1, so offset a little for finite boundaries.
    prng <- c(.Machine$double.eps, 1-.Machine$double.eps)
  } else {
    # We've built the model only for q >= 0.9.  Assume a linear drop below
    # this, although it doesn't match the actual behavior.
    prng <- c(pmin, 1-.Machine$double.eps)
  }
  lmin <- optfn(prng[1L], 0, pcf)
  lmax <- optfn(prng[2L], 0, pcf)

  p <- sapply(len,
              function(l) {
                if (is.infinite(l)) {
                  ifelse(l < 0, 0, 1)
                } else if (!is.finite(l)) {
                  NaN
                } else if (l < lmin) {
                  pmin * (l / lmin)
                } else if (lmax < l) {
                  1.0
                } else {
                  pval <- safe.uniroot(optfn, prng, l, pcf, maxiter=2500,
                                       tol=.Machine$double.eps)$root
                  if (is.na(pval)) {
                    pval <- optimize(sefn, prng, l, pcf,
                                     lower=prng[1], upper=prng[2],
                                     tol=.Machine$double.eps)$minimum
                  }
                  pval
                }
              })

  if (!lower.tail && (0 < length(p))) {
    p <- 1 - p
  }
  
  res <- list(method="low-pass spacing height model",
              statfn="Diflat.test", statistic=len, statname="len",
              p.value=p,
              model=c(n=n, flp=flp, filter=filter, basedist=basedist),
              alternative=ifelse(lower.tail, "less", "greater"))
              
  class(res) <- "Ditest"
  res
}

# Return the flat length at significance level pval for a draw of size n from
# distribution basedist and smoothed by a low-pass kernel of type filter and
# size flp (may be fractional or the actual size).  Invalid p, or p values
# that are outside the valid region of our model, evaluate to NaN.
# NOTE: when building and verifying models based on quantiles, use pval=1-q.
Diflat.critval <- function(pval, n, flp, filter, basedist) {

  n <- n[1L]
  flp <- flp[1L]
  filter <- filter[1L]
  basedist <- basedist[1L]

  check.modelargs(n, flp, TRUE)

  if (1 <= flp) {
    flp <- flp / n
  }

  if (0 < length(pval)) {
    pval[!is.numeric(pval)] <- NA_real_
    pval <- as.numeric(pval)
  }
  pgood <- (pval >= 0) & (pval <= 1)
  pval[!pgood] <- NaN

  pcf <- flat.pcoef(n, flp, filter, basedist)
  pmin <- flat.pmin(pcf)
  if (is.na(pmin)) {
    pmin <- 0
  }

  # Internally we work with quantiles.
  p <- 1 - pval

  pmat <- matrix(c(rep(1, length(p)), p, p^2, log(p/(1-p))),
                 ncol=4, byrow=FALSE)

  # Replace the model with a linear drop from lmin to 0 over pmin.
  lmin <- (c(1, pmin,  pmin^2, log(pmin/(1-pmin))) %*% pcf)[1]
  # Remove NA values from the selection.
  ltmin <- p < pmin
  ltmin[is.na(ltmin)] <- FALSE
  pmat[ltmin,c(1,3,4)] <- 0
  pmat[ltmin,2] <- p[ltmin] * lmin / (pmin * pcf[2])

  cv <- as.vector( pmat %*% pcf)
  cv[(cv < 0) | !is.finite(cv)] <- NaN
  cv
}

# Perform the Kaplansky-Riordan test on the number of runs in the data vector
# x, using the find.runs detector to count runs whose values are within feps
# of their average (ignored for character, logical, or integer values).
# Runs are tested within each interval between start point(s) stID and
# endpoint(s) endID (incl.).  NA and NaN indices generate NA test values,
# real values are rounded to integers, and non-numeric arguments raise
# errors.  lower.tail if TRUE the probability is that the run count is not
# more than the observed.  Returns a list with class 'Ditest' with elements:
#    $method             descriptive text
#    $statfn             name (text) of function used to evaluate statistic
#    $statistic          what is tested, the number of runs
#    $statname           text name of statistic
#    $parameter          Gaussian distribution parameters Erun, Vrun; feps
#    $p.value            probability of number of runs is <= actual
#    $alternative        direction of test
# The parameters are used to standardize the statistic for the test, with
# Erun being the expected number of runs and Vrun the variance in the count.
# The statistic and p.value will have the same length, one per start/end
# segment, which may be 0 if stID and endID are empty.
Dinrun.test <- function(x, stID, endID, feps, lower.tail=TRUE) {

  check.endargs(stID, endID, length(x))

  if (0 == length(stID)) {
    stats <- matrix(NA_real_, nrow=4, ncol=0)
  } else {
    stID <- round(stID)
    endID <- round(endID)

    # A straightforward implementation of the equations in Example 1 of
    # Section 6 of Kaplansky and Riordan.

    stats <- sapply(seq_along(stID),
                    function(s) {
                      if (is.na(stID[s]) || is.nan(stID[s]) ||
                          is.na(endID[s]) || is.nan(endID[s])) {
                         return(c(NA_real_, NA_real_, NA_real_, NA_real_))
                      }

                      sel <- x[stID[s]:endID[s]]
                      runs <- find.runs(sel, feps)

                      n <- table(sel)
                      # Something changed between R 4.0.1 and 4.4.1 where
                      # n became integers that would overflow on large sets.
                      # Making real fixes, and also wipes the element names.
                      n <- as.double(n)
                      nuniq <- length(n)

                      a1 <- sum(n)
                      if (2 == nuniq) {
                        # Actually the Wald/Wolfowitz test.
                        a2 <- n[1] * n[2]
                        a3 <- 0
                      } else if (3 == nuniq) {
                        a2 <- (n[1]*n[2]) + (n[1]*n[3]) + (n[2]*n[3])
                        a3 <- n[1] * n[2] * n[3]
                      } else {
                        a2 <- 0
                        a3 <- 0
                        for (i in 1:(nuniq-1)) {
                          for (j in (i+1):nuniq) {
                            aij <- n[i] * n[j]
                            a2 <- a2 + aij
                            if ((i < (nuniq - 1)) && (j < nuniq)) {
                              for (k in (j+1):nuniq) {
                                a3 <- a3 + (aij * n[k])
                              }
                            }
                          }
                        }
                      }

                      if (nuniq < 2) {
                        erun <- 0
                        vrun <- 0
                      } else {
                        erun <- 1 + ((2 * a2) / a1)
                        vrun <- (((2*a2) * ((2*a2) - a1)) -
                                 (6*a1*a3)) / (a1 * a1 * (a1-1))
                      }

                      # Per Wald/Wolfowitz (last paragraph Sec. 5),
                      # p is probability of nrun or fewer runs.
                      p <- pnorm(runs$stats$nrun, erun, sqrt(vrun),
                                 lower.tail=lower.tail)
                      c(runs$stats$nrun, erun, vrun, p)
                    })
  }

  res <- list(method="Kaplanksy-Riordan test on number of runs",
              statfn="pnorm", statistic=stats[1,], statname="nrun",
              parameter=list(Erun=stats[2,], Vrun=stats[3,], feps=feps),
              p.value=stats[4,],
              alternative=ifelse(lower.tail, "less", "greater"))
  class(res) <- "Ditest"
  res
}

# Determine the probability of the longest run   in the data vector x, using
# the find.runs detector to count runs whose values are within feps of the
# average (ignored for character, logical, or integer values).  Runs are
# checked within each each interval between start point(s) stID and endpoint(s)
# endID (incl.).  NA and NaN indices generate NA test values, reals are
# rounded to integers, and non-numeric or OOB endpoints raise erros.  Returns
# a list of class 'Ditest' with elements
#    $method             descriptive text
#    $statfn             name of function used to evaluate statistic
#    $statistic          what is tested, the maximum run length
#    $statname           text name of statistic
#    $parameter          test function parameter featlen (endID-stID+1)
#    $p.value            probability of having a run of the given length
#    $tmat               Markov chain transition matrix
#    $smat               Markov chain stationary state
# The statistic, parameter, and p.value will have the same length, one per
# start/end segment.
Dirunlen.test <- function(x, stID, endID, feps) {

  check.endargs(stID, endID, length(x))

  if (0 == length(x)) {
    runlen <- numeric(0)
    featlen <- endID - stID + 1
    p <- numeric(0)
    tmat <- NULL
    smat <- NULL
  } else {
    tmat <- mkv.transition(x)
    smat <- mkv.stationary(tmat)

    if (0 == length(stID)) {
      runlen <- numeric(0)
      featlen <- endID - stID + 1
    } else {
      rl <- sapply(seq_along(stID),
                   function(s) {
                     if (is.na(stID[s]) || is.nan(stID[s]) ||
                         is.na(endID[s]) || is.nan(endID[s])) {
                        c(NA_real_, NA_real_)
                     } else {
                       sel <- x[stID[s]:endID[s]]
                       c(find.runs(sel, feps)$stats$maxrun,
                         sum(!is.na(sel) & !is.nan(sel)))
                     }
                   })
      runlen <- rl[1,]
      featlen <- rl[2,]
    }

    if (0 == length(stID)) {
      p <- numeric(0)
    } else  {
      p1 <- sapply(seq_along(stID),
                   function(s) {
                     test.markov.runlen(runlen[s], featlen[s], tmat, smat)
                   })

      p2 <- sapply(seq_along(stID),
                   function(s) {
                     test.markov.runlen(runlen[s]-1, featlen[s], tmat, smat)
                   })

      p <- p1 - p2
    }
  }
  
  res <- list(method="Markov chain model of longest run",
              statfn="test.markov.runlen", statistic=runlen, statname="runlen",
              parameter=list(featlen=featlen), p.value=p, tmat=tmat, wt=smat)
  class(res) <- "Ditest"
  res
}

# Build a distribution of heights by nexcur draws from data xbase, each draw
# of ndraw points, reconstruct the signal and height depending on whether
# is.peak is TRUE or FALSE for flats, and determine the probability of each
# height ht.  ht and ndraw may be vectors and must have the same length.  If
# lower.tail is TRUE count the number of simulations whose reconstructed
# height is smaller than ht, if FALSE larger; in other words use TRUE for
# flats and FALSE for peaks.  seed if a positive integer is used to set up
# the RNG before the draws.  Returns a list of class 'Ditest' with elements
#    $method             descriptive text
#    $statfn             name (text) of function used to evaluate statistic
#    $statistic          what is tested, the number of runs
#    $statname           text name of statistic
#    $parameter          function arguments stID, endID, permute, nexcur, seed
#    $p.value            probability of number of runs
#    $alternative        direction of test
#    $critval            critical values at quantiles 0.10, 0.05, 0.01, 0.005
#                          (or 1-q if lower.tail TRUE)
#    $xbase              base of samples
Diexcurht.test <- function(ht, ndraw, xbase, nexcur, is.peak, lower.tail=TRUE,
                           seed=0) {

  nexcur <- round(nexcur[1])
  check.htargs(ht, ndraw, xbase, nexcur, lower.tail)

  if (0 < seed) {
    set.seed(seed)
  }

  if (lower.tail) {
    cvID <- round(c(0.005, 0.01, 0.05, 0.10) * nexcur)
    cvnames <- c("cv005", "cv01", "cv05", "cv10")
  } else {
    cvID <- round(c(0.90, 0.95, 0.99, 0.995) * nexcur)
    cvnames <- c("cv90", "cv95", "cv99", "cv995")
  }
  cvID <- pmax(cvID, 1)
  resnames <- c("ht", "pval", cvnames)

  if ((0 == length(xbase)) || (0 == length(ht))) {
    pe <- matrix(NA_real_, nrow=length(ht), ncol=length(resnames),
                 dimnames=list(NULL, resnames))
  } else {
    pe <- sapply(seq_along(ndraw),
                 function(i) {
                   # Minimum draw size to ensure a peak goes to either side.
                   if (!is.finite(ht[i]) || 
                       !is.finite(ndraw[i]) || (ndraw[i] < 3)) {
                     p <- c(ht[i], NA_real_, rep(NA_real_, length(cvID)))
                   } else {
                     excht <- .Call("C_excursion", xbase, nexcur,ndraw[i],
                                    is.peak, TRUE, PACKAGE="Dimodal")
                     pval <- .Call("C_htprob", excht, ht[i], lower.tail,
                                   PACKAGE="Dimodal")
                     p <- c(ht[i], pval, excht[cvID])
                   }
                   names(p) <- resnames
                   p
                 })
    pe <- t(pe)
  }

  res <- list(method="excursion height test",
              statfn="Diexcurht.test", statistic=pe[,"ht"],
              statname="ht",
              parameter=list(ndraw=ndraw, nexcur=nexcur, seed=seed),
              p.value=pe[,"pval"],
              alternatives=ifelse(lower.tail, "less", "greater"),
              critval=pe[,-c(1,2), drop=FALSE], xbase=xbase)
  names(res$statistic) <- NULL
  names(res$p.value) <- NULL
  class(res) <- "Ditest"
  res
}

# Carry out nperm permutations of the values in signed difference vector
# xbase and collect the height (difference of the resulting range) for each.
# We forbid the permutation to have adjacent same-sign symbols.  The
# probability of height(s) ht against this distribution are the number of
# permutations smaller than ht if lower.tail is TRUE, or larger if FALSE.
# seed if a positive integer is used to set the RNG before the draws.
# Returns a list of class 'Ditest' with elements
#    $method             descriptive text
#    $statfn             name (text) of function used to evaluate statistic
#    $statistic          what is tested, the number of runs
#    $statname           text name of statistic
#    $parameter          function arguments stID, endID, permute, nperm, seed
#    $p.value            probability of number of runs
#    $alternative        direction of test
#    $critval            critical values at quantiles 0.10, 0.05, 0.01, 0.005
#                          (or 1-q if lower.tail TRUE)
#    $xbase              base of samples
# Statistic, p.value, critval will be NA if there are two or fewer xbase points
# or no lengths.
Dipermht.test <- function(ht, xbase, nperm, lower.tail=TRUE, seed=0) {

  nperm <- round(nperm[1])
  check.htargs(ht, FALSE, xbase, nperm, lower.tail)

  if (lower.tail) {
    qcv <- c(0.005, 0.01, 0.05, 0.10)
    cvnames <- c("cv005", "cv01", "cv05", "cv10")
  } else {
    qcv <- c(0.90, 0.95, 0.99, 0.995)
    cvnames <- c("cv90", "cv95", "cv99", "cv995")
  }

  if (0 < seed) {
    set.seed(seed)
  }

  # Minimum base size to ensure a peak goes up and down.
  # Does it make sense to impose a stronger minimum, because the resolution
  # will be too low for only a few runs, ie. 5! = 120 ~ 1%?
  if ((length(xbase) < 2) || (0 == length(ht))) {
    cval <- numeric(0)
    pe <- matrix(NA_real_, nrow=length(ht), ncol=2,
                 dimnames=list(NULL, c("ht", "pval")))
  } else {
    xgrp <- sign(xbase)
    # The alternating requirement knocks out a large fraction of the possible
    # permutations.
    if (lfactorial(length(xbase)) < (log(nperm) - log(0.05))) {
      permht <- apply(permute.all.alt(xbase), 1,
                      function(r) { diff(range(cumsum(r))) })
    } else {
      gspec <- setup.permute(xgrp)
      permht <- replicate(nperm, alt.permute(xbase, gspec))
    }
    permht <- sort(permht)

    cval <- permht[pmax(round(qcv * length(permht)), 1)]
    names(cval) <- cvnames

    pval <- sapply(ht,
                   function(h) {
                     if (is.na(h) || is.nan(h)) {
                       NA_real_
                     } else {
                       .Call("C_htprob", permht, h, lower.tail,
                             PACKAGE="Dimodal")
                     }
                   })
    pe <- cbind(ht=ht, pval=pval)
  }

  res <- list(method="run height permutation test",
              statfn="Dipermht.test", statistic=pe[,"ht"],
              statname="ht",
              parameter=list(nperm=nperm, seed=seed),
              p.value=pe[,"pval"],
              alternatives=ifelse(lower.tail, "less", "greater"),
              critval=cval, xbase=xbase)
  names(res$statistic) <- NULL
  names(res$p.value) <- NULL
  class(res) <- "Ditest"
  res
}


#### Implementation Details

# Validity check on the draw and window size arguments or lower.tail flag to
# any of the model test/critical value functions.  Bad values raise an error.
# filter and basedist have defaults.
check.modelargs <- function(n, flp, lower.tail) {

  # This is consistent with R's behavior for pnorm - non-numeric arguments
  # raise errors, logical are coerced to integers.
  if ((0 == length(n)) || !is.finite(n) ||
      (!is.numeric(n) && !is.logical(n))) {
    stop("non-numeric argument n", call.=FALSE)
  }
  if ((0 == length(flp)) || !is.finite(flp) ||
      (!is.numeric(flp) && !is.logical(flp))) {
    stop("non-numeric argument flp", call.=FALSE)
  }
  if (n < 1) {
    stop("argument n must be at least 1", call.=FALSE)
  }
  # This fails if the filter size was given absolutely and is more than
  # half the data.  You probably want to pass the fractional size.
  if ((flp <= 0) || (n < flp)) {
    stop("argument flp must be positive and less than n", call.=FALSE)
  }

  if ((0 == length(lower.tail)) || !is.logical(lower.tail)) {
    stop("non-logical argument lower.tail", call.=FALSE)
  }

  if (((flp < 1.0) && ((flp < 0.05) || (0.30 < flp))) ||
      ((n < 60) || (500 < n))) {
    warning("models used outside recommended range (flp: 0.05-0.30, n 60-500)")
  }
}

# Validity checks on the start indices stID and endpoints endID of intervals
# in a vector of length nx.  Bad values (not numeric, not within vector,
# endID < stID) raise an error.  NA and NaN values are allowed.
check.endargs <- function(stID, endID, nx) {

  if (length(stID) != length(endID)) {
    stop("start and endpoint vectors do not have same length")
  }

  if (any(is.na(stID) | is.nan(stID) | is.na(endID) | is.nan(endID))) {
    return()
  }

  if (0 < length(stID)) {
    if (any(!is.numeric(stID))) {
      stop("not all start points numeric")
    }   
    if (any((stID < 1) | (nx < stID))) {
      stop("some start points lie outside data")
    }
  }

  if (0 < length(endID)) {
    if (any(!is.numeric(endID))) {
      stop("not all endpoints numeric")
    }
    if (any((endID < 1) | (nx < endID))) {
      stop("some endpoints lie outside data")
    }
  }

  if (any(endID < stID)) {
    stop("have endpoints before start points")
  }
}

# Validity checks on the Diexcurht.test and Dipermht.test arguments.  Pass
# FALSE for ndraw for the permutation tests where it is not used.  Bad values
# (empty/non-finite/non-numeric xdraw, negative nperm, ) raise an error.
# ht and ndraw may have NA, NaN values or be an empty vector.
check.htargs <- function(ht, ndraw, xbase, nperm, lower.tail) {

  if ((0 < length(xbase)) &&
      (any(!is.integer(xbase) & !is.numeric(xbase)) ||
       any(!is.finite(xbase)))) {
    stop("xbase set is empty or has non-numeric or non-finite values")
  }

  if ((!is.numeric(nperm) && !is.integer(nperm)) || !is.finite(nperm) ||
      (nperm <= 0)) {
    stop("nperm must be positive")
  }

  if ((1 != length(ndraw)) || !is.logical(ndraw) || (FALSE != ndraw)) {
    if (length(ndraw) != length(ht)) {
      stop("draw sizes do not match heights")
    }

    if ((0 < length(ndraw)) && any(!is.integer(ndraw) & !is.numeric(ndraw))) {
      stop("non-numeric draw sizes")
    }
    if (0 < length(ndraw)) {
      valid <- is.finite(ndraw)
      if (0 < sum(valid)) {
        if (any(ndraw[valid] > length(xbase), na.rm=TRUE)) {
          stop("excursion test samples more points than available")
        }
        if (any(ndraw[valid] < 0)) {
          stop("draw sizes cannot be negative")
        }
      }
    }
  }

  if ((0 < length(ht)) && any(!is.integer(ht) & !is.numeric(ht))) {
    stop("non-numeric heights")
  }
  if (0 < length(ht)) {
    valid <- is.finite(ht)
    if (0 < sum(valid)) {
      if (any(ht[valid] < 0)) {
        stop("feature heights cannot be negative")
      }
    }
  }

  if ((0 == length(lower.tail)) || !is.logical(lower.tail)) {
    stop("non-logical argument lower.tail", call.=FALSE)
  }
}

## Height Models

# Model the correction of the local maxima height relative the FIR standard
# deviation, htsd, for the named filter.
peak.filterratio <- function(filter) {

  # There's a small variation in the correction for the window size and
  # sample draw - not modeling, 10% for n=100/500, win=0.10/0.20 combinations
  # for the first three, 25% for gauss and blackman, 40% for the last two.
  # Also a small variation in quantile.  We're picking the ratio to the Kaiser
  # filter at q=0.999; other quantiles will give larger ratios which
  # under-predicts the ht/sd.
  if ((0 == length(filter)) || is.na(filter) || !is.character(filter)) {
    warning("filter not a character string, using Kaiser")
    frat <- 1.0
  } else if (("triangular" == filter) || ("bartlett" == filter)) {
    frat <- 1.086
  } else if ("hamming" == filter) {
    frat <- 1.122
  } else if ("hanning" == filter) {
    frat <- 1.155
  } else if (("gaussian" == filter) || ("normal" == filter)) {
    frat <- 1.239
  } else if ("blackman" == filter) {
    frat <- 1.248
  } else {
    if ("kaiser" != filter) {
      warning("unsupported filter for peak test, using Kaiser")
    }
    frat <- 1.0
  }

  frat
}

# Calculate the Wald/inverse gaussian parameters param (1 for the mu/mean,
# 2 for the lambda/shape; or by name) for the sample draw size n and
# FIR window width flp (as a fraction of n).
peak.qwald.param <- function(param, n, flp) {

  # Modeled at flp=0.15 only.
  #  p1 <- 3.8356 - (1.1459 * log10(n))
  #  p2 <- 4.762

  # Our model for either parameter is the same: linear against the log of
  # the draw size, where the slope and intercept are linear with the width.
  if ((1 == param) || ("mu" == param) || ("mean" == param)) {
    b <-  5.8158 + (2.4152 * log10(flp))
    m <- -1.9704 - (1.0131 * log10(flp))
  } else if ((2 == param) || ("lambda" == param) || ("shape" == param)) {
    b <- -2.0204 + (49.7357 * flp)
    m <-  2.6034 - (19.5195 * flp)
  }

  # Clip invalid (negative) values to 0.  Returning NA will not work
  # with pinvgauss, if the values also contain NAs.
  max(0, b + (m * log10(n)))
}

# Correction of height quantile in peak model, which depends on the
# fraction filter kernel size flp.  Returns a pair with elements $badj, $madj,
# the intercept and slope of the (linear) correction.
peak.correction <- function(flp) {

  c(badj=-0.2305 + (11.8716 * flp) - (46.9360 * (flp^2)) + (85.0096 * (flp^3)),
    madj=4.2412 - (7.2054 * flp) + (0.1547 / flp))
}

## Length Models

# Get the flat length model coefficients for a parametric draw of size n from
# distribution basedist and smoothed by an FIR kernel filter of fractional
# width flp (must be < 1).  Returns a 4 element vector that must be multiplied
# by the p terms (1, p, p^2, log(p/(1-p))) and summed to get the final
# quantile.
flat.pcoef <- function(n, flp, filter, basedist) {

  if ((0 == length(basedist)) || is.na(basedist) || !is.character(basedist)) {
    warning("basedist not a character string, using logistic")
    pm <- pcoef.length.logistic(filter)
  } else if ("weibull" == basedist) {
    pm <- pcoef.length.weibull(filter)
  } else if (("normal" == basedist) || ("gaussian" == basedist)) {
    pm <- pcoef.length.normal(filter)
  } else if ("gumbel" == basedist) {
    pm <- pcoef.length.gumbel(filter)
  } else {
    if ("logistic" != basedist) {
      warning(paste0("unsupported base distribution ", basedist,
                     ", using logistic"))
    }
    pm <- pcoef.length.logistic(filter)
  }

  nw <- c(1, n, n^2, flp, flp*n, flp*(n^2))

  pcf <- matrix(pm, nrow=4, byrow=TRUE)
  # Sanity check.
  if (ncol(pcf) != length(nw)) {
    stop("internal error - pcoef matrix does not conform to n/flp combinations")
  }

  # Collapse the model by column, leaving a 4 element vector to be multiplied
  # by the p terms.
  pcf %*% nw
}

# Return a vector with the flat length model coefficients based on a logistic
# draw smoothed by FIR kernel filter.  Columns are combinations of draw size
# and fractional kernel window size terms.  Can be cast into a 4x6 matrix by
# row.
pcoef.length.logistic <- function(filter) {

  if ((0 == length(filter)) || is.na(filter) || !is.character(filter)) {
    warning("filter not a character string, using Kaiser")
    filter <- "kaiser"
  }
  if ("hanning" == filter) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c(  895.07,  -6.4915,  0.050202,   -2914.0,   39.755,  -0.12859,
      -1945.1,   14.621,  -0.10957,     6365.8,  -87.586,   0.28017,
       1068.6,   -8.4033,  0.060307,   -3524.0,   49.338,  -0.15443,
      -0.80887, 0.078324, -6.8829e-5,   3.8649, -0.16948,   1.9127e-4)
  } else if ("hamming" == filter) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c( -1.3561,  -2.6245,   0.055274,   -1175.4,   41.627,  -0.15507,
      -45.672,    6.7682,  -0.12187,     2713.6,  -93.135,   0.34047,
       61.9459,  -4.4633,   0.067877,   -1604.3,   53.304,  -0.18934,
       -1.0098, 0.086396,  -9.7872e-5,  4.5358,  -0.19984,   2.8048e-4)
  } else if (("triangular" == filter) || ("bartlett" == filter)) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c( -731.78,   20.902,   9.4621e-3,  1129.6,  -29.898, -0.025898,
       1552.6,   -44.270, -0.023055,   -2347.6,   62.174,  0.062428,
       -814.69,   23.292,  0.014510,    1185.1,  -31.308, -0.039256,
      -0.11633, 0.071427, -7.8624e-5,   1.9172, -0.15436,  2.168e-4)
  } else if (("gaussian" == filter) || ("normal" == filter)) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c(  2248.3,  -20.929,  0.058887,   -7656.4,   82.141,  -0.15955,
       -4859.7,   45.630, -0.12765,    16555,   -178.30,    0.34485,
        2639.1,  -25.057,  0.069560,   -8993.5,   97.708,  -0.18757,
      -0.83352, 0.074709, -4.7317e-5,   3.5018, -0.15093,   1.2987e-4)
  } else if ("blackman" == filter) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c(  1616.7,  -18.638,  0.051899,   -5458.4,   73.494,   -0.13673,
       -3490.0,   40.584, -0.11238,    11782.1, -159.31,     0.29516,
        1894.7,  -22.256,  0.061167,   -6397.7,   87.202,   -0.16035,
      -0.41984, 0.067871, -3.3585e-05,  2.0423,   -0.12768,  8.6115e-5)
  } else {
    if ("kaiser" != filter) {
      warning("unsupported filter ", filter, ", using kaiser")
    }
    #   const         n        n^2        flp     flp*n     flp*n^2
    c(-2389.1,   39.048, -0.025085,   5.9153e+3,  -72.984,  0.047613,
       5122.2,  -83.133,  0.050001,  -1.2677e+4,  154.64,  -0.094133,
      -2739.2,   44.077, -0.023815,   6.7609e+3,  -80.650,  0.042964,
      0.43973, 0.069628, -9.5534e-5,    0.41845, -0.15918,  2.807e-4)
  }
}

# Return a vector with the flat length model coefficients based on a Weibull
# draw smoothed by FIR kernel filter.  Can be cast into a 4x6 matrix by row.
pcoef.length.weibull <- function(filter) {

  if ((0 == length(filter)) || is.na(filter) || !is.character(filter)) {
    warning("filter not a character string, using Kaiser")
    filter <- "kaiser"
  }
  if ("hanning" == filter) {
    #   const         n        n^2        flp     flp*n      flp*n^2
    c( -4.9840,  -3.2583,   1.8028e-3,  330.12,   19.067,    0.033660,
       17.469,    7.1345,  -3.8490e-3, -711.49,  -41.233,   -0.073340,
       -6.5314,  -3.9661,   2.1845e-3,  360.79,   22.817,    0.039462,
       0.72389, 0.026799,   2.1729e-5, -2.1478,  8.0694e-3, -8.6371e-5)
  } else if ("hamming" == filter) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c(  86.658,  -6.2163,   9.3797e-3,  -700.22,    37.300,   9.2935e-3,
      -175.64,   13.469,   -0.020101,    1489.3,   -80.287,  -0.021620,
        93.357,  -7.3396,   0.010898,    -804.34,   43.671,   0.011922,
       0.58325, 0.027359,   2.5845e-5,  -2.0106,   0.00681,  -9.7996e-5)
  } else if (("triangular" == filter) || ("bartlett" == filter)) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c(  133.22,  -6.5424,  0.012183,   -570.49,    36.029,  -7.5517e-4,
       -281.46,  14.191,  -0.026233,   1223.8,    -77.590,   5.4783e-4,
        154.47,  -7.7411,  0.014236,   -669.89,    42.186,   2.2254e-5,
       0.36176, 0.034808,  1.585e-5,   -1.3107, -0.015858,  -8.0323e-5)
  } else if (("gaussian" == filter) || ("normal" == filter)) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c(  81.895,  -5.3860,  4.3516e-3,  -16.150,    21.258,   0.014945,
      -160.95,   11.623,  -9.2230e-3,   24.492,   -45.817,  -0.032756,
        83.962,  -6.3206,  4.9779e-3,  -22.006,    25.059,   0.017657,
        1.3213, 0.019862,  2.0665e-5,   -3.9419, 0.026294,  -6.4009e-5)
  } else if ("blackman" == filter) {
    #    const         n        n^2        flp     flp*n    flp*n^2
    c(  -254.86,  -2.3780,  5.5634e-5,  1055.6,    12.453,   0.026062,
         551.58,   5.2308, -1.2819e-4, -2256.2,   -26.961,  -0.056475,
        -292.19,  -2.9261,  1.6688e-4,  1189.1,    14.961,   0.030318,
         1.1371, 0.019393,  1.9808e-5, -3.5074,  0.030359,  -6.5452e-5)
  } else {
    if ("kaiser" != filter) {
      warning("unsupported filter ", filter, ", using kaiser")
    }
    #   const         n        n^2        flp     flp*n    flp*n^2
    c(  185.83,  -8.9368,  0.024456,   -1864.9,    61.922,  -0.063500,
       -392.38,  19.341,  -0.052597,    3978.0,  -132.97,    0.13362,
        211.76, -10.507,   0.028442,   -2133.5,    71.896,  -0.070919,
      0.010027, 0.038499,  1.4777e-5,  0.26152, -0.035014,  -6.5996e-5)
  }
}

# Return a vector with the flat length model coefficients based on normal
# draws smoothed by FIR kernel filter.  Can be cast into a 4x6 matrix by row.
pcoef.length.normal <- function(filter) {

  if ((0 == length(filter)) || is.na(filter) || !is.character(filter)) {
    warning("filter not a character string, using Kaiser")
    filter <- "kaiser"
  }
  if ("hanning" == filter) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c(  209.32,  -7.3078,  0.011596,   -711.87,    36.242,  -2.1480e-3,
       -448.08,  15.925,  -0.025068,   1529.6,    -78.318,   3.8716e-3,
        246.65,  -8.7548,  0.013716,   -846.01,    42.923,  -2.2864e-3,
       0.59752, 0.036907,  1.4542e-5,   -1.604, -0.026236,  -6.7247e-5)
  } else if ("hamming" == filter) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c(  307.04,  -10.703,  0.022635,   -1405.5,    54.554,  -0.032498,
       -658.02,   23.252, -0.048878,    3028.0,  -117.83,    0.068855,
        356.80,  -12.685,  0.026553,   -1644.8,    64.186,  -0.037172,
       0.54728, 0.036049,  2.1316e-5,  -1.7339, -0.024355,  -8.694e-5)
  } else if (("triangular" == filter) || ("bartlett" == filter)) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c(  443.26,  -10.033,  0.025407,    -1897.6,    55.841, -0.056668,
       -946.30,   21.779, -0.054899,     4064.4,  -120.38,   0.121160,
        510.96,  -11.885,  0.029831,    -2192.6,    65.403, -0.065230,
       0.17459, 0.046524,  4.2899e-6,  -0.34232, -0.059278, -4.144e-5)
  } else if (("gaussian" == filter) || ("normal" == filter)) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c(  262.78,  -7.8587,  8.9202e-3,  -986.59,     33.510,  -4.2118e-3,
       -550.32,  16.984,  -0.019065,   2093.2,     -72.079,   7.9891e-3,
        292.97,  -9.2384,  0.010298,  -1123.7,      39.192,  -4.0579e-3,
        1.6729, 0.025024,  2.2439e-5,  -4.9313,   8.3653e-3, -7.3441e-5)
  } else if ("blackman" == filter) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c(  74.431,  -6.1651,  6.0550e-3,   7.4568,     24.212,  0.011533,
      -145.40,   13.308,  -0.012853,  -34.403,    -52.049,  -0.025881,
        76.036,  -7.2398,  6.9213e-3,  12.338,     28.388,   0.014190,
        1.2774, 0.024892,  2.1922e-5,  -3.7843,  0.011591,  -7.6688e-5)
  } else {
    if ("kaiser" != filter) {
      warning("unsupported filter ", filter, ", using kaiser")
    }
    #   const         n        n^2        flp     flp*n    flp*n^2
    c ( 741.06,  -17.064,  0.053711,   -3807.4,    93.135,  -0.16180,
      -1593.3,    36.962, -0.11593,     8194.4,   -200.65,   0.34606,
        861.61,  -20.087,  0.062820,   -4425.2,    108.73,  -0.18599,
      -0.56429, 0.055043, -1.137e-5,    2.2923, -0.089751,   8.0502e-6)
  }
}

# Return a vector with the flat length model coefficients based on a Gumbel
# draw smoothed by FIR knerel filter.  Can be cast into a 4x6 matrix by row.
pcoef.length.gumbel <- function(filter) {

  if ((0 == length(filter)) || is.na(filter) || !is.character(filter)) {
    warning("filter not a character string, using Kaiser")
    filter <- "kaiser"
  }
  if ("hanning" == filter) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c( -1598.0,   47.873, -0.032367,    4161.1,  -106.21,  0.073324,
        3410.9, -102.175,  0.065916,   -8822.6,   225.65, -0.147592,
       -1804.3,   54.258, -0.032486,    4613.0,  -118.40,  0.071224,
      -0.23631, 0.075773, -9.6954e-5,   2.6738, -0.16307,  2.5834e-4 )
  } else if ("hamming" == filter) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c( -4048.0,   77.083,  -0.10121,      11980,  -197.31,   0.27878,
        8685.7, -164.802,   0.21272,     -25613,   420.27,  -0.58524,
       -4646.0,   87.778,  -0.11039,      13626,  -221.98,   0.30297,
        1.0524,  0.06882,  -1.0146e-4, -0.56980, -0.16052,   2.9109e-4 )
  } else if (("triangular" == filter) || ("bartlett" == filter)) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c( -2278.6,   74.222,  -0.11808,    6609.5,   -188.44,   0.32789,
        4964.3, -160.127,   0.25141,  -14335.1,    405.65,  -0.69687,
       -2698.3,   86.257,  -0.13276,    7743.4,   -217.24,   0.36735,
       1.6654, 0.044069,  -5.5019e-5, -2.9821, -0.079026,   1.3708e-4 )
  } else if (("gaussian" == filter) || ("normal" == filter)) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c(  18.961, -0.26832,   0.066611,   -1454.5,   39.102,  -0.21431,
       -79.873,  1.68360,  -0.146120,    3278.1,  -87.435,   0.46803,
        80.357, -1.79288,   0.080824,   -1900.2,   50.104,  -0.25722,
       -1.2499, 0.095639,  -1.0463e-4,   4.8565, -0.20499,   2.6783e-4 )
  } else if ("blackman" == filter) {
    #   const         n        n^2        flp     flp*n    flp*n^2
    c(  148.96,   13.913,   0.038246,   -1735.7,  -6.8326,  -0.12234,
       -381.39,  -28.733,  -0.085554,    3944.8,  11.1196,   0.27155,
        256.95,   14.512,   0.048553,   -2302.8,  -2.6911,  -0.15256,
       -1.4974, 0.093077,  -1.0534e-4,   6.0483,  -0.20031,  2.74553e-4 )
  } else {
    if ("kaiser" != filter) {
      warning("unsupported filter ", filter, ", using kaiser")
    }
    #   const         n        n^2        flp     flp*n    flp*n^2
    c( -2286.5,   77.418,  -0.14717,    7699.7,   -209.25,   0.42424,
        5091.1, -168.237,   0.31626,  -17056.9,    454.52,  -0.91247,
       -2834.9,   91.323,  -0.16862,    9425.3,   -245.53,   0.48656,
       3.0400, 0.031533,  -4.2934e-5, -6.9384, -0.056694,   1.24383e-4 )
  }

}

# Find the location of the minimum of our flat model.  pcf is the matrix
# returned from flat.pcoef.  Return NA if there is no minimum, or the largest
# value below 1 if there are multiple zeroes.
flat.pmin <- function(pcf) {

  # Our model is
  #  pcf[1] * (p * pcf[2]) + (p^2 * pcf[3]) * (log(p/(1-p)) * pcf[4])
  # and the gradient is
  #  pcf[2] + (2 * p * pcf[3]) + ((1/p) * (1/(1-p)) * pcf[4])
  # Extrema are located where gradient == 0, which is a cubic equation and
  # can be solved directly rather than by a root finder.  The solution is
  # based on the Wikipedia cubic equation page.

  if ((0 == pcf[2]) && (0 == pcf[3])) {
    exp(-pcf[1]/pcf[4]) / (1.0 + exp(-pcf[1]/pcf[4]))
  } else if (0 == pcf[3]) {
    (1.0 + sqrt(1.0 + (4.0 * pcf[4] / pcf[2]))) / 2.0
  } else {
    B <- pcf[2] / (2.0 * pcf[3])
    D <- pcf[4] / (2.0 * pcf[3])
    E <- -((B^2) + B + 1.0) / 3.0
    F <- ((2.0*(B^3)) + (3.0*(B^2)) - (3.0*B) - (27.0*D) - 2.0) / 27.0
    delta <- ((E^3) / 27.0) + ((F^2) / 4.0)
    # Treat values close to zero as matching
    eps <- 1.0e-10
    if (eps < delta) {
      # Other two roots are imaginary.
      t0 <- cbrt((-F/2.0) * sqrt(delta)) + cbrt((-F/2.0) - sqrt(delta))
    } else if (delta < -eps) {
      phis <- (acos(((3 * F) / (2 * E)) * sqrt(-3/E)) - (2 * (0:2) * pi)) / 3
      t0 <- 2.0 * sqrt(-E/3.0) * cos(phis)
    } else {
      t0 <- c((3.0 * F) / E, (-3.0 * F) / (2.0 * E))
    }
  }

  # Shift back to p.
  t0 <- t0 - ((B - 1.0) / 3.0)
  t0 <- t0[is.finite(t0)]
  if ((0 == length(t0)) || (0 == sum((0 <= t0) & (t0 < 1)))) {
    NA_real_
  } else {
    max(t0[t0<1])
  }
}

# Wrapper around uniroot that returns error.value for $root instead of stopping
# if there's a problem finding a root.  Arguments passed through to uniroot.
safe.uniroot <- function(..., error.value=NA_real_) {
  dummy <- list(root=error.value, f.root=NULL, iter=NA, init.it=NA,
                estim.prec=NA)
  
  tryCatch(uniroot(...),
           error=function(e) { dummy }, warning=function(w) { dummy })
}

# Return the cube root of each x, which may be negative, unlike x^(1/3).
cbrt <- function(x) {

  sign(x) * (abs(x)^(1/3))
}

## Markov Chains

# Build the transition matrix for a first order Markov chain along data x
# for the unique values lvls, which can be a superset of what is present in
# x (in case some values are missing in a subset).  If rel is TRUE normalize
# each row to sum to 1, except if there are no transitions to the final
# state along it, where the sum will be 0, otherwise return the raw
# transition counts.  The transition matrix is #lvls^r x #lvls.
mkv.transition <- function(x, lvls=sort(unique(x), na.last=NA), rel=TRUE) {

  x <- x[is.finite(x)]

  nlvl <- length(lvls)
  
  # Guarantee that each possible combination is counted at least once.
  # The vectors are length nlvl^2.
  pairst <- sapply(lvls, rep, nlvl, simplify="vector")
  pairend <- sapply(1:nlvl, function(i) { lvls }, simplify="vector")
  tmat <- table(start=c(x[-length(x)], NA, pairst),
                  end=c(x[-1L], NA, pairend))
  # The pairs mean tmat is nlvl x nlvl.  Values in x not in lvls increase
  # the size.
  if ((nlvl != nrow(tmat)) || (nlvl != ncol(tmat))) {
    stop("data contains unrecognized levels")
  }
  tmat <- tmat - 1

  if (rel) {
    tmat <- tmat / rowSums(tmat)
    tmat[!is.finite(tmat)] <- 0
  }

  tmat
}

# Return the steady-state state vector for Markov transition matrix tmat.
# If none is found, return a vector of NAs.  
mkv.stationary <- function(tmat) {

  ttmat <- t(tmat)
  ev <- eigen(ttmat)

  if (1e-4 < abs(Mod(ev$values[1] - 1))) {
    dummy <- rep(NA_real_, ncol(tmat))
    names(dummy) <- colnames(tmat)
    dummy
  } else {
    smat <- ttmat %*% ev$vectors
    Re(smat[,1] / sum(smat[,1]))
  }
}

# Return the probability that all runs are <= runlen in length for
# trajectories of featlen steps through a Markov chain with transition
# matrix tmat.  Consider all paths starting from state(s) i; if more than
# one, scale by weight wt (stationary matrix or 1/nrow(tmat) for uniform
# starts).  If transition matrix is bad (b.v. only one state) return NA.
test.markov.runlen <- function(runlen, featlen, tmat, wt, i=1:nrow(tmat)) {

  runlen <- runlen[1L]
  featlen <- featlen[1L]

  if (!is.finite(runlen) || !is.finite(featlen)) {
    return(NA_real_)
  }

  if ((ncol(tmat) != nrow(tmat)) ||
      !all(abs(rowSums(tmat) - 1.0) < 2*.Machine$double.eps)) {
    warning("bad transition matrix in longest runs test")
    return(NA_real_)
  }
  if (nrow(tmat) != length(wt)) {
    warning("weight/stationary vector incompatible with transition matrix")
    return(NA_real_)
  }

  # Comment out this if you want the original, slower R implementation.
  return( .Call("C_test_runlen", runlen, featlen, tmat, wt, PACKAGE="Dimodal") )

  if ((runlen < 1) || (featlen < 1)) {
    return(0)
  } else if (featlen <= runlen) {
    return(1)
  }

  # A advances to the next run length.  R resets back to len=1.
  A <- diag(diag(tmat))
  R <- tmat - A
  
  recur <- array(0, c(runlen, nrow(tmat), ncol(tmat)))
  recur[runlen,,] <- A

  if (2 <= featlen) {
    for (t in 2:featlen) {
      top <- R %*% recur[1,,]
      prev <- recur[runlen,,]
      recur[runlen,,] <- top + A
      if (1 < runlen) {
        for (j in (runlen-1):1) {
          curr <- top + (A %*% prev)
          prev <- recur[j,,]
          recur[j,,] <- curr
        }
      }
    }
  }

  plong <- rowSums(recur[1,,])[i]

  if (1 < length(i)) {
    1 - sum(plong * wt)
  } else {
    1 - plong
  }
}

## Runs Permutations

# Separate the group identifiers grp (any class, we take unique values) in
# preparation for a permutation that separates them.  Returns a list with
#   $grpval      unique identifiers
#   $grpID       position in grp of each identifier, in grpval order
#   $ngrp        population of each symbol
setup.permute <- function(grp) {

  # Doing this once before the permutations, rather than at the top of
  # alt.permute, cuts the run time roughly in half, from 4.2 s to 2.2 s
  # for the test bench.
  
  grpval <- sort(unique(grp))
  grpID <- lapply(grpval, function(g) { which(grp == g) })
  ngrp <- sapply(grpID, length)

  list(grpval=grpval, grpID=grpID, ngrp=ngrp)
}

# Return the height of a permutation, or the permutation itself if raw TRUE,
# of the values in x, with the added condition that no two adjacent will
# belong to the same group, described by the setup.permute result grpspec.
alt.permute <- function(x, grpspec, raw=FALSE) {

  grpval <- grpspec$grpval
  # Scramble order within each group.
  grpID <- lapply(grpspec$grpID, function(g) { g[sample.int(length(g))] })
  ngrp <- grpspec$ngrp
  
  perm <- integer(length(x))
  
  if (1 == length(grpval)) {
    perm <- x[grpID[[1]]]
  } else {
    sID <- .Call("C_altperm_symbols", ngrp, PACKAGE="Dimodal")
    for (i in seq_along(grpval)) {
      perm[sID==i] <- x[grpID[[i]]]
    }
  }

  if (raw) {
    perm
  } else {
    # This is the old height, suitable for flats.
    # diff(range(cumsum( perm )))
    cp <- cumsum(perm)
    max(cp) - min(cp[1L], cp[length(cp)])
  }
}

# Exhaustively permute the elements of vector x, deleting any where the sign
# of adjacent elements is the same.  Returns a matrix with length(x) columns
# and permutations in rows.
permute.all.alt <- function(x) {

  perm <- permute.all(x)
  dropcol <- -ncol(perm)
  keep <- apply(perm, 1, function(r) { all(sign(r[-1L]) != sign(r[dropcol])) })
  perm[keep,,drop=FALSE]
}

# Exhaustively permute the elements of vector x.  Returns a matrix with
# length(x)! rows and length(x) columns.
permute.all <- function(x) {

  if (4 == length(x)) {
    matrix(c(x[1],x[2],x[3],x[4], x[1],x[2],x[4],x[3], x[1],x[3],x[2],x[4],
             x[1],x[3],x[4],x[2], x[1],x[4],x[2],x[3], x[1],x[4],x[3],x[2],
             x[2],x[1],x[3],x[4], x[2],x[1],x[4],x[3], x[2],x[3],x[1],x[4],
             x[2],x[3],x[4],x[1], x[2],x[4],x[1],x[3], x[2],x[4],x[3],x[1],
             x[3],x[1],x[2],x[4], x[3],x[1],x[4],x[2], x[3],x[2],x[1],x[4],
             x[3],x[2],x[4],x[1], x[3],x[4],x[1],x[2], x[3],x[4],x[2],x[1],
             x[4],x[1],x[2],x[3], x[4],x[1],x[3],x[2], x[4],x[2],x[1],x[3],
             x[4],x[2],x[3],x[1], x[4],x[3],x[1],x[2], x[4],x[3],x[2],x[1]),
             nrow=24, byrow=TRUE)
  } else if (3 == length(x)) {
    matrix(c(x[1],x[2],x[3], x[1],x[3],x[2],
             x[2],x[1],x[3], x[2],x[3],x[1],
             x[3],x[1],x[2], x[3],x[2],x[1]), nrow=6, byrow=TRUE)
  } else if (2 == length(x)) {
    matrix(c(x[1],x[2], x[2],x[1]), nrow=2, byrow=TRUE)
  } else if (1 == length(x)) {
    matrix(x[1], nrow=1, ncol=1)
  } else if (0 < length(x)) {
    # Why does sapply or simplify2array not bind the sub-matrices correctly?
    # Ah well, do it ourselves.
    sub <- lapply(1:length(x),
                  function(i) {
                    sub <- permute.all(x[-i])
                    cbind(rep(x[i], nrow(sub)), sub)
                  })
    perm <- sub[[1]]
    for (i in 2:length(x)) {
      perm <- rbind(perm, sub[[i]])
    }
    perm
  } else {
    stop("no permutations possible for 0 length vector")
  }
}
