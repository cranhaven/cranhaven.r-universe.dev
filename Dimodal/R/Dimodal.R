
# Dimodal.R:
# Use spacing to detect modality, with local peaks corresponding to changes
# and flats to modes.  Detect and evaluate features in spacing after low-pass
# filtering with height or length or aspect ratio models or excursion/
# bootstrap simulations, in interval spacing as excursions or in statistics
# of signed runs or longest or permutation simulation, or in raw spacing via
# changepoints.
#
# c 2024-2025 Greg Kreider, Primordial Machine Vision Systems, Inc.

## To Do:
#



#### Public Interface

# Perform a modality analysis on data vector x using low-pass spacing if 
# Diopt('analysis') includes 'lp', interval spacing with 'diw', and
# changepoints/raw spacing with 'cpt'; more than one method can be chosen.
# opt sets the # Diopt analysis parameters.  The return value is a list of
# class 'Dimodal' with elements added per the analyze.* functions.
Dimodal <- function(x, opt=Diopt()) {

  if ("Diopt" != class(opt)) {
    opt <- Diopt(opt)
  }

  if (get("._TIMEIT", envir=._timer.env)) { setup.timing() }

  res <- list(data=gen.data(x, opt$analysis, opt), opt=opt)
  attributes(res$data) <- c(attributes(res$data),
                            list(data.name=deparse1(substitute(x))))

  if ("lp" %in% opt$analysis) {
    res <- c(res, analyze.lp(res$data, res$opt))
  }
  if ("diw" %in% opt$analysis) {
    res <- c(res, analyze.diw(res$data, res$opt))
  }
  if ("cpt" %in% opt$analysis) {
    res <- c(res, analyze.cpt(res$data, res$opt))
  }

  class(res) <- "Dimodal"
  res
}


#### Implementation

# Perform feature detection and significance determination on the low-pass
# spacing stored in gen.data matrix d, using Diopt options opt.  Returns a
# list with elements
#   $lp.peaks      local extrema (position, height, signficance)
#   $lp.flats      local flats (position, significance)
# The tests run and added to the results depend on those stored in the
# lp.tests option.  Adds columns $p[peak|flat] and $naccept for a summary of
# each result.
analyze.lp <- function(d, opt) {

  if (is.null(d) || (0 == nrow(d)) || !("lp" %in% rownames(d))) {
    return(list(lp.peaks=mockup.Dipeak(), lp.flats=mockup.Diflat()))
  }

  if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_LP") }

  # Local overrides.
  # It seems that sapply(opt$lp.param, function(p) { ... }) strips the
  # element name from p when passing to the function, so we must
  # address by position along the list.  But we don't have to screen for NULL.
  for (i in seq_along(opt$lp.param)) {
    opt[[names(opt$lp.param)[i]]] <- opt$lp.param[[i]]
  }

  a <- attributes(d)
  lpdi <- d["lp",a$lp.stID:a$lp.endID]
  # The model tests want the fractional window, not the absolute a$wlp.
  fwlp <- a$lp.window
  if (1 <= fwlp) {
    # - 1 b/c spacing not raw data.
    fwlp <- fwlp / (ncol(d) - 1)
  }

  if (("ht" %in% opt$lp.tests) || ("pkexcur" %in% opt$lp.tests)) {
    if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_LPPEAK") }
    pk <- find.peaks(lpdi, opt$peak.fht, opt$peak.frelht, opt$peak.fhtie,
                     opt$peak.fhsupp)
    pk <- shiftID.place(pk, a$lp.stID, d["xmid",], 0)
    hts <- pmax(pk$lht, pk$rht)
    nsig <- rep(0L, nrow(pk))
    if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_LPPEAK") }

    if ("ht" %in% opt$lp.tests) {
      if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_LPPKHT") }
      # -1 because length is spacing.
      tst <- Dipeak.test(hts, ncol(d)-1, fwlp, a$lp.kernel, FALSE)
      pht <- tst$p.value
      pk <- cbind(pk, pht=pht)
      if (0 < length(pht)) {
        nsig <- nsig + as.integer(is.finite(pht) & (pht <= opt$alpha.ht))
      }
      if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_LPPKHT") }
    } else {
      pht <- rep(NA, nrow(pk))
    }

    if ("pkexcur" %in% opt$lp.tests) {
      if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_LPPKEXCUR") }
      b <- build.excursion(d, "lp", pk$lsuppID, pk$rsuppID, opt$excur.ntop)
      tst <- Diexcurht.test(b$ht, b$ndraw, b$xbase, opt$excur.nrep, TRUE,FALSE,
                            13*opt$excur.seed)
      pexcur <- tst$p.value
      pk <- cbind(pk, hexcur=tst$statistic, pexcur=pexcur)
      if (0 < length(pexcur)) {
        nsig <- nsig + as.integer(is.finite(pexcur) &
                                  (pexcur <= opt$alpha.pkexcur.lp))
      }
      if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_LPPKEXCUR") }
    } else {
      pexcur <- rep(NA, nrow(pk))
    }

    pk <- cbind(pk, ppeak=pmin(pht, pexcur, na.rm=TRUE), naccept=nsig)
    # The cbind's don't preserve the class, so re-insert.
    if (!("Dipeak" %in% class(pk))) {
      class(pk) <- c("Dipeak", class(pk))
    }
  } else {
    pk <- mockup.Dipeak()
  }

  if (("len" %in% opt$lp.tests) || ("ftexcur" %in% opt$lp.tests)) {
    if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_LPFLAT") }
    ft <- find.flats(lpdi, opt$flat.fripple, opt$flat.minlen, opt$flat.fminlen,
                     opt$flat.noutlier)
    ft <- shiftID.place(ft, a$lp.stID, d["xmid",], 0)
    nsig <- rep(0L, nrow(ft))
    if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_LPFLAT") }

    if ("len" %in% opt$lp.tests) {
      if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_LPFTLEN") }
      # -1 because length is spacing.
      tst <- Diflat.test(ft$len, ncol(d)-1, fwlp, a$lp.kernel,
                         opt$flat.distrib, FALSE)
      plen = tst$p.value
      ft <- cbind(ft, plen=plen)
      if (0 < length(plen)) {
        nsig <- nsig + as.integer(is.finite(plen) & (plen <= opt$alpha.len))
      }
      if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_LPFTLEN") }
    } else {
      plen <- rep(NA, nrow(ft))
    }

    if ("ftexcur" %in% opt$lp.tests) {
      if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_LPFTEXCUR") }
      b <- build.excursion(d, "lp", ft$stID, ft$endID, opt$excur.ntop)
      tst <- Diexcurht.test(b$ht, b$ndraw, b$xbase, opt$excur.nrep, FALSE,TRUE,
                            17*opt$excur.seed)
      fexcur <- tst$p.value
      ft <- cbind(ft, hexcur=tst$statistic, pexcur=fexcur)
      if (0 < length(fexcur)) {
        nsig <- nsig + as.integer(is.finite(fexcur) &
                                  (fexcur <= opt$alpha.ftexcur.lp))
      }
      if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_LPFTEXCUR") }
    } else {
      fexcur <- rep(NA, nrow(ft))
    }

    ft <- cbind(ft, pflat=pmin(plen, fexcur, na.rm=TRUE), naccept=nsig)
    if (!("Diflat" %in% class(ft))) {
      class(ft) <- c("Diflat", class(ft))
    }
  } else {
    ft <- mockup.Diflat()
  }

  attr(pk, "source") <- "LP"
  attr(ft, "source") <- "LP"
  
  if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_LP") }

  return(list(lp.peaks=pk, lp.flats=ft))
}

# Perform feature detection and significance determination on the interval
# spacing stored in gen.data matrix d, using Diopt options opt.  Returns a
# list with elements
#   $diw.peaks     local extrema (position, height, significance)
#   $diw.flats     local flats (position, significance)
# The tests run and added to the results depend on those stored in the
# diw.tests option.  Adds columns $p[peak|flat] and $naccept for a summary of
# each result.
analyze.diw <- function(d, opt) {

  if (is.null(d) || (0 == nrow(d)) || !("Diw" %in% rownames(d))) {
    return(list(diw.peaks=mockup.Dipeak(), diw.flats=mockup.Diflat()))
  }

  if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_DIW") }

  for (i in seq_along(opt$diw.param)) {
    opt[[names(opt$diw.param)[[i]]]] <- opt$diw.param[[i]]
  }

  a <- attributes(d)
  diw <- d["Diw",a$diw.stID:ncol(d)]

  if (("pkexcur" %in% opt$diw.tests) || ("runht" %in% opt$diw.tests) ||
      ("nrun" %in% opt$diw.tests) || ("maxrun" %in% opt$diw.tests)) {
    if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_DIWPEAK") }
    pk <- find.peaks(diw, opt$peak.fht, opt$peak.frelht, opt$peak.fhtie,
                     opt$peak.fhsupp)
    pk <- shiftID.place(pk, a$diw.stID, d["xmid",], -floor(a$wdiw/2))
    nsig <- rep(0L, nrow(pk))
    if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_DIWPEAK") }
                       
    if ("nrun" %in% opt$diw.tests) {
      if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_DIWNRUN") }
      # + 1 because the signed difference starts one point later.
      # No feps to the test because values are via sign.
      tst <- Dinrun.test(d["signed",], pk$lminID+1, pk$rminID, 0, TRUE)
      pkr <- tst$p.value
      pk <- cbind(pk, nrun=tst$statistic, pnrun=pkr)
      if (0 < length(pkr)) {
        nsig <- nsig + as.integer(is.finite(pkr) & (pkr <= opt$alpha.nrun))
      }
      if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_DIWNRUN") }
    } else {
      pkr <- rep(NA, nrow(pk))
    }

    if ("runlen" %in% opt$diw.tests) {
      if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_DIWRUNLEN") }
      # + 1 because the signed difference starts one point later.
      # 0 for feps because of the limited values from sign.
      tst <- Dirunlen.test(d["signed",], pk$lminID+1, pk$rminID, 0)
      prl <- tst$p.value
      pk <- cbind(pk, runlen=tst$statistic, prunlen=prl)
      if (0 < length(prl)) {
        nsig <- nsig + as.integer(is.finite(prl) & (prl <= opt$alpha.runlen))
      }
      if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_DIWRUNLEN") }
    } else {
      prl <- rep(NA, nrow(pk))
    }

    if ("runht" %in% opt$diw.tests) {
      if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_DIWRUNHT") }
      b <- build.permutation(d, pk$lminID, pk$rminID)
      tst <- sapply(seq_along(b$ht),
                    function(i) {
                      # Only one permutation test so no need to scale seed.
                      res <- Dipermht.test(b$ht[i], b$xbase[[i]],
                                           opt$perm.nrep, FALSE, opt$perm.seed)
                      c(ht=res$statistic, pval=res$p.value)
                    })
      if (0 == length(b$ht)) {
        pperm <- NULL
      } else {
        pperm <- tst["pval",]
        pk <- cbind(pk, runht=tst["ht",], prunht=pperm)
        if (0 < length(pperm)) {
          nsig <- nsig + as.integer(is.finite(pperm) &
                                    (pperm <= opt$alpha.runht))
        }
      }
      if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_DIWRUNHT") }
    } else {
      pperm <- rep(NA, nrow(pk))
    }

    if ("pkexcur" %in% opt$diw.tests) {
      if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_DIWPKEXCUR") }
      b <- build.excursion(d, "Diw", pk$lsuppID, pk$rsuppID, opt$excur.ntop)
      tst <- Diexcurht.test(b$ht, b$ndraw, b$xbase, opt$excur.nrep, TRUE,FALSE,
                            19*opt$excur.seed)
      pexcur <- tst$p.value
      pk <- cbind(pk, hexcur=tst$statistic, pexcur=pexcur)
      if (0 < length(pexcur)) {
        nsig <- nsig + as.integer(is.finite(pexcur) &
                                  (pexcur <= opt$alpha.pkexcur.diw))
      }
      if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_DIWPKEXCUR") }
    } else {
      pexcur <- rep(NA, nrow(pk))
    }

    pk <- cbind(pk, ppeak=pmin(pkr, prl, pperm, pexcur, na.rm=TRUE),
                naccept=nsig)
    # The cbind's don't preserve the class, so re-insert.
    if (!("Dipeak" %in% class(pk))) {
      class(pk) <- c("Dipeak", class(pk))
    }
  } else {
    pk <- mockup.Dipeak()
  }

  if ("ftexcur" %in% opt$diw.tests) {
    if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_DIWFLAT") }
    ft <- find.flats(diw, opt$flat.fripple, opt$flat.minlen, opt$flat.fminlen,
                     opt$flat.noutlier)
    ft <- shiftID.place(ft, a$diw.stID, d["xmid",], -floor(a$wdiw/2))
    nsig <- rep(0L, nrow(ft))
    if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_DIWFLAT") }
    
    if ("ftexcur" %in% opt$diw.tests) {
      if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_DIWFTEXCUR") }
      b <- build.excursion(d, "Diw", ft$stID, ft$endID, opt$excur.ntop)
      tst <- Diexcurht.test(b$ht, b$ndraw, b$xbase, opt$excur.nrep, FALSE,TRUE,
                            23*opt$excur.seed)
      fexcur <- tst$p.value
      ft <- cbind(ft, hexcur=tst$statistic, pexcur=fexcur)
      if (0 < length(fexcur)) {
        nsig <- nsig + as.integer(is.finite(fexcur) &
                                  (fexcur <= opt$alpha.ftexcur.diw))
      }
      if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_DIWFTEXCUR") }
    } else {
      fexcur <- rep(NA, nrow(ft))
    }

    ft <- cbind(ft, pflat=pmin(fexcur, na.rm=TRUE), naccept=nsig)
    if (!("Diflat" %in% class(ft))) {
      class(ft) <- c("Diflat", class(ft))
    }
  } else {
    ft <- mockup.Diflat()
  }

  attr(pk, "source") <- "Diw"
  attr(ft, "source") <- "Diw"

  if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_DIW") }
  
  return(list(diw.peaks=pk, diw.flats=ft))
}

# Run all changepoint detectors available on the system on the spacing in
# Didata d, using the $cpt.libs specification in the Diopt options list opt
# to modify the choice.  Combine the results into a master list of
# changepoints using majority voting.  Returns a list with element
#   $cpt      per-library and common changepoints
# where the contents of cpt are specified in identify.cpt() and merge.cpt().
analyze.cpt <- function(d, opt) {

  if (is.null(d) || (0 == nrow(d)) || !("Di" %in% rownames(d))) {
    return(list(cpt=mockup.Dicpt()))
  }

  if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_CPT") }

  cpt <- find.cpt(d["Di",-1L],
                  opt$cpt.libs, opt$cpt.fncpt.max, opt$cpt.timeout,
                  opt$cpt.qvote, opt$cpt.sep, opt$cpt.fsep, opt$cpt.libsep)

  cpt$cpt <- shiftID.place(cpt$cpt, 2, d["xmid",], 0)
  attr(cpt, "source") <- "Di"
  
  if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_CPT") }
  
  list(cpt=cpt)
}


### Data generation

# Build all data for the analysis from data x and Diopt list opt for the
# analysis type(s) in method.  Non-finite values are automatically dropped.
# Returns a matrix of length(x) with rows:
#   'x'        raw data
#   'xsort'    sorted data
#   'xmid'     mid-distribution function or piece-wise linear CDF b/t jumps
#   'Di'       spacing, lead column (index 1) is NA
# If method includes 'lp' adds
#   'lp'       low-pass spacing, lead/trailing columns will be NA
# If method includes 'diw' adds
#   'Diw'      interval spacing, leading columns will be NA
#   'signed'   signed difference of Diw, one more leading column NA
# If wdiw is the actual interval size (not fractional per Diopt), then the
# first wdiw columns in the Diw row are NA, and the first wdiw+1 in signed.
# If wlp is the actual low-pass kernel size, then the first wlp/2 columns
# of lp are NA, rounded up, and the last wlp/2 columns rounded down are NA.
# If method includes 'lp' the matrix has attributes added:
#   'lp.kernel', 'lp.window' copied from opt
#   'wlp'      actual size of kernel (integer)
#   'lp.stID'  first column with non-NA data in lp row
#   'lp.endID' last column with non-NA data in lp row
# If method includes 'diw' the matrix has attributes added:
#   'diw.window' copied from opt
#   'wdiw'     actual interval (integer)
#   'diw.stID' first column with non-NA data in Diw row, = wdiw+1
#              interval data always goes up to last column
gen.data <- function(x, method, opt) {

  # The low-pass filter has a long run time on large data sets: 0.6 s for 50k
  # points, 66 s for 500k, 262 s for 1M, ie. O(n^2).  We added the method
  # argument to avoid running this on such sets if not needed.

  if (!is.numeric(x)) {
    stop("original data must be numeric")
  }
  x <- x[is.finite(x)]
  nx <- length(x)
  if (nx < 2) {
    warning("not generating spacing - too little data")
  }

  if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_XSORT") }
  
  xsrt <- sort(x)

  if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_XSORT") }
  if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_XMID") }

  xmid <- midquantile(xsrt, type=opt$data.midq)

  if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_XMID") }
  if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_XDI") }

  di <- c(NA, diff(xsrt))

  if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_XDI") }

  mdi <- matrix(c(x, xsrt, xmid, di), nrow=4, byrow=TRUE,
                dimnames=list(c("x", "xsort", "xmid", "Di"), NULL))

  miw <- NULL
  aiw <- NULL
  if ("diw" %in% method) {
    if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_XDIW") }
    
    if (opt$diw.window < 1.0) {
      wdiw <- round(opt$diw.window * nx)
    } else {
      wdiw <- round(opt$diw.window)
    }
    if (wdiw < 1) {
      warning("not generating interval spacing - interval too small")
    } else if (nx <= (wdiw + 1)) {
      warning("not generating interval spacing - interval does not fit data")
    } else {
      diw <- c(rep(NA, wdiw), diff(xsrt, lag=wdiw))

      # There can be problems with machine accuracy here, see data test case 7.
      # Clip all very small differences to 0 before taking the sign.  The 
      # factor of 10 is very loose, only 4 would be needed for the test case.
      ddiw <- diff(diw)
      ddiw[abs(ddiw) < (10 * .Machine$double.eps)] <- 0
      sdiw <- c(NA, sign(ddiw))

      miw <- matrix(c(diw, sdiw), nrow=2, byrow=TRUE,
                    dimnames=list(c("Diw", "signed"), NULL))
      aiw <- list(diw.window=opt$diw.window, wdiw=wdiw, diw.stID=wdiw+1)
    }
    
    if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_XDIW") }
  }

  mlp <- NULL
  alp <- NULL
  if ("lp" %in% method) {
    if (get("._TIMEIT", envir=._timer.env)) { start.timer("TIMER_XLP") }
    
    kernel <- fir.kernel(di[-1L], opt)
    wlp <- length(kernel)
    if (wlp < 3) {
      warning("not generating low-pass spacing - filter kernel too small")
    } else if (nx < (wlp + 1)) {
      warning("not generating low-pass spacing - not enough data for kernel")
    } else {
      u <- filter(di, rev(kernel), method="convolution", sides=2)

      stID <- 1 + ((wlp + 1) %/% 2)
      endID <- nx - (wlp - stID) - 1
    
      mlp <- matrix(u, nrow=1, byrow=TRUE, dimnames=list(c("lp"), NULL))
      alp <- list(lp.kernel=opt$lp.kernel, lp.window=opt$lp.window,
                wlp=wlp, lp.stID=stID, lp.endID=endID)
    }
    
    if (get("._TIMEIT", envir=._timer.env)) {  stop.timer("TIMER_XLP") }
  }

  m <- rbind(mdi, mlp, miw)
  attributes(m) <- c(attributes(m), alp, aiw)
  class(m) <- c("Didata", class(m))

  m
}

# Return the kernel for the low-pass filter specified in Diopt opt.  The
# data x is needed to determine the window size if a fraction.
fir.kernel <- function(x, opt) {

  if (opt$lp.window < 1.0) {
    wlp <- round(opt$lp.window * length(x))
  } else {
    wlp <- round(opt$lp.window)
  }

  if (wlp < 3) {
    return(NULL)
  }

  filter <- match.arg(tolower(opt$lp.kernel), fir.names)

  # Filter definitions in Discrete-Time Signal Porcessing ch 7.4 have 0
  # in first and last position - except Hamming and Kaiser - so we've
  # effectively increased the window width by 2 and then deleted these.

  if (("triangular" == filter) || ("bartlett" == filter)) {
    wlp <- wlp + 1
    kernel <- sapply(1:(wlp-1),
                     function(i) { ifelse(i<(wlp/2), 2*i/wlp, 2-(2*i/wlp)) })
  } else if ("kaiser" == filter) {
    beta <- fir.kaiser.beta(0.01)
    alpha <- (wlp - 1) / 2
    kernel <- besselI(beta * sqrt(1 - ((((0:(wlp-1)) - alpha)/alpha)^2)), 0) /
              besselI(beta, 0)
  } else if (("gaussian" == filter) || ("normal" == filter)) {
    # Per Klette (before eq. (2.18)) a three sigma drop-off means the kernel
    # width is 6*sigma - 1, ie. s=1 => diameter 5/radius 2, s=2 => d 11/r 5.
    sigmasq <- 2 * ((wlp + 1) / 6)^2
    wseq <- (1:wlp) - (wlp / 2)
    kernel <- exp(-(((wlp/2) - (1:wlp))^2)/sigmasq) / sqrt(sigmasq * pi)
  } else {
    if ("hamming" == filter) {
      f <- pi * (0:(wlp-1)) / (wlp - 1)
    } else if (("hanning" == filter) || ("blackman" == filter)) {
      f <- pi * (1:wlp) / (wlp + 1)
    } else {
      stop("internal error - uncaught unsupported filter")
    }
    a <- fir.coscoef(filter)
    kernel <- rep(a[1], length(f))
    for (i in 2:length(a)) {
      kernel <- kernel + (a[i] * cos(2*(i-1)*f))
    }
  }

  if (abs(sum(kernel)) < 1e-6) {
    kernel
  } else {
    kernel / sum(kernel)
  }
}

# a is a vector of coefficients for the cosine-based FIR filter, or the
# name of the filter.  If the latter, return the corresponding vector; if
# the former, just return it.  Filters that don't use cosine terms cause
# an error.
fir.coscoef <- function(a) {

  if (is.numeric(a)) {
    a
  } else {
    filter <- match.arg(a, fir.names)
    if ("hanning" == filter) {
      c(0.5, -0.5)
    } else if ("hamming" == filter) {
      # Discrete-Time Signal Processing gives c(0.54, -0.46).  Via Harris
      # window filters page this is an approx of c(25/46, -21/46), which
      # elimates the first sidelobe.  Another option is c(0.53836, -0.46164)
      # which is equiripple.
      # c(0.54, -0.46)
      # c(0.53836, -0.46164)
      c(25/46, -21/46)
    } else if ("blackman" == filter) {
      # Harris values are exact, Discrete-Time SP uses the approximation
      # c(0.42, -0.5, 0.08).
      c(7938/18608, -9240/18608, 1430/18608)
    } else {
      stop(sprintf("unsupported filter '%s'", a))
    }
  }
}
  
# Return the Kaiser beta parameter for a stopband fripple (as a fraction).
fir.kaiser.beta <- function(fripple) {

  A <- -20 * log10(fripple)
  if (A < 21) {
    beta <- 0
  } else if (50 < A) {
    beta <- 0.1102 * (A - 8.7)
  } else {
    beta <- (0.5842 * ((A - 21) ^ 0.4)) + (0.07886 * (A - 21))
  }
  beta
}

# Construct the information needed for an excursion test on the row in
# Didata d.  Features extend between stID and endID (may be vectors) and
# the base of the excursion draws will be the difference of the data,
# ignoring the ntop largest changes if they are within the first or last
# ntop/2 data.  Returns a list with elements
#   $ht       the feature height
#   $ndraw    the feature width, or number of samples per excursion draw
#   $xbase    a vector with the differential signal to build excursion signals
# The height is taken over the endpoints, and therefore reflects the support
# if they are used.
build.excursion <- function(d, row, stID, endID, ntop) {

  if (length(stID) != length(endID)) {
    stop("internal error - feature bounds do not have same length")
  }

  if ("lp" == row) {
    xbase <- diff(d[row,attr(d,"lp.stID"):attr(d,"lp.endID")])
    xoff <- attr(d,"lp.stID") - 1
  } else if ("Diw" == row) {
    xbase <- diff(d[row,attr(d,"diw.stID"):ncol(d)])
    xoff <- attr(d,"diw.stID") - 1
  } else {
    xbase <- diff(d[row,])
    xbase <- xbase[is.finite(xbase)]
    xoff <- 0
  }
  
  ntop <- min(round(0.1 * length(xbase)), ntop)
  if (0 < ntop) {
    o <- order(abs(diff(xbase)), decreasing=TRUE)
    drop <- o[1:ntop]
    ldrop <- drop[drop <= (ntop/2)]
    # + 1 to compensate for the diff, deleting the upper point.
    # But not for the left drop to delete the lower point.
    rdrop <- drop[(length(o)-(ntop/2)+1) <= drop] + 1
    if ((0 < length(ldrop)) || (0 < length(rdrop))) {
      xbase <- xbase[-c(ldrop, rdrop)]
    }

    # Need to move endpoints out of dropped region.
    if (0 < length(ldrop)) {
      # +1 so endpoint after largest ldrop.
      stID <- pmax(stID, max(ldrop)+xoff+1)
      endID <- pmax(endID, max(ldrop)+xoff+1)
    }
    if (0 < length(rdrop)) {
      # -1 so endpoint is smaller than smallest rdrop.
      stID <- pmin(stID, min(rdrop)+xoff-1)
      endID <- pmin(endID, min(rdrop)+xoff-1)
    }
  }

  hts <- sapply(seq_along(stID),
                function(i) {
                  if (!is.finite(stID[i]) || !is.finite(endID[i])) {
                    NA_real_
                  } else {
                    diff(range(d[row,stID[i]:endID[i]]))
                  }
                })

  # Normally would add + 1, but the excursion is based on the diff.
  ndraw <- endID - stID

  list(ht=hts, ndraw=ndraw, xbase=xbase)
}

# Construct the information needed for a permutation test on the runs in
# Didata d for features with endpoints stID, endID (may be vectors).  The
# base data for the draw will be the delta along each run, ie. its length
# times its value (with 0 values mapped to 1).  Returns a list with elements
#   $ht       the feature height
#   $xbase    a list of the run deltas for each feature
build.permutation <- function(d, stID, endID) {

  if (length(stID) != length(endID)) {
    stop("internal error - feature bounds do not have the same length")
  }

  # We work in the signed row, so starting indices are offset by 1.
  stID <- stID + 1

  xsign <- lapply(seq_along(stID),
                 function(i) {
                   if (!is.finite(stID[i]) || !is.finite(endID[i])) {
                     NULL
                   } else {
                     d["signed",stID[i]:endID[i]]
                   }
                 })

  hts <- sapply(seq_along(stID),
                function(i) {
                  if (!is.finite(stID[i]) || !is.finite(endID[i])) {
                    NA_real_
                  } else {
                    diff(range(cumsum(xsign[[i]])))
                  }
                })

  xbase <- lapply(seq_along(stID),
                  function(i) {
                    if (!is.finite(stID[i]) || !is.finite(endID[i])) {
                      NULL
                    } else {
                      r <- rle(xsign[[i]])
                      # Note that we lose the length when the value is 0.
                      # This is OK for the permutation test as a placeholder
                      # but would not work for the runs tests.
                      r$lengths * r$values
                    }
                  })

  list(ht=hts, xbase=xbase)
}


#### Timing Analysis

# Undocumented performance tracking of the main steps in Dimodal.
# Creates a set of clocks TIMER_* for the main data preparation, low-pass
# spacing, interval spacing, and changepoint steps.  All functions must
# be called as Dimodal:::<fn> since they are not exported.  Use track.timing()
# to start or stop the overall tracking, clear.timing() to reset the clocks,
# dump.timing() to print tables of each step averaged over all calls to
# Dimodal() since the last clearing, and get.timers() to return the times
# in a matrix.  start.timer() and stop.timer() are used internally around
# each analysis step.

# Usage Guide:
#   track.timing() -         Track or stop performance tracking of Dimodal()
#   setup.timing() -         Prepares for timing if never done before,
#                            otherwise adds an entry for a new run.
#   clear.timing() -         Removes all history of timing and sets up for a
#                            new run.
#   dump.timing()  -         Print a table with the average timing (in ms)
#                            over however many runs have been timed.  Will
#                            only print the subset of the analysis if parts
#                            are skipped (ex. no low-pass).
#   get.timers()   -         Return a a matrix with the timing, columns are
#                            timer names (see setup.timing) and one row per run
#   start.timer("TIMER_*") - Register the clock time (well, elapsed process
#                            time) for the indicated timer.  A list of timers
#                            is found in setup.timing.
#   stop.timer("TIMER_*")  - Stores the elapsed time spent in the indicated
#                            timer for the current run.

# As with Diopt, we need a new environment because globals get locked.
if (!exists("._timer.env")) {
  ._timer.env <- new.env(hash=FALSE)
  assign("._TIMEIT", FALSE, envir=._timer.env)
}

# Allow timing analysis of Dimodal steps if onoff is TRUE, or stop it if FALSE.
track.timing <- function(onoff) {
  assign("._TIMEIT", onoff, envir=._timer.env)
}

# Prepare for timing Dimodal.  Modifies the ._timer.env global with the
# variables needed for timing Dimodal, including creating the TIMER_*
# identifiers for [start|stop].timer.
setup.timing <- function() {

  # Timers for low-pass spacing analysis.
  assign("TIMER_LP",         1L, envir=._timer.env)
  assign("TIMER_LPPEAK",     2L, envir=._timer.env)
  assign("TIMER_LPPKHT",     3L, envir=._timer.env)
  assign("TIMER_LPPKEXCUR",  4L, envir=._timer.env)
  assign("TIMER_LPFLAT",     5L, envir=._timer.env)
  assign("TIMER_LPFTLEN",    6L, envir=._timer.env)
  assign("TIMER_LPFTEXCUR",  7L, envir=._timer.env)

  # Timers for interval spacing analysis.
  assign("TIMER_DIW",        8L, envir=._timer.env)
  assign("TIMER_DIWPEAK",    9L, envir=._timer.env)
  assign("TIMER_DIWNRUN",    10L, envir=._timer.env)
  assign("TIMER_DIWRUNLEN",  11L, envir=._timer.env)
  assign("TIMER_DIWRUNHT",   12L, envir=._timer.env)
  assign("TIMER_DIWPKEXCUR", 13L, envir=._timer.env)
  assign("TIMER_DIWFLAT",    14L, envir=._timer.env)
  assign("TIMER_DIWFTEXCUR", 15L, envir=._timer.env)

  # Timers for changepoint analysis.
  assign("TIMER_CPT",   16L, envir=._timer.env)

  # Timers for data preparation.
  assign("TIMER_XSORT", 17L, envir=._timer.env)
  assign("TIMER_XMID",  18L, envir=._timer.env)
  assign("TIMER_XDI",   19L, envir=._timer.env)
  assign("TIMER_XDIW",  20L, envir=._timer.env)
  assign("TIMER_XLP",   21L, envir=._timer.env)

  # The timings matrix contains the time spent for each column, a matrix with
  # #(TIMER_*) columns and one row per timing run.
  if (!exists("timings", envir=._timer.env, inherits=FALSE)) {
    assign("timings", matrix(rep(NA_real_, 21), nrow=1), envir=._timer.env)
  } else {
    assign("timings",
           rbind(get("timings", envir=._timer.env),
                 matrix(rep(NA_real_, 21), nrow=1)),
           envir=._timer.env)
  }

  # The tst vector contains the start time for each timer.
  assign("tst", rep(NA_real_, 21), envir=._timer.env)
}

# Reset all timers.  Deletes any history, then re-creates the setup.
# Modifies the ._timer.env contents.
clear.timing <- function() {

  rm(list=ls(._timer.env), envir=._timer.env)
  setup.timing()
}

# Print a table with the average of all timers.
dump.timing <- function() {

  dt <- get("timings", envir=._timer.env)
  avgdt <- apply(dt, 2, mean, na.rm=TRUE)
  ntime <- apply(dt, 1, function(r) { !all(is.na(r)) })
  if (0 == sum(ntime)) {
    cat("\nTiming Analysis\n")
    cat("  none performed\n\n")
    return()
  } else {
    cat("\nTiming Analysis   from ", sum(ntime), " runs, times in [ms]\n")
  }

  dataID <- c("TIMER_XSORT", "TIMER_XDI", "TIMER_XDIW", "TIMER_XLP",
              "TIMER_XMID")
  datacol <- sapply(dataID, get, envir=._timer.env)
  have.data <- !all(is.na(avgdt[datacol]))
  xnames <- c("sort", "spacing", "interval spacing", "low-pass filtering",
              "mid-quantile")

  lpID <- c("TIMER_LP", "TIMER_LPPEAK", "TIMER_LPPKHT", "TIMER_LPPKEXCUR",
            "TIMER_LPFLAT", "TIMER_LPFTLEN", "TIMER_LPFTEXCUR")
  lpcol <- sapply(lpID, get, envir=._timer.env)
  have.lp <- !all(is.na(avgdt[lpcol]))
  lpnames <- c("analysis", "peak detection", "peak height test",
               "peak excursion test", "flat detection", "flat length test",
               "flat excursion test")

  diwID <- c("TIMER_DIW", "TIMER_DIWPEAK", "TIMER_DIWNRUN", 
             "TIMER_DIWRUNLEN", "TIMER_DIWRUNHT", "TIMER_DIWPKEXCUR",
             "TIMER_DIWFLAT", "TIMER_DIWFTEXCUR")
  diwcol <- sapply(diwID, get, envir=._timer.env)
  have.diw <- !all(is.na(avgdt[diwcol]))
  diwnames <- c("analysis", "peak detection", "run count test",
                "run length test", "run height (perm) test",
                "peak excursion test", "flat detection", "flat excursion test")

  cptID <- c("TIMER_CPT")
  cptcol <- sapply(cptID, get, envir=._timer.env)
  have.cpt <- !all(is.na(avgdt[cptcol]))
  cptnames <- c("analysis")

  wdt <- ceiling(log10(max(dt, na.rm=TRUE)))
  wname <- max(nchar(xnames[!is.na(avgdt[datacol])]),
               nchar(lpnames[!is.na(avgdt[lpcol])]),
               nchar(diwnames[!is.na(avgdt[diwcol])]),
               nchar(cptnames[!is.na(avgdt[cptcol])]))

  if (have.data) {
    cat("  Data Preparation\n")
    for (i in seq_along(datacol)) {
      if (!is.na(avgdt[datacol[i]])) {
        cat(sprintf("    %-*s   %*.0f\n",
                    wname, xnames[i], wdt, avgdt[datacol[i]]))
      }
    }
  }
  
  if (have.lp) {
    cat("  Low-Pass Spacing\n")
    for (i in seq_along(lpcol)) {
      if (!is.na(avgdt[lpcol[i]])) {
        cat(sprintf("    %-*s   %*.0f\n",
                    wname, lpnames[i], wdt, avgdt[lpcol[i]]))
      }
    }
  }
  
  if (have.diw) {
    cat("  Interval Spacing\n")
    for (i in seq_along(diwcol)) {
      if (!is.na(avgdt[diwcol[i]])) {
        cat(sprintf("    %-*s   %*.0f\n",
                    wname, diwnames[i], wdt, avgdt[diwcol[i]]))
      }
    }
  }
  
  if (have.cpt) {
    cat("  Changepoints\n")
    for (i in seq_along(cptcol)) {
      if (!is.na(avgdt[cptcol[i]])) {
        cat(sprintf("    %-*s   %*.0f\n",
                    wname, cptnames[i], wdt, avgdt[cptcol[i]]))
      }
    }
  }

  cat("\n")
}

# Return the timing since the last clear.  This is a matrix with TIMER_*
# in columns and one row per Dimodal run.
get.timers <- function() {

  t <- get("timings", envir=._timer.env)
  # Strip out any incomplete placeholder rows.
  t <- t[!apply(t, 1, function(x) { all(is.na(x)) }),]
  # Add timer names to matrix.
  nm <- ls(envir=._timer.env)[grepl("TIMER", ls(envir=._timer.env), fixed=TRUE)]
  nm <- nm[order(sapply(nm, get, envir=._timer.env))]
  colnames(t) <- nm
  
  t
}

# Register the start time for one of the TIMER_* identifiers tid registered
# in setup.timing.  Modifies the ._timer.env contents.
start.timer <- function(tid) {

  tst <- get("tst", envir=._timer.env)
  tid <- get(tid, envir=._timer.env)
  tst[tid] <- proc.time()['elapsed']
  assign("tst", tst, envir=._timer.env)
}

# Store the timing in ms of the TIMER_* identifier tid listed in setup.timing
# in the history.  Modifies the ._timer.env contents.
# If start.timer has never been called for this tid, the registered time will
# be NA.
stop.timer <- function(tid) {

  tend <- proc.time()['elapsed']
  tst <- get('tst', envir=._timer.env)
  tid <- get(tid, envir=._timer.env)
  dt <- get('timings', envir=._timer.env)
  dt[nrow(dt),tid] <- (tend - tst[tid]) * 1000
  assign('timings', dt, envir=._timer.env)
}
