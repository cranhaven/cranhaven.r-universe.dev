
# cpt.R:
# Use external changepoint libraries to detect, well, changes in the spacing.
# This is set up to use what is available on a system.  Use majority voting
# to select the common points from all algorithms run.
#
# c 2025 Greg Kreider, Primordial Machine Vision Systems, Inc.

## To Do:
# - 


##### Public Interface

# Run changepoints detectors on data vector x, then combine them with majority
# voting into a final set of changepoints.  Use the changepoint libraries
# available on the system, modified by the cptlibs spec.  Impose maximum run
# time timeout (sec, 0 for none) on each library call.  Automatically ignore
# libraries selecting more than the fraction fncpt.max of the data as
# changepoints.  Points from libraries are considered the same if their indices
# are within absolute distance sep or relative fsep (as fraction of data).
# Points within library variants use libsep during initial merge.
# Libraries are ignored if their changepoint count is outside the quantile
# pair qvote.  Returns a list of class Dicpt with elements
#   $test       data frame with cols 'library', 'detector', 'abbrev', 'ncpt'
#   $rawpts     list of lists of points
#   $rawparam   list of argument values
#   $libpts     list of changepoints per library, element name of library
#   $voting     boolean vector of length libpts, if library used in voting
#   $cpt        data frame with cpt per row, cols 'pos', 'x', 'nvote', 'val'
#   $cptparam   list of voting/merging arguments
# The test matrix will have one row per combination of library and detector,
# and may have more than one row per library.  The points list will have one
# element (unnamed) per row, a list of the detector's points.  These are
# stored in a list as the length will differ.
find.cpt <- function(x, cptlibs, fncpt.max, timeout, qvote, sep, fsep, libsep) {

  libres <- identify.cpt(x, cptlibs, fncpt.max, timeout)
  merge.cpt(libres, x, qvote, sep, fsep, libsep)
}


##### Implementation

# Run the available changepoint detectors modified by the cptlibs spec
# (see list.cptlibs) on Didata x.  Impose timout in sec (0 for none) on each
# algorithm.  Automatically reject points lists that are more than the
# fraction maxfncpt of the data.  Returns a list of class Dicpt with elements
# $test, $rawpts, and $rawparam as per find.cpt.
identify.cpt <- function(x, cptlibs, maxfncpt, timeout) {

  libs <- list.cptlibs(cptlibs)
  ncpt <- ceiling(maxfncpt * length(x))

  cpts <- lapply(libs,
                 function(lname) {
                   list.cpt.runfn(lname)(x, ncpt, timeout)
                 })
  cpts <- unlist(cpts, recursive=FALSE)
  rawpts <- lapply(cpts, getElement, "pts")

  test <- data.frame(library=sapply(cpts, getElement, "lib"),
                     detector=sapply(cpts, getElement, "detector"),
                     abbrev=sapply(cpts, getElement, "abbrev"),
                     ncpt=sapply(rawpts, length))
  rawparam <- list(cptlibs=cptlibs, maxfncpt=maxfncpt, ncpt=ncpt,
                   timeout=timeout, nx=length(x))

  errmsg <- linebreak(unlist(lapply(cpts, getElement, "msg")),
                             0.8*getOption("width"))
  if (0 < length(errmsg) && (0 < max(nchar(errmsg)))) {
    errmsg <- paste0(errmsg, "\n")
    warning(c("Messages raised while identifying changepoints:\n", errmsg),
            call.=FALSE)
  }

  res <- list(test=test, rawpts=rawpts, rawparam=rawparam)
  class(res) <- "Dicpt"
  res
}

# Break each line in s, per newlines inside, into strings no longer
# than w characters, preserving the whitespace at the start of the start of
# each line for each broken line.  Returns a vector of character strings.
# Empty elements in s are dropped.
linebreak <- function(s, w) {

  if (0 < length(s)) {
    s <- strsplit(s, "\n")
  }
  
  sbreak <- lapply(s,
                   function(l) {
                     if (0 == length(l)) {
                       l
                     } else if (w < nchar(l)) {
                       nlead <- nchar(l) - nchar(trimws(l, "left"))
                       strwrap(l, width=w-nlead, indent=nlead, exdent=nlead)
                     } else {
                       l
                     }
                   })
  unlist(sbreak)
}

# Combine the individual changepoint detectors in cpt from data x into one
# set of points.  First points from any individual methods within the library
# are merged into one list, then the library results are merged.  Points in
# either step are clustered in absolute distance sep (points) or as a fraction
# fsep of the data length in d and collapsed to the center of mass.  Libraries
# are ignored if the number of points is outside the quantile pair qvote of
# all results.  Points in the final list are kept if they appear in a majority
# of the remaining libraries.  Adds elements $libpts, $voting, $cpt, and
# $cptparam to cpt, per find.cpt.
merge.cpt <- function(cpt, x, qvote, sep, fsep, libsep) {

  near <- min(round(length(x) * fsep), sep)

  if (0 == nrow(cpt$test)) {
    cpt$libpts <- list()
    cpt$voting <- logical()
    cpt$cpt <- data.frame(pos=NULL, val=NULL, nvote=NULL)
    cpt$cptparam <- list(libsep=libsep, sep=sep, fsep=fsep, near=near,
                         qvote=qvote, qpts=c(NA,NA), minvote=0)
    return(cpt)
  }

  libs <- sort(unique(cpt$test[,"library"]))
  libpts <- lapply(libs,
                   function(l) {
                     unlist(sapply(which(l == cpt$test[,"library"]),
                                   function(i) { cpt$rawpts[[i]] })) })

  libpts <- lapply(libpts, rm.nearby, libsep)
  names(libpts) <- libs
  libpts

  npts <- sapply(libpts, length)
  if ((length(libs) <= 5) || (sum(0 < npts) <= 5)) {
    qpts <- pmax(1, range(npts))
    libkeep <- 0 < npts
  } else {
    # Do not interpolate the quantile, with rounding to an integer this can
    # go the wrong way.
    qpts <- pmax(1, round(quantile(npts, qvote, names=FALSE, type=4)))
    libkeep <- (qpts[1] <= npts) & (npts <= qpts[2])
    # Guarantee at least some activity - more than the endpoints.
    while (sum(libkeep) < 5) {
      qpts <- qpts + c(-1, 1)
      libkeep <- (qpts[1] <= npts) & (npts <= qpts[2])
    }
  }

  nkeep <- sum(libkeep)
  if (0 == nkeep) {
    pts <- matrix(NA, ncol=3, nrow=0)
    minvote <- 0
  } else {
    minvote <- max(nkeep/2, 1)
    allpts <- unlist(lapply(which(libkeep), function(i) { libpts[[i]] }) )
    pts <- merge.nearby(allpts, near)
    vote <- sapply(pts,
                   function(p) {
                     sum( sapply(which(libkeep),
                                 function(i) {
                                   ifelse(any(abs(p-libpts[[i]]) <= near), 1,0)
                                 }) ) })
    vkeep <- minvote <= vote
    if (0 < length(pts)) {
      pts <- data.frame(pos=pts[vkeep], val=x[pts[vkeep]], nvote=vote[vkeep])
    } else {
      pts <- data.frame(pos=NULL, val=NULL, nvote=NULL)
    }
  }

  cpt$libpts <- libpts
  cpt$voting <- libs[libkeep]
  cpt$cpt <- pts
  cpt$cptparam <- list(libsep=libsep, sep=sep, fsep=fsep, near=near,
                       qvote=qvote, qpts=qpts, minvote=minvote)

  cpt
}

# Return a list of the names of changepoint libraries available on the system.
# The libspec is a vector of names or abbreviations preceded by '+' or '-'.
# If any +name strings exist, then they form the base list, as long as they are
# present on the system, otherwise the function probes a list it knows about/
# supports.  -name strings are then deleted from this list.  Name comparisons
# are done case-insensitive.  Several known libraries are not supported and
# ignored with a warning.  If at least three libraries are selected the
# Dimodal detector level.sections is added to the list, unless excluded with
# '-level.sections' or '-lvlsec' or an abbreviation of the two.
#   ex. list.cptlibs(c('-astsa', '-ecp'))
#         returns the available libraries (incl. level.sections) except these
#   ex. list.cptlibs(c('+changepoint.np', '+icss', '+jointseg'))
#         recommended minimum list
list.cptlibs <- function(libspec) {

  # Other libraries not included in master list:
  #   npcp          - only finds if changepoint occurs, not where, not multiple
  #   FDRSeg        - no longer supported, very slow, smuce segfaults
  #   BayesProject  - multivariate only
  #   fpop          - sensitive to lambda parameter with no guidence on value
  #   capushe       - can't figure out how to set up and use
  #   prophet       - requires dummy time index, raises error on help page ex.
  #   gSeg          - must sequentially, manually subdivide data, not clear how
  #                   not clear how to create graph to match
  #   modehunt      - use level section test
  cptlibs.excl <- c("npcp", "FDRSeg", "BayesProject", "fpop", "capushe",
                    "prophet", "gSeg", "modehunt")

  cptlibs <- c("anomaly", "astsa", "bcp", "breakfast", "bwd", "ccid",
               "changepoint", "changepoint.np", "cpm", "cpss", "ecp", "ICSS",
               "jointseg", "mosum", "ocp", "otsad", "Rbeast", "strucchange")

  # Per R-exts.pdf this is the way to check and use suggested packages.
  cptlibs <- cptlibs[sapply(cptlibs, requireNamespace, quietly=TRUE)]
  if (length(cptlibs) < 3) {
    warning("few changepoint libaries found, suggest changepoint.np, jointseg, and ICSS")
  }

  if (0 == length(libspec)) {
    libs <- cptlibs
    addlibs <- NULL
    dellibs <- NULL
  } else {
    libspec <- tolower(libspec)
    char1 <- substr(libspec, 1, 1)
    libspec <- substr(libspec, 2, max(nchar(libspec)))
    addlibs <- libspec["+"==char1]
    dellibs <- libspec["-"==char1]
  }

  excl <- NULL
  if (0 == length(addlibs)) {
    libs <- cptlibs
  } else {
    sel <- pmatch(addlibs, tolower(cptlibs))
    sel <- sel[!is.na(sel)]
    libs <- cptlibs[sel]
    excl <- c(excl, pmatch(addlibs, tolower(cptlibs.excl)))
  }

  if (0 < length(dellibs)) {
    drop <- pmatch(dellibs, tolower(libs))
    drop <- drop[!is.na(drop)]
    if (0 < length(drop)) {
      libs <- libs[-drop]
    }
  }
  
  excl <- excl[!is.na(excl)]
  if (1 == length(excl)) {
    warning(paste0("ignoring unsupported CPT library ", cptlibs.excl[excl]))
  } else if (1 < length(excl)) {
    warning(paste0("ignoring unsupported CPT libraries ", cptlibs.excl[excl]))
  }

  if (0 == length(libs)) {
    warning("no changepoint libraries available after screening per cpt.libs")
  }
  if (3 <= length(libs)) {
    if (all(is.na(pmatch(dellibs, c("lvlsec", "level.sections"))))) {
      libs <- c(libs, "level.sections")
    }
  }
  
  sort(libs)
}

# Return the function used to run changepoint detectors in library libname
# (single string).
list.cpt.runfn <- function(libname) {

  switch(libname,
         anomaly=run.anomaly,
         astsa=run.astsa,
         bcp=run.bcp,
         breakfast=run.breakfast,
         bwd=run.bwd,
         ccid=run.ccid,
         changepoint=run.cpt,
         changepoint.np=run.npcpt,
         cpm=run.cpm,
         cpss=run.cpss,
         ecp=run.ecp,
         ICSS=run.icss,
         jointseg=run.jointseg,
         level.sections=run.lvlsec,
         mosum=run.mosum,
         ocp=run.ocp,
         otsad=run.otsad,
         Rbeast=run.rbeast,
         strucchange=run.strucchange,
         run.dummycpt)
}

# Evaluate expr and return the result as element $pts, or NULL if there are
# too many (> ncpt) points, or no points, or if the evaluation takes longer
# than timeout seconds.  The timeout check may not affect non-R code in the
# libraries.  Use fnname as an identifier string for the library function.
# Any conditions (error, warning, timeout interrupts, other) are captured
# and returned in the list as element $msg; any other output or errors not
# going through the condition system are swallowed.
run.cptfn <- function(expr, ncpt, timeout, fnname) {

  # Just how defensive do we want to get?  A typo debugging at the command
  # line passed NULL for timeout, which blew up the check before setTimeLimit.
  # But normally this would raise an error, just here, after setting up the
  # sink, does it cause a problem, so paranoia here is OK.
  if (0 == length(ncpt)) {
    stop("internal error - no ncpt value")
  } else {
    ncpt <- ncpt[1]
  }
  if (0 == length(timeout)) {
    stop("internal error - no timeout value")
  } else {
    timeout <- timeout[1]
  }

  # Something doesn't play nice with the tryCatch approach when the timeout
  # occurs outside R.  An example is the cpss SN algorithm.  It calls
  # checkUserInterrupt() which has hard-wired behavior in R/src/main/errors.c;
  # per comments if options(interrupt) is not defined then it uses
  # options(error).  This seems not be true - providing an interrupt function
  # does not stop the error call.  The printing of an error message is
  # hard-wired, it seems.  BTW, this might explain the conversion of a timeout
  # to an interrupt that the tryCatch does handle.
  # Overwriting options(error) to an empty function does squelch the stack
  # trace but keeps the hard-wired error message.  We choose to simply eat any
  # output, redirecting it to a local variable.  This has the advantage of
  # swallowing output from the libraries (looking at you, Rbeast, with your
  # extra newline).  The messages from a signaled condition are captured in
  # errmsg for return.
  dummy <- ""
  errmsg <- ""
  closeAllConnections()
  fcon <- textConnection("dummy", open="a", local=TRUE)
  sink(fcon, append=TRUE, type="output")
  sink(fcon, append=TRUE, type="message")
  ferr <- textConnection("errmsg", open="a", local=TRUE)

  on.exit({ if (0 < timeout) {
              setTimeLimit(cpu=Inf, elapsed=Inf, transient=FALSE) ;
            } ;
            closeAllConnections()
          })

  if (0 < timeout) {
    setTimeLimit(cpu=timeout, elapsed=timeout, transient=TRUE)
  }

  pts <- tryCatch({ eval(expr) },
                  condition=function(e) { cptfn.condmsg(ferr, e, fnname) })

  npt <- length(pts)

  if ((0 == npt) || ((1 == npt) && is.na(pts))) {
    pts <- NULL
  } else if (ncpt < npt) {
    pts <- NULL
    cat(paste0("    Ignoring ", fnname, " - too many points\n"), file=ferr)
  }
  
  list(pts=pts, msg=rm.timeout(errmsg))
}

# Handle the condition e, saving a subset of its message to connection ferr.
# fnname is an identifier for the function being executed.  Returns NULL
# representing an empty points list.
cptfn.condmsg <- function(ferr, e, fnname) {

  if (!is.null(conditionMessage(e))) {
    msg <- strsplit(conditionMessage(e), "\n", fixed=TRUE)[[1]]
    if (1 < length(msg)) {
      msg <- msg[2L]
    }
    msg <- trimws(msg)
  } else {
    msg <- ""
  }

  if ("interrupt" %in% class(e)) {
    txt <- "Interrupt raised in "
    if ("" == msg) {
      msg <- "(timeout)"
    }
  } else {    
    if ("error" %in% class(e)) {
      txt <- "Error running "
    } else if ("warning" %in% class(e)) {
      txt <- "Warning from "
    } else {
      txt <- "Message from "
    }
  }
  
  # Extra spaces in messages so everything lines up in identify.cpt when done.
  if (any(0 < nchar(msg))) {
    cat(paste0("    ", txt, fnname, " -\n      " , msg, "\n"), file=ferr)
  }

  # This is actually the empty points list.
  NULL
}

# Simulate a (failed) library call, returning a list with elements $pts and
# msg as in run.cptfn, both NULL.
dummy.cptfn <- function() {

  list(pts=NULL, msg=NULL)
}

# The run.<library> functions perform the changepoint analysis for each
# library.  They search vector x for changepoints, ignoring results with
# more than ncpt points (assumed to come from a noisy detector).  The
# analysis will be terminated after timeout seconds (if 0, no limit).  Each
# function returns a list with, per detector/method within the library, a
# sub-list with elements
#   $lib        the name of the changepoint library
#   $detector   a method or analysis parameter within the library
#   $abbrev     a short name for the detector
#   $pts        the points returned by the detector, possibly after post-proc
#   $msg        any messages from the detector captured while it ran
run.anomaly <- function(x, ncpt, timeout) {

  # API changed, handle both cases.
  # The lambda here is the default, and setting the value squelches a warning.
  if ("transform" %in% names(formals(anomaly::pass))) {
    # Do this manually.  The first standardization is robustscale from the
    # old version of the anomaly library.  Testing uncovered cases where
    # it generates NaN and Inf values.  scale fails if all values are the
    # same.  The anomaly routines fail if x is integer, so convert as a
    # last-ditch case.
    xscl <- (x - median(x)) / mad(x)
    if (any(!is.finite(xscl))) {
      xscl <- scale(x)
      if (any(!is.finite(xscl))) {
        xscl <- as.double(x)
      }
    }
    m <- matrix(xscl, ncol=1)
    ppts <- run.cptfn({ process.anomaly(
                          anomaly::pass(m, lambda=10, transform=identity),
                          FALSE) },
                      ncpt, timeout, "anomaly/pass")
    cpts <- run.cptfn({ process.anomaly(
                          anomaly::capa(m, type="meanvar", transform=identity),
                          TRUE) },
                      ncpt, timeout, "anomaly/capa")
  } else {
    m <- matrix(x, ncol=1)
    ppts <- run.cptfn({ process.anomaly(
                          anomaly::pass(m, lambda=10), FALSE) },
                      ncpt, timeout, "anomaly/pass")
    cpts <- run.cptfn({ process.anomaly(
                          anomaly::capa(m, type="meanvar"), TRUE) },
                      ncpt, timeout, "anomaly/capa")
  }

  list(c(list(lib="anomaly", detector="pass", abbrev="pass"), ppts),
       c(list(lib="anomaly", detector="capa.uv", abbrev="capa"), cpts))
}

# Process the anomaly detector results res into a list of points.  If has.point
# is TRUE the detector provides points in addition to intervals.
process.anomaly <- function(res, has.point) {

  # In development had done more processing of the bands, combining those
  # that overlapped or that were close (endpoints w/i 10 of each other) and
  # shrinking small segments (<= 5) to a point.  Let the rm.nearby at the
  # library level take care of this.
  seg <- anomaly::collective_anomalies(res)
  if (0 == nrow(seg)) {
    pts <- NULL
  } else {
    pts <- c(seg$start, seg$end)
  }

  if (has.point) {
    pts <- c(pts, anomaly::point_anomalies(res)$location)
  }

  if (!is.null(pts)) {
    sort(unique(pts))
  } else {
    pts
  }
}

run.astsa <- function(x, ncpt, timeout) {

  # Very slow!  per Rprof ~ 50% of time spent in ar, 50% in autoParm code.
  pts <- run.cptfn({ astsa::autoParm(x)$breakpoints },
                   ncpt, timeout, "astsa")
  list(c(list(lib="astsa", detector="autoParm", abbrev="autoparm"), pts))
}

run.bcp <- function(x, ncpt, timeout) {

  # Too few data points causes a segfault.
  if (length(x) <= 3) {
    bcp <- dummy.cptfn()
  } else {
    # No recommendation on cut-off in papers about this library.  Above 0.85
    # the number of changepoints stabilizes in many test sets and is better
    # than at 0.50.  0.90 and 0.95 are generally close in count, so taking
    # the more relaxed.
    bcp <- run.cptfn({ which(bcp::bcp(x)$posterior.prob >= 0.90) },
                     ncpt, timeout, "bcp")
  }
  list(c(list(lib="bcp", detector="bcp", abbrev="bcp"), bcp))
}

run.breakfast <- function(x, ncpt, timeout) {

  # If wanted to run process.breakfast outside run.cptfn, would have to do
  #   idet['pts'] <- process.breakfast(idet$pts)
  # and have it return list(NULL) if there are no points to avoid removing
  # the element.  This wouldn't work for anomaly because the processing
  # must create the points list.
  idet <- run.cptfn({ process.breakfast(
                        breakfast::breakfast(x, solution.path="idetect_seq")) },
                    ncpt, timeout, "breakfast/idetect_seq")
  tguh <- run.cptfn({ process.breakfast(
                        breakfast::breakfast(x, solution.path="tguh")) },
                    ncpt, timeout, "breakfast/tguh")
  wbs2 <- run.cptfn({ process.breakfast(
                        breakfast::breakfast(x, solution.path="wbs2")) },
                    ncpt, timeout, "breakfast/wbs2")
  
  list(c(list(lib="breakfast", detector="idetect_seq", abbrev="idet"), idet),
       c(list(lib="breakfast", detector="tguh", abbrev="tguh"), tguh),
       c(list(lib="breakfast", detector="wbs2", abbrev="wbs2"), wbs2))
}

# Extract the changepoints from the breakfast result res.
process.breakfast <- function(res) {

  # In development had used band.cpt to remove the smallest segments.
  # Changing to return all segment endpoints as the changepoints, trusting
  # the library merge to do the simplification.  This does make breakfast a
  # noisy detector, and it can generate too many points (vs. ncpt).
  cpt <- res$cptmodel.list[[1]]$cpts
  if (is.null(cpt) || (0 == length(cpt))) {
    NULL
  } else {
    # Had tried to reduce very small width segments to 0 here, equivalent
    # to slim.bands, but that had little effect.
    sort(unique( cpt ))
  }
}

run.bwd <- function(x, ncpt, timeout) {

  pts <- run.cptfn({ bwd::bwd(x)$segments[[1]][,2] },
                   ncpt, timeout, "bwd")
  if (1 <= length(pts$pts)) {
    pts$pts <- c(1, pts$pts)
  }
  list(c(list(lib="bwd", detector="bwd", abbrev="bwd"), pts))
}

run.ccid <- function(x, ncpt, timeout) {

  pts <- run.cptfn({ ccid::detect.ic(matrix(x, ncol=1))$changepoints },
                   ncpt, timeout, "ccid")
  list(c(list(lib="ccid", detector="detect.ic", abbrev="ic"), pts))
}

run.cpt <- function(x, ncpt, timeout) {

  # We do not support testing just mean or variance.
  sic <- run.cptfn({ changepoint::cpt.meanvar(x, method="PELT", penalty="SIC", Q=ncpt)@cpts },
                   ncpt, timeout, "changepoint/SIC")
  bic <- run.cptfn({ changepoint::cpt.meanvar(x, method="PELT", penalty="BIC", Q=ncpt)@cpts },
                   ncpt, timeout, "changepoint/BIC")
  aic <- run.cptfn({ changepoint::cpt.meanvar(x, method="PELT", penalty="AIC", Q=ncpt)@cpts },
                   ncpt, timeout, "changepoint/AIC")
  mbic <- run.cptfn({ changepoint::cpt.meanvar(x, method="PELT", penalty="MBIC", Q=ncpt)@cpts },
                   ncpt, timeout, "changepoint/MBIC")

  list(c(list(lib="changepoint", detector="PELT-SIC", abbrev="SIC"), sic),
       c(list(lib="changepoint", detector="PELT-BIC", abbrev="BIC"), bic),
       c(list(lib="changepoint", detector="PELT-AIC", abbrev="AIC"), aic),
       c(list(lib="changepoint", detector="PELT-MBIC", abbrev="MBIC"), mbic))
}

run.npcpt <- function(x, ncpt, timeout) {

  sic <- run.cptfn({ changepoint.np::cpt.np(x, method="PELT", penalty="SIC")@cpts },
                   ncpt, timeout, "changepoint.np/SIC")
  bic <- run.cptfn({ changepoint.np::cpt.np(x, method="PELT", penalty="BIC")@cpts },
                   ncpt, timeout, "changepoint.np/BIC")
  aic <- run.cptfn({ changepoint.np::cpt.np(x, method="PELT", penalty="AIC")@cpts },
                   ncpt, timeout, "changepoint.np/AIC")
  mbic <- run.cptfn({ changepoint.np::cpt.np(x, method="PELT", penalty="MBIC")@cpts },
                    ncpt, timeout, "changepoint.np/MBIC")

  list(c(list(lib="changepoint.np", detector="npPELT-SIC", abbrev="npSIC"), sic),
       c(list(lib="changepoint.np", detector="npPELT-BIC", abbrev="npBIC"), bic),
       c(list(lib="changepoint.np", detector="npPELT-AIC", abbrev="npAIC"), aic),
       c(list(lib="changepoint.np", detector="npPELT-MBIC", abbrev="npMBIC"), mbic))
}

run.cpm <- function(x, ncpt, timeout) {

  # These tests are for mean and variance.
  mw <- run.cptfn({ cpm::processStream(x, "Mann-Whitney")$changePoints },
                   ncpt, timeout, "cpm/MW")
  md <- run.cptfn({ cpm::processStream(x, "Mood")$changePoints },
                   ncpt, timeout, "cpm/Md")
  lp <- run.cptfn({ cpm::processStream(x, "Lepage")$changePoints },
                   ncpt, timeout, "cpm/Lp")
  ks <- run.cptfn({ cpm::processStream(x, "Kolmogorov-Smirnov")$changePoints },
                   ncpt, timeout, "cpm/KS")
  cvm <- run.cptfn({ cpm::processStream(x, "Cramer-von-Mises")$changePoints },
                   ncpt, timeout, "cpm/CvM")

  list(c(list(lib="cpm", detector="Mann-Whitney", abbrev="MW"), mw),
       c(list(lib="cpm", detector="Mood", abbrev="Md"), md),
       c(list(lib="cpm", detector="Lepage", abbrev="Lp"), lp),
       c(list(lib="cpm", detector="Kolmogorov-Smirnov", abbrev="KS"), ks),
       c(list(lib="cpm", detector="Cramer-vonMises", abbrev="CvM"), cvm))
}

run.cpss <- function(x, ncpt, timeout) {

  bs <- run.cptfn({ cpss::cpss.meanvar(x, "BS")@cps },
                  ncpt, timeout, "cpss/BS")
  sn <- run.cptfn({ cpss::cpss.meanvar(x, "SN")@cps },
                  ncpt, timeout, "cpss/SN")
  wbs <- run.cptfn({ cpss::cpss.meanvar(x, "WBS")@cps },
                   ncpt, timeout, "cpss/WBS")

  list(c(list(lib="cpss", detector="BS", abbrev="BS"), bs),
       c(list(lib="cpss", detector="SN", abbrev="SN"), sn),
       c(list(lib="cpss", detector="WBS", abbrev="WBS"), wbs))
}

run.ecp <- function(x, ncpt, timeout) {

  # e.cp3o, ks.cp3o left out because they segfault.  kcpa left out because
  # it's even slower than e.divisive, which is very slow.
  pts <- run.cptfn({ ecp::e.divisive(matrix(x, ncol=1))$estimates },
                   ncpt, timeout, "ecp")
  list(c(list(lib="ecp", detector="e.divisive", abbrev="ediv"), pts))
}

run.icss <- function(x, ncpt, timeout) {

  icss <- run.cptfn({ suppressWarnings(ICSS::ICSS(x)) }, ncpt, timeout, "ICSS")

  list(c(list(lib="ICSS", detector="ICSS", abbrev="ICSS"), icss))
}

run.jointseg <- function(x, ncpt, timeout) {

  rbs <- run.cptfn({ jointseg::jointSeg(x, method="RBS", K=ncpt)$bestBkp },
                   ncpt, timeout, "jointSeg/RBS")
  gfl <- run.cptfn({ jointseg::jointSeg(x, method="GFLars", K=ncpt)$bestBkp },
                   ncpt, timeout, "joingSeg/GFLars")
  list(c(list(lib="jointseg", detector="RBS", abbrev="RBS"), rbs),
       c(list(lib="jointseg", detector="GFLars", abbrev="GFLars"), gfl))
}

run.lvlsec <- function(x, ncpt, timeout) {

  # We're running the changepoint detector on the spacing, but the level
  # section algorithm assumes it gets the raw data, so regenerate that.
  xcum <- cumsum(x)
  pts <- run.cptfn({ process.lvlsec( find.level.sections(xcum, 0.95, TRUE) ) },
                   ncpt, timeout, "level.sections")
  list(c(list(lib="Dimodal", detector="level.sections", abbrev="lvlsec"), pts))
}

# Combine the stID and endID columns in pts into a unique vector.
process.lvlsec <- function(pts) {

  sort(unique( c(pts$stID, pts$endID) ))
}

run.mosum <- function(x, ncpt, timeout) {

  pts <- run.cptfn({ mosum::multiscale.bottomUp(as.numeric(x))$cpts },
                   ncpt, timeout, "mosum")
  list(c(list(lib="mosum", detector="multiscale.bottomUp", abbrev="mscale"), pts))
}

run.ocp <- function(x, ncpt, timeout) {

  pts <- run.cptfn({ ocp::onlineCPD(x)$changepoint_lists$maxCP[[1]] },
                   ncpt, timeout, "ocp")
  list(c(list(lib="ocp", detector="onlineCPD", abbrev="ocp"), pts))
}

run.otsad <- function(x, ncpt, timeout) {

  pewma <- run.cptfn({ which(otsad::CpPewma(x)$is.anomaly == 1) },
                     ncpt, timeout, "otsad/pewma")
  sdewma <- run.cptfn({ which(otsad::CpSdEwma(x, 19)$is.anomaly == 1) },
                      ncpt, timeout, "otsad/sdewma")
  tsewma <- run.cptfn({ which(otsad::CpTsSdEwma(x, 19)$is.anomaly == 1) },
                      ncpt, timeout, "otsad/tsewma")
  knn <- run.cptfn({ which(otsad::CpKnnCad(x, 47, l=19,k=27)$is.anomaly == 1) },
                       ncpt, timeout, "otsad/knn")
  # Trigger-happy.  Pewma seems the  best of SdEwma, TsSdEwma, KnnCad.
  list(c(list(lib="otsad", detector="CpPewma", abbrev="pewma"), pewma),
       c(list(lib="otsad", detector="CpSdEwma", abbrev="sdewma"), sdewma),
       c(list(lib="otsad", detector="CpTsSdEwma", abbrev="tsewma"), tsewma),
       c(list(lib="otsad", detector="CpKnnCad", abbrev="knn"), knn))
}

run.rbeast <- function(x, ncpt, timeout) {

  pts <- run.cptfn({ process.rbeast( Rbeast::beast(x, season="none", quiet=TRUE, print.options=FALSE, print.progress=FALSE)) },
                   ncpt, timeout, "Rbeast")
  list(c(list(lib="Rbeast", detector="beast", abbrev="beast"), pts))
}

# Extract the changepoints from beast result res.
process.rbeast <- function(res) {

  res$trend$res[1:res$trend$ncp_mode]
}

run.strucchange <- function(x, ncpt, timeout) {

  xdf <- data.frame(x=x)
  lm <- run.cptfn({ strucchange::breakpoints(x~1, data=xdf, h=0.05)$breakpoints },
                  ncpt, timeout, "strucchange/lm")
  f <- run.cptfn({ strucchange::breakpoints(strucchange::Fstats(x~1, data=xdf), h=0.05)$breakpoints },
                  ncpt, timeout, "strucchange/Fstats")

  list(c(list(lib="strucchange", detector="lm", abbrev="lm"), lm),
       c(list(lib="strucchange", detector="Fstats", abbrev="Fstat"), f))
}

run.dummycpt <- function(x, ncpt, timeout) {

  NULL
}


# Replace points in x that are within sep (incl.) of each other by their
# average, rounding if round is TRUE.  This chains before averaging.
rm.nearby <- function(x, sep, round=TRUE) {

  x <- as.integer(sort(x))
  sep <- as.integer(sep)
  dx <- diff(x)
  midpt <- NULL
  drop <- NULL
  i <- 1
  while (i <= length(dx)) {
    if (dx[i] <= sep) {
      j <- i
      while ((j <= length(dx)) && (dx[j] <= sep)) {
        j <- j + 1
      }
      # dx is offset from x by 1, so as an x index we have j-1.
      sel <- i:j
      pt <- mean(x[sel])
      # Note rounding of *.5 is towards the even digit.
      if (round) {
        pt <- round(pt)
      }
      midpt <- c(midpt, pt)
      drop <- c(drop, sel)
      i <- j
    }
    i <- i + 1
  }
  
  if (!is.null(drop)) {
    x <- x[-drop]
  }
  as.integer(sort(unique( c(x, midpt) )))
}

# Merge the points in indices x within sep.  This does not chain like
# rm.nearby.  The merged points are those values over the range of x with
# the most x nearby, ie. local maxima.  The merged points are combined in
# a last step with rm.nearby.  Returns the final merged set.
merge.nearby <- function(x, sep) {

  if (0 == length(x)) {
    return(double())
  }
  x <- sort(x)
  if (!all(is.integer(x))) {
    stop("x converted back to double")
  }
  xrng <- range(x)

  # For each possible value we count the number of points in x within sep.
  i <- 1 ; k <- 1
  j <- xrng[1]
  cnt <- rep(0, xrng[2])
  for (j in xrng[1]:xrng[2]) {
    while ((i < length(x)) && (sep < (j - x[i]))) { i <- i + 1 }
    while ((k < length(x)) && ((x[k+1] - j) <= sep)) { k <- k + 1 }
    cnt[j] <- k - i + 1
    j <- j + 1
  }

  # Then look for all local maxima in the count.  This is where the sign
  # of the slope (dir) changes, ignoring constant values (edges).  Place
  # the maximum in the middle of the mesa.
  dir <- c(0, sign(diff(cnt)))
  sel <- which(dir != 0)
  edges <- dir[sel] * sel
  locmaxID <- sapply(which((edges[-length(edges)] > 0) & (edges[-1L] < 0)),
                     function(i) { round(mean(edges[i]:(-edges[i+1]-1))) })

  # Combine nearby maxima.
  if (0 < length(locmaxID)) {
    rm.nearby(locmaxID, sep)
  } else {
    locmaxID
  }
}

# Remove the timeout error from the messages msg generated in run.cptfn,
# keeping only our notice via the interrupt.
rm.timeout <- function(msg) {

  errline <- which(grepl("reached", msg, fixed=TRUE) &
                   grepl("time limit", msg, fixed=TRUE))
  if ((2 == length(errline)) &&
      !grepl("Interrupt raised in", msg[errline[1]-1], fixed=TRUE) &&
       grepl("Interrupt raised in", msg[errline[2]-1], fixed=TRUE)) {
    msg <- msg[-((errline[1]-1):(errline[2]-2))]
  }
  msg
}

# Print a table with the changepoints in cpt per library and detector.
dump.detectcpt <- function(cpt) {

  wlib <- max(nchar(cpt$test[,"library"]))
  wdet <- max(nchar(cpt$test[,"detector"]))

  cat("\n")
  for (i in 1:nrow(cpt$test)) {
    cat(sprintf("  %-*s  %-*s : ",
                wlib, cpt$test[i,"library"], wdet, cpt$test[i,"detector"]),
        cpt$rawpts[[i]], "\n")
  }
  cat("\n")
  
}
