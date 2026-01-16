
# Diopt:
# Gathering of analysis and test parameters for the Dimodal package.  Modeled
# on the par() options storage.
#
# c 2024-2025 Greg Kreider, Primordial Machine Vision Systems, Inc.

## To Do:
# - 
#


##### Public Interface

# Diopt()
#   returns current db
# Diopt(NULL)
#   returns current db, resets db to default
# Diopt(tag1=val1, tag2=val2, ...)
#   changes db with valid values, returns old values
# Diopt(list(tag1=val1, tag2=val2, ...))
#   changes db with values valids, returns old values
# Diopt("tag1", "tag2", ...)
#   returns current db of tags
# Diopt.local(tag1=val1, tag2=val2, ...)
#   returns current db overridden by valid values, no change to db
# Diopt.local(list(tag1=val1, tag2=val2, ...))
#   returns current db overridden by valid values, no change to db


# Access or change the analysis and test options.  If called without arguments
# return a list of all current values.  If called with NULL as the argument,
# reset all values to their default.  If called with strings, return the
# values for those tags, or NULL if the tag does not exist.  If called
# with tag=value pairs, or such pairs wrapped in an unnamed list, change
# the option and return the old value.  Can mix strings and pairs.
# Diopt(Diopt(tag1=val1, tag2=val2)) will restore the original state.
# Maintains the default option values as global state, but the library will
# use a local copy during an analysis.
Diopt <- function(...) {

  args <- list(...)
  if (0 == length(args)) {
    get("diopt", envir=diopt.env)
  } else if ((1 == length(args)) && is.null(args[[1]])) {
    opt <- get("diopt", envir=diopt.env)
    assign("diopt", diopt.dflt, envir=diopt.env)
    opt
  } else {
    if ((1 == length(args)) && is.list(args[[1]]) &&
        (is.null(names(args)[1]) || ("" == names(args)[1]))) {
      args <- args[[1]]
    }

    vals <- lapply(seq_along(args),
                   function(i) {
                     tag <- names(args)[i]
                     val <- args[[i]]
                     opt <- get("diopt", envir=diopt.env)
                     if (is.null(tag) || ("" == tag)) {
                       # If assign a name to this, then the returned list
                       # element is still unnamed.  Must handle separately.
                       opt[[val]]
                     } else if (!(tag %in% names(opt))) {
                       warning(paste0("ignoring invalid Diopt key ", tag))
                       NULL
                     } else {
                       val <- validate.diopt(tag, val)
                       if (is.null(val)) {
                         NULL
                       } else {
                         old <- opt[[tag]]
                         opt[[tag]] <- val
                         assign("diopt", opt, envir=diopt.env)
                         old
                       }
                     }
                   })
    names(vals) <- lapply(seq_along(args),
                          function(i) {
                            tag <- names(args)[i]
                            if (is.null(tag) || ("" == tag)) {
                              args[[i]]
                            } else {
                              tag
                            }
                          })
    vals
  }
}

# Return a list with all current options, overridden by the tag=value pairs
# in the argument.  These pairs may be wrapped in a single unnamed list.
Diopt.local <- function(...) {

  args <- list(...)
  if ((1 == length(args)) && is.list(args[[1]]) &&
      (is.null(names(args)[1]) || ("" == names(args)[1]))) {
    args <- args[[1]]
  }
  
  opt <- get("diopt", envir=diopt.env)
  for (i in seq_along(args)) {
    tag <- names(args)[i]
    val <- args[[i]]
    val <- validate.diopt(tag, val)
    # Do not allow adding extra tags.
    if (!(tag %in% names(opt))) {
      warning(paste0("ignoring invalid Diopt key ", tag))
    } else if (!is.null(val)) {
      opt[[tag]] <- val
    }
  }
  opt
}


##### Internal Implementation

# Default option values.
diopt.dflt <-
  list(
    # Analyses
    analysis=c("lp", "diw", "cpt"),
                               # which spacing/smoothing to check (cpt is Di)
    # Data prep
    data.midq=0,               # mid-quantile approximation method
    # Low-pass Filter Setup
    lp.kernel="kaiser",        # filter kernel
    lp.window=0.15,            # kernel size as fraction of data or integer
    lp.tests=c("ht", "pkexcur", "len", "ftexcur"),
                               # default tests on low-pass features
    lp.param=NULL,             # overrides of detector/test parameters
    # Interval Spacing Setup
    diw.window=0.10,           # interval width as fraction of data or integer
    diw.tests=c("pkexcur", "runht", "nrun", "runlen", "ftexcur"),
                               # default tests on interval features
    diw.param=NULL,            # overrides of detector/test parameters
    # Local Extrema Detector
    peak.fht=0.05,             # min peak ht as fraction of data range
    peak.frelht=0.15,          # peak ht as fraction of local range
    peak.fhtie=0.001,          # max relative difference for tied points
    peak.fhsupp=0.9,           # fraction peak height for support (1 min-min)
    # Local Flat Detector
    flat.fripple=0.05,         # ripple specification
    flat.minlen=30,            # min absolute length of flat
    flat.fminlen=0.05,         # min flat length as fraction of data
    flat.noutlier=1,           # number of outliers (outside ripple) w/i flat
    flat.distrib="logistic",   # null distribution model
    # Excursion/Permutation/Runs Tests
    excur.nrep=15000,          # number sample trials peak/flat excursion tests
    excur.ntop=8,              # num large spacings at start/end to skip
    excur.seed=0,              # excursion RNG seed before test, 0 to not set
    perm.nrep=5000,            # number sample trials for the runht test
    perm.seed=0,               # permutation RNG seed before test, 0 to not set
    # Test Significance (Acceptance) Levels
    alpha.ht=0.01,             # peak height model significance level
    alpha.pkexcur.lp=0.05,     # low-pass peak excursion significance level
    alpha.pkexcur.diw=0.05,    # Diw peak excursion significance level
    alpha.len=0.05,            # flat length model significance level
    alpha.ftexcur.lp=0.01,     # low-pass flat excursion significance level
    alpha.ftexcur.diw=0.01,    # Diw flat excursion significance level
    alpha.runht=0.005,         # runs permutation significance level
    alpha.nrun=0.01,           # runs statistics significance level
    alpha.runlen=0.01,         # longest run significance level
    # Changepoint Detectors
    cpt.libs=c("-astsa", "-ecp"),
                               # library [in|ex]clusion list
    cpt.fncpt.max=0.05,        # max cpt count as fraction of data
    cpt.qvote=c(0.10, 0.90),   # ignore cpt libs with #pt outside range
    cpt.fsep=0.02,             # min separation b/t cpt as fraction data
    cpt.sep=10,                # min separation b/t cpt as absolute index diff
    cpt.libsep=2,              # min distance joining libary variant cpt
    cpt.timeout=2,             # time-out in seconds per CPT algorithm
    # Tracking Parameters
    track.maxwindow=0.4,       # largest value for [lp|diw].window
    # Display Parameters
    palette="Dark 2",          # plotting color palette, may start w/ 'hcl:'
    colID.data=7,              # index in palette drawing data points/spacing
    colID.filter=4,            # index in palette drawing LP/interval spacing
    colID.hist=6,              # index in palette drawing histogram of data
    colID.cdf=1,               # index in palette drawing distribution
    colID.peak=8,              # index in palette marking peaks
    colID.flat=5,              # index in palette marking flats
    colID.cpt=2,               # index in palette marking changepoints
    mark.alpha=TRUE,           # whether to underline significant probabilities
    mark.flat="box",           # how to indicate flats in graphs
    digits=4                   # signif digits for raw values, 0 take options()
  )
class(diopt.dflt) <- "Diopt"

# Global state.  Must stash diopt in a new environment because globals are
# locked in a namespace, but the environment contents are not.
diopt.env <- new.env(hash=FALSE)
assign("diopt", diopt.dflt, envir=diopt.env)


# Spacing analysis to generate/run.
analysis.names <- c("lp", "diw", "cpt")

# Low-pass filter kernels.
fir.names <- c("kaiser", "triangular", "bartlett", "hamming", "hanning",
               "gaussian", "normal", "blackman")

# Null distributions for flat model.
flat.basedist <- c("logistic", "weibull", "gaussian", "normal", "gumbel")

# Feature tests available in the low-pass spacing.
feattest.lp <- c("ht", "pkexcur", "len", "ftexcur")

# Feature tests available in the interval spacing.
feattest.diw <- c("pkexcur", "runht", "nrun", "runlen", "ftexcur")

# Styles for indicating flats in graph.
flat.style <- c("box", "bar")


# Check if the value val is appropriate for the option tag.  Return the valid
# value (may be modified, b.v. for string completion) or NULL if the value
# cannot be used.  If tag is not recognized return NULL.
validate.diopt <- function(tag, val) {

  # Does no checking of cpt.libs.
  switch(tag,
         analysis=diopt.isstring(val, analysis.names, TRUE),
         data.midq=diopt.isposint(val, incl0=TRUE, maxval=4),
         lp.kernel=diopt.isstring(val, fir.names, FALSE),
         lp.window=diopt.iswindow(val),
         lp.tests=diopt.isstring(val, feattest.lp, TRUE),
         lp.param=diopt.isparam(val),
         diw.window=diopt.iswindow(val),
         diw.tests=diopt.isstring(val, feattest.diw, TRUE),
         diw.param=diopt.isparam(val),
         peak.fht=diopt.isfraction(val),
         peak.frelht=diopt.isfraction(val),
         peak.fhtie=diopt.isfraction(val),
         peak.fhsupp=diopt.isfraction(val, TRUE),
         flat.fripple=diopt.isfraction(val),
         flat.minlen=diopt.isposint(val),
         flat.fminlen=diopt.isfraction(val),
         flat.noutlier=diopt.isposint(val, incl0=TRUE),
         flat.distrib=diopt.isstring(val, flat.basedist, FALSE),
         excur.nrep=diopt.isposint(val),
         excur.ntop=diopt.isposint(val, incl0=TRUE),
         excur.seed=diopt.isposint(val, incl0=TRUE),
         perm.nrep=diopt.isposint(val),
         perm.seed=diopt.isposint(val, incl0=TRUE),
         alpha.ht=diopt.isfraction(val),
         alpha.pkexcur.lp=diopt.isfraction(val),
         alpha.pkexcur.diw=diopt.isfraction(val),
         alpha.len=diopt.isfraction(val),
         alpha.ftexcur.lp=diopt.isfraction(val),
         alpha.ftexcur.diw=diopt.isfraction(val),
         alpha.runht=diopt.isfraction(val),
         alpha.nrun=diopt.isfraction(val),
         alpha.runlen=diopt.isfraction(val),
         cpt.libs=val,
         cpt.fncpt.max=diopt.isfraction(val),
         cpt.qvote=diopt.isqrange(val),
         cpt.fsep=diopt.isfraction(val),
         cpt.sep=diopt.isposint(val, incl0=FALSE),
         cpt.libsep=diopt.isposint(val, incl0=TRUE),
         cpt.timeout=diopt.ispos(val),
         track.maxwindow=diopt.iswindow(val),
         palette=diopt.ispalette(val),
         colID.data=diopt.iscolorID(val),
         colID.filter=diopt.iscolorID(val),
         colID.hist=diopt.iscolorID(val),
         colID.cdf=diopt.iscolorID(val),
         colID.peak=diopt.iscolorID(val),
         colID.flat=diopt.iscolorID(val),
         colID.cpt=diopt.iscolorID(val),
         mark.alpha=diopt.isbool(val),
         mark.flat=diopt.isstring(val, flat.style, FALSE),
         digits=diopt.isposint(val, incl0=TRUE),
         NULL)
}


# Screen the values val, returning the full match to options opt, or NULL if
# there is no partial match.  If several.ok is TRUE allow multiple values and
# return NULL if any fail.
diopt.isstring <- function(val, opts, several.ok=FALSE) {

  val <- tolower(val)
  if (several.ok) {
    optID <- pmatch(val, opts, nomatch=0L)
    if (any(0 == optID)) {
      NULL
    } else {
      opts[optID]
    }
  } else if (1 == length(val)) {
    optID <- pmatch(val, opts, nomatch=0L)
    if (0 == optID) {
      NULL
    } else {
      opts[optID]
    }
  } else {
    NULL
  }
}

# Return the value val if either a fraction or a positive integer, or NULL.
diopt.iswindow <- function(val) {
  fval <- diopt.isfraction(val)
  if (is.null(fval)) {
    diopt.isposint(val, TRUE)
  } else {
    fval
  }
}

# Return the value val if a fraction between 0 and 1, excl unless incl01 is
# TRUE, or NULL.
diopt.isfraction <- function(val, incl01=FALSE) {
  if ((1 == length(val)) && is.numeric(val) &&
     ifelse(incl01, 0.0 <= val, 0.0 < val) &&
     ifelse(incl01, val <= 1.0, val < 1.0)) {
    val
  } else {
    NULL
  }
}

# Return the value val if a positive integer, or NULL.  If incl0 is TRUE
# also allow zero values.
diopt.isposint <- function(val, incl0=FALSE, maxval=Inf) {
  if ((1 == length(val)) && (abs(val - round(val)) < 1e-6) &&
      ifelse(incl0, 0 <= val, 0 < val) && (val <= (maxval + 1e-6))) {
    as.integer(round(val))
  } else {
    NULL
  }
}

# Return the value val if any positive number, or NULL.
diopt.ispos <- function(val) {

  if ((1 == length(val)) && is.numeric(val) && (0.0 < val)) {
    val
  } else {
    NULL
  }
}

# Return the value val if a pair of fractions in increasing order, or NULL.
diopt.isqrange <- function(val) {

  if ((2 == length(val)) &&
      is.numeric(val[1]) && (0.0 <= val[1]) && (val[1] <= 1.0) &&
      is.numeric(val[2]) && (0.0 <= val[2]) && (val[2] <= 1.0) &&
      (val[1] < val[2])) {
    val
  } else {
    NULL
  }
}

# Return the value val if a list with acceptable detector/test parameters
# (peak.*, flat.*, alpha.*), or NULL if any member is invalid.  Returned
# values may be changed by the tests (b.v. string completion).
diopt.isparam <- function(val) {

  if (is.list(val) &&
      !any(is.na(match(names(val), names(diopt.dflt)))) &&
      all( sapply(names(val),
                  function(tag) {
                    grepl("^peak\\.", tag) || grepl("^flat\\.", tag) ||
                    grepl("^alpha\\.", tag)
                  }) )) {
    param <- lapply(seq_along(val),
                    function(i) {
                      validate.diopt(names(val)[i], val[[i]])
                    })
    if (!any(sapply(param, is.null))) {
      names(param) <- names(val)
      param
    } else {
      NULL
    }
  } else {
    NULL
  }
}

# Return the value val if a known palette, either default R set from
# palette.pals or, if with initial prefix 'hcl:<name>', from hcl.pals.  val
# must be a string and may be in lower case; matching is done as in the R
# functions.  Return NULL if the value is invalid, including more than one.
diopt.ispalette <- function(val) {

  if ((1 == length(val)) && is.character(val)) {
    # This regexp is used by both palette.pals() and hcl.pals().
    re <- "[-_\\,()\\ \\.]"
    val <- tolower(val)
    if ("hcl:" == substr(val, 1, 4)) {
      pals <- hcl.pals()
      val <- substring(val, 5)
      prefix <- "hcl:"
    } else {
      pals <- palette.pals()
      prefix <- ""
    }
    pos <- match(gsub(re, "", val), tolower(gsub(re, "", pals)), nomatch=0L)
    if (0 < pos) {
      paste0(prefix, pals[pos])
    } else {
      NULL
    }
  } else {
    NULL
  }
}

# Return the value val if a valid index into the palette, which will be
# generated with 8 colors, ie. val must be 1 /tm 8.  Convert value to an
# integer.  Return NULL if invalid, including more than one.
diopt.iscolorID <- function(val) {

  if ((1 == length(val)) && is.numeric(val) && (1 <= val) && (val <= 8)) {
    as.integer(round(val))
  } else {
    NULL
  }
}

# Return the value val coerced to a logical TRUE/FALSE, or NULL if that
# cannot be done.
diopt.isbool <- function(val) {

  if ((1 == length(val)) && !is.na(as.logical(val))) {
    as.logical(val)
  } else {
    NULL
  }
}
