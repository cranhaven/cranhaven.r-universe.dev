## re-implemented from sgibb/MALDIquant
.isMassObject <- function(x) {
  inherits(x=x, what="AbstractMassObject")
}

## re-implemented from sgibb/MALDIquant
.isMassObjectList <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }
  
  length(x) && all(unname(vapply(x, .isMassObject, logical(1L))))
}

## re-implemented from sgibb/MALDIquant
.stopIfNotIsMassObjectList <- function(x) {
  if (!.isMassObjectList(x)) {
    parentCall <- deparse(sys.call(-1L))
    stop(parentCall, " : ", sQuote(deparse(substitute(x))),
         " is no list of MALDIquant::AbstractMassObject objects!", call.=FALSE)
  }
  TRUE
}


## .doByLabels, re-implemented from sgibb/MALDIquant
##  run a specific function labelwise
##
## params:
##  l: list of AbstractMassObject objects
##  labels: factor, labels for samples
##  fun: function
##
## returns:
##  list of modified AbstractMassObject objects
##
.doByLabels <- function(l, labels, FUN, ..., mc.cores=1L) {
  
  ## test parameters
  .stopIfNotIsMassObjectList(l)
  
  FUN <- match.fun(FUN)
  
  if (!missing(labels)) {
    ## drop unused levels and turn argument into factor
    if (is.factor(labels)) {
      labels <- droplevels(labels)
    } else {
      ## preserve order in labels
      labels <- factor(labels, levels=unique(labels))
    }
    
    if (length(labels) != length(l)) {
      stop("For each item in ", sQuote("l"), " there must be a label in ",
           sQuote("labels"), "!")
    }
    
    ## replace tapply by split to preserve order
    tmp <- lapply(split(unlist(l), labels), FUN=FUN, ...)
    
    k <- unlist(tmp)
    
    if (length(k) != length(tmp)) {
      k <- unsplit(tmp, labels)
    }
  } else {
    k <- FUN(l, ...)
  }
  
  k
}

## MassSpectrum, re-implemented from sgibb/MALDIquant
setMethod(f="approxfun",
          signature=signature(x="MassSpectrum"),
          definition=function(x, y=NULL, method="linear", yleft, yright,
                              rule=1L,  f=0L, ties=mean) {
            if (isEmpty(x)) {
              function(x)rep.int(NA, length(x))
            } else {
              approxfun(x=x@mass, y=x@intensity, method=method,
                        yleft=yleft, yright=yright, rule=rule, f=f, ties=ties)
            }
          })

## merge different metaData by equal list names, re-implemented from sgibb/MALDIquant
##
## params
##  m: list of metaData
##
## returns:
##  merged list
##
.mergeMetaData <- function(m) {
  
  .flat <- function(x)unname(unlist(x))
  
  nm <- names(m[[1L]])
  names(nm) <- nm
  lapply(nm, function(n) {
    cur <- m[[1L]][[n]]
    all <- lapply(m, "[[", n)
    len <- lengths(all)
    
    if (all(length(cur) == len)) {
      fcur <- .flat(cur)
      fall <- .flat(all)
      if (all(is.na(fcur) == is.na(fall)) && all(fcur == fall, na.rm=TRUE))
        return(cur)
    }
    
    if (!is.list(cur)) {
      all <- unlist(all)
    }
    unname(all)
  })
}

#' Column wise standard deviation
#'
#' also a function of sgibb
#' see https://stackoverflow.com/questions/17549762/is-there-such-colsd-in-r
#' @param x     matrix
#' @param na.rm logical
#'
#' @return sd
#' 
#' @importFrom MALDIquant isEmpty
#' @noRd
colSdColMeans <- function(x, na.rm = TRUE) {
  if (na.rm) {
    n <- colSums(!is.na(x)) # thanks @flodel
  } else {
    n <- nrow(x)
  }
  colVar <- colMeans(x * x, na.rm = na.rm) - (colMeans(x, na.rm = na.rm))^2
  return(sqrt(colVar * n / (n - 1)))
}

#' Still fork from sgibb/MALDIquant
#'
#' @param l       see MALDIquant
#' @param mergeMetaData see MALDIquant
#'
#' @return see MALDIquant
#' @importFrom stats approxfun
#' @noRd
sdMassSpectraFun <- function(l, mergeMetaData = TRUE) {

  ## merge metaData
  if (mergeMetaData) {
    metaData <- .mergeMetaData(lapply(l, function(x) x@metaData))
  } else {
    metaData <- list()
  }

  ## use the first non empty spectrum as reference
  i <- which(!vapply(l, isEmpty, logical(1L)))[1L]
  if (!is.na(i)) {
    mass <- l[[i]]@mass
  } else {
    mass <- NA_real_
  }

  ## interpolate not existing masses
  approxSpectra <- lapply(l, approxfun)

  ## get interpolated intensities
  intensityList <- lapply(approxSpectra, function(x) x(mass))

  ## create a matrix which could merged
  m <- do.call(rbind, intensityList)

  ## merge intensities
  intensity <- colSdColMeans(m, na.rm = TRUE)

  ## create an empty spectrum if all intensities are NaN
  if (is.nan(intensity[1L])) {
    intensity <- double()
    mass <- double()
  }

  createMassPeaks(mass = mass, intensity = intensity, snr = rep(NA_integer_, length(intensity)),metaData = metaData)
}

#' Compute standard-deviation spectra
#'
#' This is a fork from sgibb's MALDIquant::averageMassSpectra() function.
#' It is now able to compute "standard-deviation spectra".
#'
#' @param l      list, list of MassSpectrum objects.
#' @param labels list, list of factors (one for each MassSpectrum object) to do groupwise averaging.
#' @param ...    arguments to be passed to underlying functions (currently only mc.cores is supported).
#'
#' @return
#' Returns a single (no labels given) or a list (labels given) of standard-deviation spectra as MassSpectrum objects.
#' @export
#' @examples
#' data(Blank2022spec)
#' 
#' sdMassSpectrum(Blank2022spec, labels = names(Blank2022spec))[[1]]

sdMassSpectrum <- function(l, labels, ...) {

  .doByLabels(
    l = l, labels = labels, FUN = sdMassSpectraFun,
  )
}
