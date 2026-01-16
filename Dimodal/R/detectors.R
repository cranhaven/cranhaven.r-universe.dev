
# detectors.R:
# R side interface to feature detectors.  These are not specific to the
# spacing module; they can be used for any data.
#
# c 2024-2025 Greg Kreider, Primordial Machine Vision Systems, Inc.

## To Do:
# -
#


##### Public Interface

# Finds runs in data x, with fuzzy relative comparison where the difference
# from all points within the run to its start (not chaining along) is
# within the fraction feps of their average, ie. |(x2-x1)/((x1+x2)/2)| < feps.
# If feps is 0 then values must match exactly.  Infinities match each other
# but not finite values and NA and NaN values are skipped.  x can also be
# integer, logical, or string values in which only exact matches are supported
# (but still skipping NAs, unlike rle).  Returns a general vector with elements
#   $runs     lengths of fuzzy runs, length of x, 0 if no run starts there
#   $nskip    number of skipped elements
#   $stats    ['nrun'] number of runs, ['maxrun'] longest run
# The runs and nskip vectors are set up to scan along the original data.
#   i <- if (0 == r$runs[1]) { r$nskip[1] + 1 } else { 1 }
#   i <- i + r$runs[i] + r$nskip[i]
# Use runs.as.rle to convert the result to an rle encoding.
# Typical values are 0.001 for feps.
find.runs <- function(x, feps) {

  if (!is.vector(x)) {
    x <- as.vector(x)
  }
  if (0 == length(x)) {
    stop("vector has no data")
  }
  
  .Call("C_find_runs", x, feps, PACKAGE="Dimodal")
}


# Find local minima and maxima in data x, eliminating small peaks if 
# they are less than the fraction fht of the total range of the data or if
# the height to an adjacent minimum is less than the fraction frelht of their
# average value.  Values within fhtie are treated as equal (a fuzzy run) and
# considered located in the middle of the mesa.  x must be an integer or real
# vector.  The support of a maximum is the range of points until their values
# drop to the fraction fhsupp of the peak height to that side, similar to FWHM
# where fhsupp would be 0.5.  The range is NA for minima and one-sided peaks.
# Return a data frame assigned to class Dipeak with extrema in rows and columns
#   $pos       position of extremum in x
#   $ismax     TRUE if a maximum, FALSE if a minimum
#   $valsd     data at extremum standardized by stdev
#   $lht       peak height to left minimum, NA at minimum or first peak
#   $rht       peak height to right minimum, NA at minimum or last peak
#   $lminID    position of minimum to left, NA if first/last or at minimum
#   $rminID    position of minimum to right, NA if first/last or at minimum
#   $lsuppID   first index of support range
#   $rsuppID   last index (incl.) of support range
# Typical values are 0.05 for fht, 0.15 for frelht, and 0.001 for fhtie.
# The data frame will be empty (0 rows) if the data do not support extrema:
# there are too few, or the values are constant, or are not finite.
find.peaks <- function(x, fht, frelht, fhtie, fhsupp) {

  if (!is.vector(x)) {
    x <- as.vector(x)
  }
  if (0 == length(x)) {
    return(mockup.Dipeak())
  }

  pk <- .Call("C_find_peaks", x, fht, frelht, fhtie, fhsupp, PACKAGE="Dimodal")
  df <- as.data.frame(pk)
  class(df) <- c("Dipeak", class(df))
  df
}


# Find local flats in data x, as defined by the fripple value as a fraction
# of the range of x centered at each point.  Flats must have a length at least
# minlen (absolute) or a fraction fminlen as a fraction of the number of data
# points, whichever is larger.  To each side of the seed flats may contain
# noutlier points outside the ripple spec, although the start or endpoint will
# always be in range.  They may overlap.  Data must be real or integer and
# non-finite values will be ignored.  Returns a data frame with features in
# rows and columns
#   $src       point in x that seeded the flat, a unique identifier
#   $stID      starting index in x of each flat
#   $endID     ending index in x of each flat
#   $len       length of flat
#   $srcval    data value at src (center of flat height)
#   $ht        height of flat in x
#   $htsd      standardized height (by stdev) of flat
# Typical values are 0.05 for fripple, 30 for minlen, 0.05 for rellen, and
# 1 for noutlier.
find.flats <- function(x, fripple, minlen, fminlen, noutlier) {

  # Final timing [ms] based on sub-set of asteroid axes, for Diw 150 window.
  # This is using identify_flats_cover for both.
  #   # samples    scan     tree    speed-up
  #     1k            1        1        1x
  #     5k            5        8        0.6x
  #    10k           31       17      1.8x
  #    50k          827      105      7.9x
  #   100k         5250      294     17.9x
  #   250k        33214      969     34.3x
  #   500k       142768     1934     73.8x
  #   750k       336697     4370     77.0x
  if (!is.vector(x)) {
    x <- as.vector(x)
  }
  if (length(x) < 2) {
    return(mockup.Diflat())
  }

  ft <- .Call("C_find_flats", x, fripple, minlen, fminlen, noutlier, 0L,
              PACKAGE="Dimodal")
  df <- as.data.frame(ft)
  class(df) <- c("Diflat", class(df))
  df
}

# Find intervals without a significant slope, inverting the interval spacing
# test used in the modehunt package.  Data x must be real or integer, and
# non-finite values, including NA and NaN, will be removed; the indices will
# refer to the sorted raw data after removal.  alpha is the significance
# level of the test, between 0 and 1, and correct, a boolean, if TRUE adjusts
# the test against short intervals.  Returns a data frame with one row per
# section and columns
#   $stID      starting index in the sorted x of each section
#   $endID     ending index (incl.) of each section
# Typical values are 0.95 for alpha and TRUE for correct.
find.level.sections <- function(x, alpha, correct) {

  if (!is.vector(x)) {
    x <- as.vector(x)
  }

  # finmap will store the indices in the original data we're processing,
  # which we need when mapping back from the level sections.
  finmap <- which(is.finite(x))
  x <- sort(x[finmap])
  if (is.integer(x[1L])) {
    x <- as.numeric(x)
  }

  if (length(x) < 3) {
    stop("must have at least three points for level sections")
  }

  lvl <- .Call("C_find_level_sections", x, alpha, correct, PACKAGE="Dimodal")

  data.frame( stID=finmap[lvl[["stID"]]], endID=finmap[lvl[["endID"]]] )
}


