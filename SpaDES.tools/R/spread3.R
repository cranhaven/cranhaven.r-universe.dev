utils::globalVariables(c(
  "abund", "abundActive", "abundSettled", "direction", "distance", "distGrp",
  "from", "indFull", "lenRec", "lenSrc", "mags", "meanNumNeighs",
  "newDirs", "newMags", "prop", "srcAbundActive", "sumAbund", "sumAbund2"
))

#' An alternative spread function, conceived for insects
#'
#' This is built with [spread2()] and is still experimental.
#' This one differs from other attempts in that it treats the advection and
#' dispersal as mathematical vectors that are added together.
#' They are "rounded" to pixel centres.
#'
#' @param start Raster indices from which to initiate dispersal
#'
#' @param rasQuality A raster with habitat quality. Currently, must
#'   be scaled from 0 to 1, i.e., a probability of "settling"
#' @param rasAbundance A raster where each pixel represents the number
#'   of "agents" or pseudo-agents contained. This number of agents, will
#'   be spread horizontally, and distributed from each pixel
#'   that contains a non-zero non NA value.
#'
#' @param advectionDir A single number or `RasterLayer` in degrees from North = 0
#'   (though it will use radians if all values are `abs(advectionDir) > 2 * pi)`.
#'   This indicates the direction of advective forcing (i.e., wind).
#'
#' @param advectionMag A single number or `RasterLayer` in distance units of the
#'   `rasQuality`, e.g., meters, indicating the relative forcing that will
#'   occur. It is imposed on the total event, i.e., if the `meanDist` is
#'   `10000`, and `advectionMag` is `5000`, then the expected
#'   distance (i.e., `63%` of agents) will have settled by `15000` map units.
#'
#' @param dispersalKernel One of either `"exponential"` or `"weibull"`.
#'
#' @param meanDist A single number indicating the mean distance parameter in map units
#'    (not pixels), for a negative exponential distribution
#'    dispersal kernel (e.g., `dexp`). This will mean that `63%` of agents will have
#'    settled at this `meanDist` (still experimental).
#'
#' @param sdDist A single number indicating the `sd` parameter of a two-parameter
#'   `dispersalKernel`.
#'   Defaults to `1`, which is the same as the `exponential` distribution.
#'
#' @param verbose Numeric. With increasing numbers above 0, there will be more
#'     messages produced. Currently, only 0, 1, or 2+ are distinct.
#'
#' @param plot.it Numeric. With increasing numbers above 0, there will be plots
#'     produced during iterations. Currently, only 0, 1, or 2+ are distinct.
#'
#' @param minNumAgents Single numeric indicating the minimum number of agents
#'    to consider all dispersing finished. Default is 50.
#'
#' @param saveStack If provided as a character string, it will save each iteration
#'   as part of a `rasterStack` to disk upon exit.
#'
#' @param skipChecks Logical. If `TRUE`, assertions will be skipped (faster, but could miss
#'   problems)
#'
#' @return A `data.table` with all information used during the spreading
#'
#' @export
#' @importFrom data.table := setattr
#' @importFrom fpCompare %>=% %>>%
#' @importFrom graphics par
#' @importFrom terra distance xyFromCell
#' @importFrom stats pexp dweibull pweibull
#'
#' @example inst/examples/example_spread3.R
#'
spread3 <- function(start, rasQuality, rasAbundance, advectionDir,
                    advectionMag, meanDist, dispersalKernel = "exponential",
                    sdDist = 1, plot.it = 2,
                    minNumAgents = 50, verbose = getOption("LandR.verbose", 0),
                    saveStack = NULL, skipChecks = FALSE) {
  dtThr <- data.table::getDTthreads()
  if (!skipChecks)
    testEquivalentMetadata(rasAbundance, rasQuality)

  if (missing(advectionDir)) {
    advectionDir <- 0
    message("advectionDir was not supplied; removing advection")
    advectionMag <- 0
  }

  if (is(advectionDir, "Raster")) {
    if (!skipChecks)
      testEquivalentMetadata(rasAbundance, advectionDir)
    advectionDir <- advectionDir[]
  } else if (length(advectionDir) != 1) {
    if (length(advectionDir) != ncell(rasAbundance)) {
      stop("advectionDir must be length 1, length ncell(rasAbundance), or a Raster with ",
           "identical metadata as rasAbundance")
    }
  }

  if (missing(advectionMag)) {
    advectionMag <- 0
  }
  if (is(advectionMag, "Raster")) {
    if (!skipChecks)
      testEquivalentMetadata(rasAbundance, advectionMag)
    advectionMag <- advectionMag[]
  } else if (length(advectionMag) != 1) {
    if (length(advectionMag) != ncell(rasAbundance)) {
      stop("advectionMag must be length 1, length ncell(rasAbundance), or a Raster with ",
           "identical metadata as rasAbundance")
    }
  }

  #if (any(abs(advectionDir) > 2 * pi)) {
    messAngles <- "degrees"
    advectionDir <- rad2(advectionDir)
  # } else {
  #   messAngles <- "radians"
  # }
  if (verbose > 0)
  message("assuming that advectionDir is in geographic ", messAngles,
          "(i.e., North is 0)")

  if (missing(start))
    start <- which(!is.na(rasAbundance[]) & rasAbundance[] > 0)

  start <- spread2(rasQuality, start, iterations = 0, returnDistances = TRUE,
                   returnDirections = TRUE, returnFrom = TRUE, asRaster = FALSE,
                   skipChecks = skipChecks)
  start[, `:=`(abundActive = rasAbundance[][start$pixels],
               abundSettled = 0)]
  abundanceDispersing <- sum(start$abundActive, na.rm = TRUE)
  plotMultiplier <- mean(start$abundActive, na.rm = TRUE) /
    ((meanDist * 10 / res(rasQuality)[1]))
  if (isTRUE(plot.it > 1)) {
    rasIterations <- terra::rast(rasQuality)
    rasIterations[] <- NA
    rasIterations[start$pixels] <- 0
  }

  while (abundanceDispersing > minNumAgents) {
    if (dtThr == 1 && data.table::getDTthreads() != 1) data.table::setDTthreads(1)
    b <- spread2(landscape = rasQuality, start = start,
                 spreadProb = 1, iterations = 1, asRaster = FALSE,
                 returnDistances = TRUE, returnFrom = TRUE,
                 returnDirections = TRUE, skipChecks = skipChecks,
                 circle = TRUE, allowOverlap = 3)
    #b <- b[!duplicated(b, by = c("initialPixels", "pixels"))]
    spreadState <- attr(b, "spreadState")
    # faster than assessing with a which()
    active <- spreadState$whActive
    inactive <- spreadState$whInactive

    iteration <- spreadState$totalIterations
    if (verbose > 1) message("Iteration ", iteration)
    if (isTRUE(plot.it > 1)) {
      rasIterations[b[active]$pixels] <- iteration
      terra::plot(rasIterations, range = c(0, meanDist / (res(rasQuality)[1] / 12)))
    }

    fromPts <- xyFromCell(rasQuality, b[["from"]][active])
    toPts <- xyFromCell(rasQuality, b[["pixels"]][active])
    dists <- distance(fromPts, toPts, pairwise = TRUE, lonlat = FALSE)
    dirs <- b[["direction"]][active]

    # Convert advection vector into length of dirs from pixels, if length is not 1
    advectionDirTmp <- if (length(advectionDir) > 1 || is(advectionDir, "SpatRaster")) {
      advectionDir[][b[["pixels"]][active]]
    } else {
      advectionDir
    }
    advectionMagTmp <- if (length(advectionMag) > 1 || is(advectionMag, "SpatRaster")) {
      advectionMag[][b[["pixels"]][active]]
    } else {
      advectionMag
    }
    xDist <- round(sin(advectionDirTmp) * advectionMagTmp + sin(dirs) * dists, 4)
    yDist <- round(cos(advectionDirTmp) * advectionMagTmp + cos(dirs) * dists, 4)

    # This calculates: "what fraction of the distance being moved is along the dirs axis"
    #   This means that negative mags is "along same axis, but in the opposite direction"
    #   which is dealt with next, see "opposite direction"
    set(b, active, "mags", round(sin(dirs) * xDist + cos(dirs) * yDist, 3))

    # b[active, mags := round(sin(dirs) * xDist + cos(dirs) * yDist, 3)]
    negs <- b[["mags"]][active] < 0
    negs[is.na(negs)] <- FALSE
    #dirs2 <- dirs
    anyNegs <- any(negs)
    nonNA <- !is.na(b[["direction"]][active])
    activeNonNA <- active[nonNA]

    if (any(anyNegs)) { # "opposite direction"
      dirs2 <- (dirs[negs] + pi) %% (2 * pi)
      activeNegs <- active[negs]
      set(b, activeNegs, "mags", -b$mags[activeNegs])
      # b[active[negs], mags := -mags]
      set(b, activeNegs, "newDirs", dirs2)
      # b[active[negs], newDirs := dirs2]

      b[activeNonNA, newMags := mags + mags[match(round(direction, 4), round(newDirs, 4))],
        by = "initialPixels"]
      nonNANewMags <- !is.na(b[["newMags"]][active])
      activeNonNANewMags <- active[nonNANewMags]

      set(b, activeNonNANewMags, "mags", b[["newMags"]][activeNonNANewMags])
      # b[active[nonNANewMags], mags := newMags]

      nonNANewDirs <- !is.na(b[["newDirs"]][active])

      activeNonNANewDirs <- active[nonNANewDirs]
      set(b, activeNonNANewDirs, "mags", 0)
      # b[active[nonNANewDirs], mags := 0]
      set(b, NULL, c("newDirs", "newMags"), NULL)
    }
    b[active[nonNA], prop := round(mags / sum(mags), 3), by = c("from", "initialPixels")]
    if (FALSE) # almost
      b[active, -c("abundActive")][b[inactive, c("pixels", "initialPixels", "abundActive")],
                                   on = c("from" = "pixels", "initialPixels")]

    b[distance %>>% ((iteration - 2) * res(rasAbundance)[1]),
      srcAbundActive := abundActive[match(from, pixels)], by = "initialPixels"]

    # Expected number, based on advection and standard spread2
    set(b, active, "abund", b[["srcAbundActive"]][active] * b[["prop"]][active])
    # b[active, abund := srcAbundActive * prop]

    byGroup <- c("pixels", "initialPixels")
    b[active, lenRec := .N, by = byGroup]
    b[active, lenSrc := min(2.5, .N), by = c("from", "initialPixels")]

    # Sum all within a receiving pixel,
    #    then collapse so only one row per receiving cell,
    #    it is a markov chain of order 1 only, except for some initial info
    b[active, `:=`(sumAbund = sum(abund, na.rm = TRUE),
                   indWithin = seq(.N),
                   indFull = .I,
                   meanNumNeighs = mean(lenSrc / lenRec) * mean(mags)), by = byGroup]
    keepRows <- which(b$indWithin == 1 | is.na(b$indWithin))
    b <- b[keepRows]
    active <- na.omit(match(active, b$indFull))
    #b[, indFull := seq_len(NROW(b))]

    set(b, active, "sumAbund2", b[["sumAbund"]][active] * b[["meanNumNeighs"]][active] /
          mean(b[["mags"]][active]))
    # b[active, sumAbund2 := sumAbund * meanNumNeighs/ mean(mags)]

    totalSumAbund <- sum(b[["sumAbund"]][active], na.rm = TRUE)
    totalSumAbund2 <- sum(b[["sumAbund2"]][active], na.rm = TRUE)
    multiplyAll <- totalSumAbund / totalSumAbund2

    set(b, active, "sumAbund", b[["sumAbund2"]][active] * multiplyAll)
    set(b, NULL, c("abund", "sumAbund2", "mags", "lenSrc", "lenRec",
                   "meanNumNeighs", "prop", "indWithin", "indFull"),
        NULL)

    # Some of those active will not stop: estimate here by kernel probability
    advectionMagTmp <- if (length(advectionMag) > 1 || is(advectionMag, "SpatRaster")) {
      advectionMag[][b[active]$pixels]
    } else {
      advectionMag
    }
    if (startsWith(tolower(dispersalKernel), prefix = "expon")) {
      cumProb <- pexp(q = b[["distance"]][active],
                      rate = pi / (meanDist + advectionMagTmp)^1.5)
    } else if (startsWith(tolower(dispersalKernel), prefix = "weib")) {
      mn <- (meanDist + advectionMagTmp)
      sd <- mn / sdDist # 0.8 to 2.0 range
      shape <- (sd / mn)^(-1.086)
      scale <- mn / exp(lgamma(1 + 1 / shape))
      cumProb <- pweibull(b[["distance"]][active], shape, scale = scale)
      if (plot.it > 0) {
        par(mfrow = c(1, 2))
        x <- seq(mn / 5) * 10
        plot(x, cumProb)
        plot(x, dweibull(x, shape, scale = scale))
      }

    } else {
      stop("dispersalKernel must be either exponential or weibull")
    }
    set(b, active, "abundSettled", cumProb * b[["sumAbund"]][active])
    #b[active, abundSettled := pexp(q = distance,
    #                               rate = pi / (meanDist + advectionMagTmp)^1.5) * sumAbund] # kernel is 1 dimensional,
    # b[active, abundSettled :=
    #     dexp(x = distance, rate = 1/(meanDist+advectionMag)) * sumAbund] # kernel is 1 dimensional,
    # but spreading is dropping agents in 2 dimensions
    # It doesn't work to use ^2, I think because we are discretizing the landscape
    # from a continuous surface, so, the number of pixels with agents settled
    # is not actually the full square on a 1 dimensional line ... I might be wrong
    # Some of the estimated dropped will not drop because of quality
    #   First place to round to whole numbers
    set(b, active, "abundSettled", b[["abundSettled"]][active] * rasQuality[][b[["pixels"]][active]])
    # b[active, abundSettled := abundSettled  * rasQuality[][pixels]]
    set(b, active, "abundActive", b[["sumAbund"]][active] - b[["abundSettled"]][active])
    # b[active, abundActive := sumAbund - abundSettled]
    set(b, active[active %in% which(b$abundActive < 1)], "abundActive", 0)
    # b[active[active %in% which(b$abundActive < 1)], abundActive := 0]


    abundanceDispersing <- sum(b[active]$abundActive, na.rm = TRUE)
    if (verbose > 1) message("Number still dispersing ", abundanceDispersing)
    if (isTRUE(plot.it > 0)) {
      b2 <- b[, sum(abundSettled), by = "pixels"]
      rasAbundance[b2$pixels] <- ceiling(b2$V1)
      needNew <- FALSE
      if (max(ceiling(b2$V1), na.rm = TRUE) > plotMultiplier) {
        plotMultiplier <- plotMultiplier * 1.5
        needNew <- TRUE
      }
      if (quickPlotUse()) {
        if (iteration == 1) quickPlot::clearPlot()
        quickPlot::Plot(rasAbundance, new = iteration == 1 || needNew,
                        legendRange = c(0, plotMultiplier), title = "Abundance")
      } else {
        terra::plot(rasAbundance, # new = iteration == 1 || needNew,
                    range = c(0, plotMultiplier), main = "Abundance")
      }
    }

    newInactive <- b[["abundActive"]][active] == 0

    set(b, active[newInactive], "state", "inactive")
    # b[active[newInactive], state := "inactive"]
    spreadState$whActive <- active[!newInactive]
    spreadState$whInactive <- c(spreadState$whInactive, active[newInactive])
    setattr(b, "spreadState", spreadState)

    # clean up temporary columns
    set(b, NULL, c("sumAbund", "srcAbundActive"), NULL)

    start <- b
  }
  if (!is.null(saveStack)) {
    saveStackFALSE <- isFALSE(saveStack) # allow TRUE or character
    if (!saveStackFALSE) {
      if (isTRUE(saveStack))
        saveStack <- tempfile(fileext = ".tif")
      # make 30 maps
      b[, distGrp := floor(distance / (diff(range(b$distance)) / 30))]
      ras <- terra::rast(rasAbundance)
      out1 <- lapply(unique(b$distGrp), function(x)  {
        r <- terra::rast(ras)
        x1 <- b[distGrp <= x, sum(abundSettled), by = "pixels"]
        r[x1$pixels] <- ceiling(x1$V1)
        # r <- writeRaster(r, tempfile(fileext = ".tif"))
        r
      })
      writeRaster(terra::rast(out1), filename = saveStack, overwrite = TRUE)
      message("stack saved to ", saveStack)
    }
  }

  return(start)
}

#' Return the (approximate) middle pixel on a raster
#'
#' @param ras A `Raster` or `SpatRaster` object
#'
#' @return This calculation is slightly different depending on whether
#' the `nrow(ras)` and `ncol(ras)` are even or odd.
#' It will return the exact middle pixel if these are odd, and
#' the pixel just left and/or above the middle pixel if either
#' dimension is even, respectively.
#'
#' @export
#' @importFrom terra ncell ncol nrow
middlePixel <- function(ras) {
  if (nrow(ras) %% 2 == 1) {
    floor(ncell(ras) / 2)
  } else {
    floor(nrow(ras) / 2) * ncol(ras) - floor(ncol(ras) / 2)
  }
}

#' Test that metadata of 2 or more objects is the same
#'
#' Currently, only Raster class has a useful method. Defaults to
#' `all(sapply(list(...)[-1], function(x) identical(list(...)[1], x)))`
#'
#' @param ... 2 or more of the same type of object to test for equivalent metadata.
#'
#' @export
testEquivalentMetadata <- function(...) {
  UseMethod("testEquivalentMetadata")
}

#' @export
#' @importFrom terra compareGeom
testEquivalentMetadata.default <- function(...) {
  if (is(list(...)[[1]], "Raster"))
    raster::compareRaster(..., orig = TRUE)
  else
    terra::compareGeom(...)
  return(invisible())
}

quickPlotUse <- function() {
  requireNamespace("quickPlot", quietly = TRUE) &&
  tryCatch(packageVersion("quickPlot") > "0.1.8", error = function(x) FALSE)
}
