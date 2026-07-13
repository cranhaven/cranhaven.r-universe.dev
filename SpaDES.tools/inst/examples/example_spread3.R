## these tests are fairly heavy, so don't run during automated tests
#########################################################
# Simple case, no variation in rasQuality, numeric advectionDir and advectionMag
#########################################################
\donttest{
  library(terra)

  origDTThreads <- data.table::setDTthreads(2L)
  origNcpus <- options(Ncpus = 2L)

  maxDim <- 10000
  ras <- terra::rast(terra::ext(c(0, maxDim, 0, maxDim)), res = 100, vals = 0)
  rasQuality <- terra::rast(ras)
  rasQuality[] <- 1
  rasAbundance <- terra::rast(rasQuality)
  rasAbundance[] <- 0
  # startPixel <- middlePixel(rasAbundance)
  startPixel <- sample(seq(terra::ncell(rasAbundance)), 30)
  rasAbundance[startPixel] <- 1000
  advectionDir <- 70
  advectionMag <- 4 * res(rasAbundance)[1]
  meanDist <- 2600

  # Test the dispersal kernel -- create a function
  plotDispersalKernel <- function(out, meanAdvectionMag) {
    out[, disGroup := round(distance / 100) * 100]
    freqs <- out[, .N, by = "disGroup"]
    freqs[, `:=`(cumSum = cumsum(N), N = N)]
    plot(freqs$disGroup, freqs$cumSum, # addTo = "CumulativeNumberSettled",
         main = "Cumulative Number Settled") # can plot the distance X number
    abline(v = meanAdvectionMag + meanDist)
    newTitle <- "Number Settled By Distance"
    plot(freqs$disGroup, freqs$N, # addTo = gsub(" ", "", newTitle),
         main = newTitle) # can plot the distance X number
    abline(v = meanAdvectionMag + meanDist)
    # should be 0.63:
    freqs[disGroup == meanAdvectionMag + meanDist, cumSum] / tail(freqs, 1)[, cumSum]
    mtext(side = 3, paste("Average habitat quality: ",
                          round(mean(rasQuality[], na.rm = TRUE), 2)),
          outer = TRUE, line = -2, cex = 2)
  }
  out <- spread3(rasAbundance = rasAbundance,
                 rasQuality = rasQuality,
                 advectionDir = advectionDir,
                 advectionMag = advectionMag,
                 meanDist = meanDist, verbose = 2,
                 plot.it = interactive())

  plotDispersalKernel(out, advectionMag)

  # The next examples are potentially time consuming; avoid on automated testing
  if (interactive()) {
    #########################################################
    ### The case of variable quality raster
    #########################################################
    rasQuality <- terra::rast(system.file("extdata", "rasQuality.tif",
                                          package = "SpaDES.tools"))
    terra::crs(rasQuality) <- system.file("extdata", "targetCRS.rds", package = "SpaDES.tools") |>
      readRDS() |>
      slot("projargs")
    mask <- rasQuality < 5
    rasQuality[mask[] %in% TRUE] <- 0
    # rescale so min is 0.75 and max is 1
    rasQuality[] <- rasQuality[] / (reproducible::maxFn(rasQuality) * 4) + 1 / 4
    rasAbundance <- terra::rast(rasQuality)
    rasAbundance[] <- 0
    startPixel <- sample(seq(ncell(rasAbundance)), 300)
    rasAbundance[startPixel] <- 1000
    advectionDir <- 75
    advectionMag <- 4 * res(rasAbundance)[1]
    meanDist <- 2600
    out <- spread3(rasAbundance = rasAbundance,
                   rasQuality = rasQuality,
                   advectionDir = advectionDir,
                   advectionMag = advectionMag,
                   meanDist = meanDist, verbose = 2,
                   plot.it = interactive())
    if (interactive()) {
      plotDispersalKernel(out, advectionMag)
    }

    ###############################################################################
    ### The case of variable quality raster, raster for advectionDir & advectionMag
    ###############################################################################
    maxDim <- 10000
    ras <- terra::rast(terra::ext(c(0, maxDim, 0, maxDim)), res = 100, vals = 0)
    rasQuality <- terra::rast(ras)
    rasQuality[] <- 1
    rasAbundance <- terra::rast(rasQuality)
    rasAbundance[] <- NA
    # startPixel <- middlePixel(rasAbundance)
    startPixel <- sample(seq(ncell(rasAbundance)), 25)
    rasAbundance[startPixel] <- 1000

    # raster for advectionDir
    advectionDir <- terra::rast(system.file("extdata", "advectionDir.tif",
                                            package = "SpaDES.tools"))
    crs(advectionDir) <- crs(rasQuality)
    # rescale so min is 0.75 and max is 1
    advectionDir[] <- advectionDir[] / (reproducible::maxFn(advectionDir)) * 180

    # raster for advectionMag
    advectionMag <- terra::rast(system.file("extdata", "advectionMag.tif",
                                            package = "SpaDES.tools"))
    crs(advectionMag) <- crs(rasQuality)
    # rescale so min is 0.75 and max is 1
    advectionMag[] <- advectionMag[] / (reproducible::maxFn(advectionMag)) * 600

    out <- spread3(rasAbundance = rasAbundance,
                   rasQuality = rasQuality,
                   advectionDir = advectionDir,
                   advectionMag = advectionMag,
                   meanDist = meanDist, verbose = 2,
                   plot.it = interactive())

    if (interactive()) {
      names(advectionDir) <- "Wind direction"
      names(advectionMag) <- "Wind speed"
      names(rasAbundance) <- "Initial abundances"
      terra::plot(c(advectionDir, advectionMag, rasAbundance))

      plotDispersalKernel(out, mean(advectionMag[]))
    }

    #########################################
    # save iterations to a stack to make animated GIF
    ########################################
    tmpStack <- tempfile(pattern = "stackToAnimate", fileext = ".tif")
    out <- spread3(rasAbundance = rasAbundance,
                   rasQuality = rasQuality,
                   advectionDir = advectionDir,
                   advectionMag = advectionMag,
                   meanDist = 2600, verbose = 2,
                   plot.it = interactive(), saveStack = tmpStack)

    ## This animates the series of images into an animated GIF
    if (require(animation, quietly = TRUE)) {
      out2 <- terra::rast(tmpStack)
      gifName <- file.path(tempdir(), "animation.gif")

      # Only works on some systems; may need to configure
      # Works on Windows without system adjustments
      if (identical(.Platform$OS.type, "windows"))
        saveGIF(interval = 0.1, movie.name = gifName, expr = {
          for (i in seq(length(names(out2)))) terra::plot(out2[[i]])
        })
    }
  }

  # clean up
  data.table::setDTthreads(origDTThreads)
  options(Ncpus = origNcpus)
}
