test_that("spread2 tests", {
  testInit("terra")
  rastDF <- needTerraAndRaster()

  # inputs for x
  aOrig <- terra::rast(system.file("extdata", "a.tif", package = "SpaDES.tools"))
  bOrig <- terra::rast(aOrig)

  bSimple <- terra::rast(terra::ext(0, 10, 0, 10), res = 1)

  spRas <- terra::rast(system.file("extdata", "spRas.tif", package = "SpaDES.tools"))

  utm <- "+proj=utm +zone=28 +datum=WGS84 +units=m +no_defs"
  terra::crs(spRas) <- utm


  for (ii in seq_len(NROW(rastDF))) {
    pkg <- rastDF$pkg[ii]
    cls <- rastDF$class[ii]
    # read <- rastDF$read[ii]
    read <- eval(parse(text = rastDF$read[ii]))

    a <- read(aOrig)
    b <- read(bOrig)

    sp <- 0.225
    spRas[] <- spRas[] / reproducible::maxFn(spRas) * sp / 2 + sp / 2 * 1.5
    b[] <- 1
    bb <- focal(b, matrix(1 / 9, nrow = 3, ncol = 3), fun = sum, pad = TRUE, padValue = 0)
    innerCells <- which(bb[] %==% 1)
    sams <- sample(innerCells, 2)

    set.seed(123)
    for (jjj in 1:20) {
      sams <- sample(innerCells, 2)
      out <- spread2(a, start = sams, spreadProb = 0.225, asRaster = FALSE)
      expect_true(length(unique(out$initialPixels)) == 2)
      expect_true(all(out$active == 0))
    }

    # Test numeric vector passed to spreadProb
    sams <- sample(innerCells, 2)
    numNAs <- 25
    sps <- sample(c(rep(NA_real_, numNAs), runif(ncell(a) - numNAs, 0, 1)))
    spsRas <- read(aOrig)
    spsRas[] <- sps
    set.seed(123)
    out1 <- spread2(a, start = sams, spreadProb = sps, asRaster = FALSE)
    set.seed(123)
    out2 <- spread2(a, start = sams, spreadProb = spsRas, asRaster = FALSE)
    expect_true(identical(out1, out2))

    # Test warning for raster on disk
    spsRas[] <- sps
    spsRas <- writeRaster(spsRas, filename = tempfile(fileext = ".tif"))
    warn <- capture_warnings({
      out1 <- spread2(a, start = sams, spreadProb = spsRas, asRaster = FALSE)
    })
    expect_true(grepl("spreadProb is a raster layer stored on disk", warn))

    if (interactive()) message("testing maxSize")
    maxSizes <- 2:3
    for (kkk in 1:20) {
      seed <- sample(1e6, 1)
      set.seed(seed)
      sams <- sample(innerCells, 2)
      out <- spread2(a, start = sams, spreadProb = 0.225, maxSize = maxSizes, asRaster = FALSE)
      expect_true(all(out[, .N, by = "initialPixels"]$N <= maxSizes[order(sams)]))
    }

    if (interactive()) message("testing exactSize")
    exactSizes <- c(5, 3.1)
    for (mmm in 1:20) {
      sams <- sample(innerCells, 2)
      out <- spread2(
        a, start = sams, spreadProb = 0.225, exactSize = exactSizes, asRaster = FALSE
      )
      attrib <- attr(out, "spreadState")$cluster$numRetries > 10
      if (any(attrib)) {
        frequ <- out[, .N, by = "initialPixels"]$N
        expect_true(all(frequ[attrib] <= floor(exactSizes[order(sams)][attrib])))
        expect_true(all(frequ[!attrib] == floor(exactSizes[order(sams)][!attrib])))
      } else {
        expect_true(all(out[, .N, by = "initialPixels"]$N == floor(exactSizes[order(sams)])))
      }
    }

    if (interactive()) message("testing exactSize")
    exactSizes <- c(5.01, 3.1, 4)
    for (nnn in 1:20) {
      sams <- sample(innerCells, length(exactSizes))
      out <- spread2(a, start = sams, spreadProb = 0.225, exactSize = exactSizes, asRaster = FALSE)
      attrib <- attr(out, "spreadState")$clusterDT$numRetries > 10
      if (any(attrib)) {
        frequ <- out[, .N, by = "initialPixels"]$N
        expect_true(all(frequ[attrib] <= floor(exactSizes[order(sams)][attrib])))
        expect_true(all(frequ[!attrib] == floor(exactSizes[order(sams)][!attrib])))
      } else {
        expect_true(all(out[, .N, by = "initialPixels"]$N == floor(exactSizes[order(sams)])))
      }
    }

    if (interactive()) message("testing exact maxSize, can't be achieved, allow jumping")
    exactSizes <- c(154, 111, 134) # too big for landscape, can't achieve it --
    #  will hit max numRetries, and will try jumping
    for (rrr in 1:20) {
      seed <- sample(1e6, 1)
      set.seed(seed)
      sams <- sample(innerCells, 3)
      out <- spread2(a, start = sams, spreadProb = 0.225, exactSize = exactSizes, asRaster = FALSE)
      expect_true(all(out[, .N, by = "initialPixels"]$N < exactSizes))
      expect_true(all(out$numRetries == 11)) # current max
    }

    if (interactive()) message("test circle = TRUE")
    for (ppp in 1:20) {
      message(ppp)
      seed <- sample(1e6, 1)
      set.seed(seed)
      sams <- sample(innerCells, length(sams))

      expect_error(spread2(a, start = sams, spreadProb = runif(1, 1.00000001, 1e4),
                           circle = TRUE, asRaster = FALSE, plot.it = TRUE))
      expect_error(spread2(a, start = sams, spreadProb = runif(1, -1e5, -0.00000001, 1e4),
                           circle = TRUE, asRaster = FALSE, plot.it = TRUE))
      out <- spread2(a, start = sams, spreadProb = 1, circle = TRUE, asRaster = FALSE)
      expect_true(is.numeric(out$distance))
      expect_true(NROW(out) == ncell(a))
    }

    # test circle
    sams <- sort(sample(innerCells, 3)) # sorted -- makes comparisons later easier
    out <- spread2(a, start = sams, spreadProb = 1, circle = TRUE, asRaster = FALSE,
                   returnDistances = TRUE)
    expect_true(NROW(out) == ncell(a))
    expect_true(all(out$state == "inactive"))
    expect_true(all(out$distance <= (sqrt(2) * ncol(a))))

    out <- spread2(a, start = sams, spreadProb = 1, circle = TRUE, allowOverlap = TRUE,
                   asRaster = FALSE, returnDistances = TRUE)
    expect_true(NROW(out) == ncell(a) * length(sams))
    expect_true(all(out$state == "inactive"))
    expect_true(all(out$distance <= (sqrt(2) * ncol(a))))

    setkey(out, initialPixels, distance)

    if (interactive()) {
      count <- 1
      for (ids in unique(out$initialPixels)) {
        # dev(3 + count)
        count <- count + 1
        ras <- read(aOrig)
        ras[] <- 0
        ras[out[initialPixels == ids, pixels]] <- out[initialPixels == ids, distance]
        # clearPlot()
        terra::plot(ras)#Plot(ras)
      }
    }

    if (interactive()) message("compare spread2 circle with cir circle")
    cirOut <- data.table(
      cir(a, allowOverlap = TRUE, loci = sams, minRadius = 0, maxRadius = 15,
          returnDistances = TRUE, simplify = TRUE)
    )
    if (interactive()) {
      for (ids in seq(unique(cirOut$id))) {
        # dev(3 + ids)
        ras[cirOut[id == ids, indices]] <- cirOut[id == ids, dists]
        # clearPlot()
        terra::plot(ras) # Plot(ras)
      }
    }
    cirOut$dists <- round(cirOut$dists, 4)
    out$distance <- round(out$distance, 4)
    setkey(cirOut, id, dists)
    #quickDT <- data.table(id = seq_along(sams), initialPixels = sams, key = "id")
    cirOut <- unique(cirOut)
    #cirOut <- quickDT[cirOut, on = ]
    setnames(cirOut, "id", "initialPixels")
    compare <- out[cirOut, on = c(initialPixels = "initialPixels", pixels = "indices")]
    expect_true(sum(abs(compare$dists - compare$distance)) %==% 0)

    ## TODO: need better test for hov this scales
    #if (interactive()) message("Scales with number of starts, not maxSize of raster")
    #set.seed(21)
    #b <- terra::rast(terra::ext(0, 33000, 0, 33000), res = 1)
    #sams <- sample(ncell(b), 2)
    #st1 <- system.time({
    #  out <- spread2(b, start = sams, spreadProb = 0.225, allowOverlap = TRUE, asRaster = FALSE)
    #})
    #expect_true(st1[1] < 1) ## don't check timing as it fluctuates ased on machine load!

    if (interactive()) message("test neighProbs")
    maxSizes <- 14
    sp <- read(aOrig)
    spreadProbOptions <- 1:5
    sp[] <- sample(spreadProbOptions, ncell(sp), replace = TRUE)
    set.seed(2123)
    sams <- sample(innerCells, 2)
    set.seed(321)
    out <- spread2(a, spreadProb = 1, spreadProbRel = sp, start = sams,
                   neighProbs = c(0.7, 0.3), maxSize = maxSizes, asRaster = FALSE)
    expect_true(uniqueN(out) == maxSizes * length(sams))
    expect_true(NROW(out) == maxSizes * length(sams))

    if (interactive()) message("check variable lengths of neighProbs")
    set.seed(29937)
    sams <- sample(innerCells, 2)
    for (www in 1:8) {
      alwaysN <- rep(0, www)
      alwaysN[www] <- 1
      out <- spread2(
        a, spreadProb = 1, spreadProbRel = sp, iterations = 1,start = sams,
        neighProbs = alwaysN, asRaster = FALSE
      )
      expect_true(NROW(out) == (length(alwaysN) * 2 + length(sams)))
    }

    if (interactive()) {
      message(
        paste(
          "Test that when using neighProbs & a Raster of spreadProbs,",
          "the spreadProb raster is followed probabilistically.",
          "This test does only 1 iteration from 2 pixels that are",
          "not interacting with edges or each other"
        )
      )
    }
    sams <- sort(c(0:2 * 3 + 12) + rep(c(0, 30, 60, 90), 3))
    sams <- sams[sams < 90]
    set.seed(654)
    out <- list()
    for (yyy in 1:10) {
      out[[yyy]] <- spread2(a, spreadProbRel = sp, spreadProb = 1, iterations = 1,
                          start = sams, neighProbs = c(1), asRaster = FALSE)
    }
    out <- rbindlist(out)[state == "activeSource"]
    uniquePixels <- out[, list(uniquePix = unique(pixels)), by = "initialPixels"]
    avail <- table(sp[uniquePixels$uniquePix])
    actual <- unname(table(sp[out$pixels]))
    relProbs <- spreadProbOptions / sum(spreadProbOptions)
    aa <- rmultinom(1, size = 1e4, prob = relProbs)[, 1] * unname(avail)
    suppressWarnings({
      cht <- chisq.test(x = cbind(aa, actual))
    })

    #if (as.numeric_version(paste0(R.version$major, ".", R.version$minor)) < "3.6.0") {
    expect_true(cht$p.value > 0.05)
    #} else {
    #  expect_false(cht$p.value > 0.05) ## TODO: is this valid/correct test?
    #}

    message("Scales with number of starts, not maxSize of raster")
    set.seed(21)
    b <- read(bSimple)
    bProb <- read(system.file("extdata", "bProb.tif", package = "SpaDES.tools"))

    set.seed(1232)
    out <- spread2(spreadProb = 0.5, landscape = b, asRaster = FALSE,
                   start = ncell(b) / 2 - ncol(b) / 2, spreadProbRel = bProb,
                   returnFrom = TRUE, neighProbs = c(0.3, 0.7), exactSize = 30)

    set(out, NULL, "relProb", bProb[][out$pixels])
    if (interactive()) out

    if (interactive())
      message("check wide range of spreadProbs and that it makes a RasterLayer")
    set.seed(654)
    rasts <- list()
    for (ccc in 1:20) {
      rasts[[ccc]] <- spread2(a, spreadProb = stats::runif(1, 0, 1))
      expect_s4_class(rasts[[ccc]], cls)
    }
    if (interactive()) {
      names(rasts) <- paste0("ras", 1:20)
      rasts <- eval(parse(text = rastDF$stack[ii]))(rasts)
      terra::plot(rasts)
    }

    if (interactive())
      message("testing iterative calling of spread2")
    set.seed(299)
    sams <- sample(innerCells, 2)
    set.seed(299)
    out <- spread2(a, iterations = 1, start = sams, asRaster = FALSE)
    stillActive <- TRUE
    while (stillActive) {
      stillActive <- any(out$state == "activeSource")
      out <- spread2(a, iterations = 1, start = out, asRaster = FALSE)
    }
    set.seed(299)
    out2 <- spread2(a, start = sams, asRaster = FALSE)
    keyedCols <- c("initialPixels", "pixels")
    expect_equal(out2, out, ignore_attr = TRUE)

    if (interactive())
      message("testing iterative calling of spread2, but asRaster = TRUE")
    set.seed(299)
    sams <- sample(innerCells, 2)
    set.seed(299)
    out1 <- spread2(a, iterations = 1, start = sams, asRaster = TRUE)
    stillActive <- TRUE
    while (stillActive) {
      stillActive <- any(attr(out1, "pixel")$state == "activeSource")
      out1 <- spread2(a, iterations = 1, start = out1, asRaster = TRUE)
    }
    expect_true(identical(out, attr(out1, "pixel")))

    if (interactive())
      message("testing iterative with maxSize")
    set.seed(299)
    seed <- sample(1e6, 1)
    set.seed(seed)
    sams <- sample(innerCells, 2)
    exactSizes <- 5:6
    out <- spread2(a, start = sams, spreadProb = 0.225, iterations = 1,
                   exactSize = exactSizes, asRaster = FALSE)
    for (pip in 1:20) {
      out <- spread2(a, start = out, spreadProb = 0.225, iterations = 1,
                     exactSize = exactSizes, asRaster = FALSE)
    }

    if (interactive())
      message("testing iterative with maxSize -- where needRetry occurs")
    set.seed(299)
    sams <- sample(innerCells, 2)
    exactSizes <- 60:61

    out <- spread2(a, start = sams, spreadProb = 0.225, iterations = 1,
                   exactSize = exactSizes, asRaster = FALSE)
    out2 <- spread2(a, start = sams, spreadProb = 0.225, iterations = 1,
                    exactSize = exactSizes, asRaster = FALSE)
    for (wip in 1:25) {
      out <- spread2(a, start = out, spreadProb = 0.225, iterations = 1,
                     exactSize = exactSizes, asRaster = FALSE)
      attr(out2, "spreadState") <- NULL
      out2 <- spread2(a, start = out2, spreadProb = 0.225, iterations = 1,
                      exactSize = exactSizes, asRaster = FALSE)
    }
    expect_true(is.data.table(out))
    expect_true(is.data.table(out2))
    expect_true(all(attr(out2, "spreadState")$clusterDT$numRetries == 0))
    expect_true(all(attr(out, "spreadState")$clusterDT$numRetries > 10))

    # because loses info on how many retries, it will always be smaller
    expect_true(all(attr(out, "spreadState")$clusterDT$numRetries >
                      attr(out2, "spreadState")$clusterDT$numRetries))

    sams <- c(25, 75)
    set.seed(234)
    out <- spread2(a, start = sams, spreadProb = 0.225, iterations = 1,
                   exactSize = exactSizes, asRaster = FALSE)
    set.seed(234)
    out2 <- spread2(a, start = sams, spreadProb = 0.225, iterations = 1,
                    exactSize = exactSizes, asRaster = FALSE)
    for (jij in 1:4) {
      # limit this so it doesn't get into retries, which will cause them to differ
      set.seed(234)
      out <- spread2(a, start = out, spreadProb = 0.225, iterations = 1,
                     exactSize = exactSizes, asRaster = FALSE)

      attr(out2, "spreadState") <- NULL
      set.seed(234)
      out2 <- spread2(a, start = out2, spreadProb = 0.225, iterations = 1,
                      exactSize = exactSizes, asRaster = FALSE)
    }

    ## they start to diverge if there is a jump that occurs, because the one without
    ## memory doesn't know how many retries it has had
    #expect_identical(data.table(out2), data.table(out)) ## TODO: fix this test

    for (eoe in 1:25) {
      set.seed(234)
      out <- spread2(a, start = out, spreadProb = 0.225, iterations = 1,
                     exactSize = exactSizes, asRaster = FALSE)

      attr(out2, "spreadState") <- NULL
      set.seed(234)
      out2 <- spread2(a, start = out2, spreadProb = 0.225, iterations = 1,
                      exactSize = exactSizes, asRaster = FALSE)
    }
    expect_false(identical(data.table(out2), data.table(out)))
  }
})

test_that("spread2 tests -- asymmetry", {
  testInit("terra")
  rastDF <- needTerraAndRaster()

  aOrig <- terra::rast(terra::ext(0, 100, 0, 100), res = 1)

  for (ii in seq_len(NROW(rastDF))) {
    read <- eval(parse(text = rastDF$read[ii]))

    # inputs for x
    b <- read(aOrig)
    a <- read(aOrig)
    b[] <- 1
    bb <- focal(b, matrix(1 / 9, nrow = 3, ncol = 3), fun = sum, pad = TRUE, padValue = 0)
    innerCells <- which(bb[] %==% 1)

    set.seed(123)
    sams <- sample(innerCells, 2)
    out <- spread2(a, start = sams, 0.215, asRaster = FALSE, asymmetry = 2,
                   asymmetryAngle = 90)
    for (eof in 1:20) {
      expect_silent({
        out <- spread2(a, start = out, 0.215, asRaster = FALSE, asymmetry = 2,
                       asymmetryAngle = 90)
      })
    }

    hab <- read(system.file("extdata", "hab.tif", package = "SpaDES.tools"))
    names(hab) <- "hab"
    hab2 <- hab > 0
    maxRadius <- 25
    maxVal <- 50
    set.seed(53432)

    startCells <- as.integer(sample(1:ncell(hab), 1))

    n <- 16
    avgAngles <- numeric(n)
    lenAngles <- numeric(n)

    # function to calculate mean angle -- returns in degrees
    meanAngle <- function(angles) {
      deg2(atan2(mean(sin(rad2(angles))), mean(cos(rad2(angles)))))
    }

    # if (interactive()) {
    #   # dev()
    #   # clearPlot()
    # }
    seed <- sample(1e6, 1)
    set.seed(seed)
    for (asymAng in (2:n)) {
      circs <- spread2(hab, spreadProb = 0.25, start = ncell(hab) / 2 - ncol(hab) / 2,
                       asymmetry = 40, asymmetryAngle = asymAng * 20, asRaster = FALSE)
      ci <- read(hab)
      ci[] <- 0
      ci[circs$pixels] <- circs$initialPixels
      ciCentre <- read(ci)
      ciCentre[] <- 0
      ciCentre[unique(circs$initialPixels)] <- 1
      newName <- paste0("ci", asymAng * 20)
      assign(newName, ci)

      where2 <- function(name, env = parent.frame()) {
        # simplified from pryr::where
        if (exists(name, env, inherits = FALSE)) env else where2(name, parent.env(env))
      }
      env <- where2(newName)
      if (interactive()) {
        objToPlot <- get(newName, envir = env)
        terra::plot(objToPlot) #, add = TRUE)
        ciCentre[ciCentre == 0] <- NA
        terra::plot(ciCentre, add = TRUE, col = "black")
      }
      a <- cbind(id = circs$initialPixels, to = circs$pixels, xyFromCell(hab, circs$pixels))
      initialLociXY <- cbind(id = unique(circs$initialPixels),
                             xyFromCell(hab, unique(circs$initialPixels)))
      dirs <- directionFromEachPoint(from = initialLociXY, to = a)
      dirs[, "angles"] <- deg2(dirs[, "angles"])
      avgAngles[asymAng] <- tapply(dirs[, "angles"], dirs[, "id"], meanAngle) %% 360
      lenAngles[asymAng] <- tapply(dirs[, "angles"], dirs[, "id"], length)
    }

    whBig <- which(lenAngles > 50)
    pred <- (1:n)[whBig] * 20
    expect_true(abs(coef(lm(avgAngles[whBig] ~ pred))[[2]] - 1) < 0.1)

    # test that the events spread to the middle
    # Create a raster with one point at the centre
    ciCentre <- read(hab)

    utm <- "+proj=utm +zone=28 +datum=WGS84 +units=m +no_defs"
    terra::crs(ciCentre) <- utm

    # terra::crs(ciCentre) <- "epsg:23028"
    ciCentre <- setValues(ciCentre, 1)
    ciCentre[seq_len(ncell(ciCentre))[-(ncell(ciCentre) / 2 - ncol(ciCentre) / 2)]] <- NA_integer_
    # create a direction raster with all points leading to that point
    directionRas <- direction(ciCentre)
    directionRas[] <- deg2(directionRas[])

    seed <- 4406
    set.seed(seed)
    sams <- ncol(directionRas) + 2
    circs <- spread2(hab, spreadProb = 0.265, start = sams, asymmetry = 300,
                     asymmetryAngle = directionRas, asRaster = TRUE)
    circs2 <- spread2(hab, spreadProb = 0.265, start = sams, asRaster = TRUE)
    if (interactive()) {
      terra::plot(circs)
      ciCentrePlot <- ciCentre
      ciCentrePlot[ciCentrePlot == 2] <- NA
      ciCentrePlot[sams] <- 2
      ciCentrePlot[ciCentrePlot == 0] <- NA
      terra::plot(ciCentrePlot, add = TRUE, col = c("black", "red"))
      terra::plot(circs2, add = TRUE, col = "#1211AA33")
      # Plot(ciCentrePlot, cols = c("transparent", "black", "red"), addTo = "circs")
      # Plot(circs2, addTo = "circs", col = "#1211AA33")
    }
    #test whether it stopped before hitting the whole map
    expect_true(sum(circs[], na.rm = TRUE) < ncell(circs))

    if (as.numeric_version(paste0(R.version$major, ".", R.version$minor)) < "3.6.0") {
      #test that it reached the centre, but not circs2 that did not have directionality
      expect_equal(circs[sams], circs[which(ciCentre[] == 1)]) ## TODO: restore this test
    }
    expect_true(is.na(circs2[ciCentre == 1]))
    expect_true(!is.na(circs2[sams]))

    # Here, test that the asymmetry version, with adjusted downward spreadProb is creating the
    #  same size events as the Non-asymmetry one. This is a weak test, really. It should
    sizes <- data.frame(a = numeric())
    set.seed(1234)
    for (rso in 1:10) {
      sams <- ncell(hab) / 4 - ncol(hab) / 4 * 3
      circs <- spread2(hab, spreadProb = 0.18, start = sams,
                       asymmetry = 2, asymmetryAngle = 135, asRaster = TRUE)
      sizes <- rbind(sizes, cbind(a = attr(circs, "pixel")[, .N]))
      if (FALSE) {
        Plot(circs, new = TRUE)
        ciCentre[ciCentre == 2] <- NA
        ciCentre[sams] <- 2
        Plot(ciCentre, cols = c("black", "red"), addTo = "circs")
        Plot(circs2, addTo = "circs", cols = "#1211AA33")
      }
    }

    ttestOut <- t.test(sizes$a, mu = 994)
    expect_true(ttestOut$p.value > 0.05)

    next # the following isn't tested
    if (!interactive()) next
    # This code is used to get the mean value for the t.test above
    n <- 100
    sizes <- integer(n)
    for (iwo in 1:n) {
      circs <- spread2(hab, spreadProb = 0.225,
                       start = ncell(hab) / 4 - ncol(hab) / 4 * 3,
                       asRaster = FALSE)
      sizes[iwo] <- circs[, .N]
    }
    goalSize <- mean(sizes)

    library(parallel)
    # only need 10 cores for 10 populations in DEoptim
    cl <- makeCluster(pmin(10, detectCores() - 2))
    parallel::clusterEvalQ(cl, {
      library(SpaDES.tools)
      library(raster)
      library(fpCompare)
    })

    objFn <- function(sp, n = 20, ras, goalSize) {
      sizes <- integer(n)
      for (wnn in 1:n) {
        circs <- spread2(ras, spreadProb = sp,
                         start = ncell(ras) / 4 - ncol(ras) / 4 * 3,
                         asymmetry = 2, asymmetryAngle = 135,
                         asRaster = FALSE)
        sizes[wnn] <- circs[, .N]
      }
      abs(mean(sizes) - goalSize)
    }
    aa <- DEoptim(objFn, lower = 0.2, upper = 0.23,
                  control = DEoptim.control(
                    cluster = cl, NP = 10, VTR = 0.02,
                    initialpop = as.matrix(rnorm(10, 0.213, 0.001))
                  ),
                  ras = hab, goalSize = goalSize)

    # The value of spreadProb that will give the same expected event sizes to spreadProb = 0.225 is:
    sp <- aa$optim$bestmem
    circs <- spread2(ras, spreadProb = sp, start = ncell(ras) / 4 - ncol(ras) / 4 * 3,
                     asymmetry = 2, asymmetryAngle = 135, asRaster = FALSE)

    ####### Calibration curve
    skip("Calibration curves")
    # n <- 500
    # ras <- terra::rast(terra::ext(0, 1000, 0, 1000), res = 1)
    # sp <- runif(n, 0.15, 0.25)
    # sizes <- integer()
    # for (qwe in 1:n) {
    #   circs <- spread2(ras, spreadProb = sp[qwe], start = ncell(ras) / 2 - ncol(ras) / 2,
    #                    asRaster = FALSE)
    #   sizes[qwe] <- NROW(circs)
    #   message(qwe)
    # }
    # dt1 <- data.table(sp, sizes)
    # library(mgcv)
    # aa <- gam(log10(dt1$sizes) ~ s(dt1$sp))
    # aap <- predict(aa, se.fit = FALSE)
    # plot(dt1$sp, log10(dt1$sizes), axes = FALSE, ylab = "Fire Size, ha", xlab = "Spread Probability")
    # axis(2, 0:5, labels = 10 ^ (0:5))
    # axis(1)
    # aapOrd <- order(dt1$sp)
    # lines(dt1$sp[aapOrd], aap[aapOrd], lwd = 2, col = "red")
    # mtext(side = 3, paste("Resulting fire sizes, for given spread probabilities",
    #                       "Red line shows expected size", sep = "\n"))
    #
    # aa1 <- gam(dt1$sp ~ s(log10(dt1$sizes)))
    # aap1 <- predict(aa1, se.fit = FALSE, type = "response")
    # plot(log10(dt1$sizes), dt1$sp, axes = FALSE, xlab = "Fire Size, ha", ylab = "Spread Probability")
    # axis(2)
    # axis(1, 0:5, labels = 10 ^ (0:5))
    # aap1Ord <- order(log10(dt1$sizes))
    # lines(log10(dt1$sizes)[aap1Ord], aap1[aap1Ord], lwd = 2, col = "red")
    # mtext(side = 3, paste("Resulting fire sizes, for given spread probabilities",
    #                       "Red line shows expected size", sep = "\n"))
  }
})

test_that("spread2 returnFrom", {
  testInit("terra")
  rastDF <- needTerraAndRaster()

  aOrig <- terra::rast(terra::ext(0, 100, 0, 100), res = 1)

  for (ii in seq_len(NROW(rastDF))) {
    read <- eval(parse(text = rastDF$read[ii]))

    # inputs for x
    a <- read(aOrig)
    b <- read(aOrig)
    b[] <- 1
    bb <- focal(b, matrix(1 / 9, nrow = 3, ncol = 3), fun = sum, pad = TRUE, padValue = 0)
    innerCells <- which(bb[] %==% 1)

    set.seed(123)
    for (iso in 1:20) {
      sams <- sample(innerCells, 2)
      expect_silent(out <- spread2(a, start = sams, 0.215, asRaster = FALSE,
                                   returnFrom = TRUE))
      out <- spread2(a, start = sams, 0.215, asRaster = FALSE, returnFrom = TRUE)
      expect_true("from" %in% colnames(out))
      expect_true(sum(is.na(out$from)) == length(sams))
    }
  }
})

test_that("spread2 tests", {
  testInit("terra")
  rastDF <- needTerraAndRaster()

  aOrig <- terra::rast(terra::ext(0, 100, 0, 100), res = 1)

  for (ii in seq_len(NROW(rastDF))) {
    read <- eval(parse(text = rastDF$read[ii]))
    a <- read(aOrig)
    b <- read(aOrig)

    b[] <- 1
    bb <- focal(b, matrix(1 / 9, nrow = 3, ncol = 3), fun = sum, pad = TRUE, padValue = 0)
    innerCells <- which(bb[] %==% 1)
    sams <- sample(innerCells, 9)

    # dev()
    expect_no_error({
      out <- spread2(a, start = sams, 1, circle = TRUE, asymmetry = 4,
                     asymmetryAngle = 120, iterations = 10, asRaster = FALSE,
                     returnDistances = TRUE, allowOverlap = TRUE)
    })
    expect_true("effectiveDistance" %in% colnames(out))
    expect_true(all(out$state == "activeSource"))
    expect_true(all(out$distance[out$distance > 0] <= out$effectiveDistance[out$distance > 0]))

  }

})

test_that("spread2 works with terra", {
  testInit("terra")
  rastDF <- needTerraAndRaster()

  aOrig <- terra::rast(terra::ext(0, 100, 0, 100), res = 1)

  for (ii in seq_len(NROW(rastDF))) {
    read <- eval(parse(text = rastDF$read[ii]))
    a <- read(aOrig)
    b <- read(aOrig)

    # inputs for x
    b[] <- 1
    bb <- focal(b, matrix(1 / 9, nrow = 3, ncol = 3), fun = sum, pad = TRUE, padValue = 0)
    innerCells <- which(bb[] %==% 1)
    sams <- sample(innerCells, 9)

    expect_silent({
      out <- spread2(a, start = sams, 1, iterations = 1, asRaster = FALSE)
    })
    # TODO: add more tests once asymmetry, circle, etc works
    expect_true(all(out[pixels %in% sams]$state == "inactive"))
    expect_true(any("activeSource" %in% out$state))

  }
})

test_that("spread2 tests -- persistence", {
  testInit("terra")
  rastDF <- needTerraAndRaster()

  aOrig <- terra::rast(terra::ext(0, 50, 0, 50), res = 1)

  for (ii in seq_len(NROW(rastDF))) {
    read <- eval(parse(text = rastDF$read[ii]))
    a <- read(aOrig)
    b <- read(aOrig)
    landscape <- read(aOrig)
    landscape[] <- 1
    start <- 1:5

    ## test the effect of persistence as a single numeric value
    set.seed(5)
    noPersist <- spread2(landscape = landscape, start = start, asRaster = FALSE,
                         spreadProb = 0.23, persistProb = 0, iterations = 10, directions = 8L, plot.it = FALSE)
    wPersist <- spread2(landscape = landscape, start = start, asRaster = FALSE,
                        spreadProb = 0.23, persistProb = 0.8, iterations = 10, directions = 8L, plot.it = FALSE)

    expect_true(sum(noPersist$state == "activeSource") < sum(wPersist$state == "activeSource"))

    ## test the effect of persistence as a raster layer
    M <- matrix(0.8, nrow = 50, ncol = 50)
    M[upper.tri(M)] <- 0
    persistRas <- read(aOrig)
    persistRas[] <- as.vector(M)

    ## first fire in high persistence area,
    ## second fire in low persistence area:
    start <- c(50, ncell(landscape) - 49)

    set.seed(5)
    wRasPersist <- spread2(landscape = landscape, start = start,
                           spreadProb = 0.23, persistProb = persistRas, iterations = 10,
                           directions = 8L, asRaster = TRUE, plot.it = FALSE)

    expect_true(sum(wRasPersist[] == 1, na.rm = TRUE) > sum(wRasPersist[] == 2, na.rm = TRUE))
  }
})

test_that("spread2 tests -- SpaDES.tools issue #22 NA in spreadProb", {
  testInit("terra")
  rastDF <- needTerraAndRaster()

  aOrig <- terra::rast(terra::ext(0, 50, 0, 50), res = 1)

  for (ii in seq_len(NROW(rastDF))) {
    read <- eval(parse(text = rastDF$read[ii]))
    landscape <- read(aOrig)
    landscape[] <- 1
    landscape[51:55] <- NA
    start <- 1:5
    spreadProb <- landscape
    spreadProb[!is.na(landscape[])] <- runif(sum(!is.na(landscape[])))
    expect_silent(
      spread2(landscape = landscape, spreadProb = spreadProb, start = start, plot.it = FALSE)
    )
  }
})
