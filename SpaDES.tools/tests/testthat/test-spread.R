test_that("spread produces legal raster", {
  testInit(c("dqrng", "raster", "terra", "withr"))
  rastDF <- needTerraAndRaster()

  for (ii in seq_len(NROW(rastDF))) {
    type <- rastDF$pkg[ii]
    cls <- rastDF$class[ii]
    pkg <- rastDF$read[ii]
    read <- eval(parse(text = rastDF$read[ii]))

    a <- terra::rast(terra::ext(0, 20, 0, 20), resolution = 1)

    withr::local_seed(125)
    b <- terra::rast(terra::ext(a), resolution = 1, vals = stats::runif(ncell(a), 0, 1))

    a <- read(a)
    b <- read(b)

    ## check it makes a RasterLayer or SpatRaster
    expect_s4_class(spread(a, loci = ncell(a) / 2, stats::runif(1, 0.15, 0.25)), cls)

    ## check wide range of spreadProbs
    for (wwt in 1:20) {
      expect_s4_class(spread(a, loci = ncell(a) / 2, stats::runif(1, 0, 1)), cls)
    }

    ## Test for NAs in a numeric vector of spreadProb values
    numNAs <- 50
    sps <- sample(c(rep(NA_real_, numNAs), runif(ncell(a) - numNAs, 0, 0.5)))
    expect_s4_class(spread(a, loci = ncell(a) / 2, spreadProb = sps), cls)

    ## check spreadProbs outside of legal returns an "spreadProb is not a probability"
    expect_error(spread(a, loci = ncell(a) / 2, 1.1), "spreadProb is not a probability")
    expect_error(spread(a, loci = ncell(a) / 2, -0.1), "spreadProb is not a probability")

    ## checks if maxSize is working properly
    ## One process spreading
    expect_equal(ncell(a), tabulate(spread(a, spreadProb = 1, id = TRUE)[]))

    ## several processes spreading
    sizes <- rep_len(50, 3)
    expect_equal(
      sizes,
      tabulate(spread(a, loci = c(40, 200, 350), spreadProb = 1, id = TRUE, maxSize = sizes)[])
    )

    ## Check that with maxSize, the active cells are removed when maxSize is reached
    b <- terra::rast(terra::ext(0, 20, 0, 20), resolution = 1)
    if (pkg == "raster::raster") {
      a <- read(a)
    }

    loci <- sample(ncell(b), size = 1)
    maxSize1 <- 1e2
    spreadProb <- 0.27

    ## test results are RNG seed dependent;
    ## testthat makes calls to e.g., sample() that affect seed;
    ## newer testthat versions were triggering failures with prev seed (#98)
    withr::local_seed(9194)
    spreadState <- spread(
      landscape = b,
      loci = loci,
      spreadProb = spreadProb,
      returnIndices = TRUE,
      maxSize = maxSize1
    )

    expect_true(length(unique(spreadState[["indices"]])) <= maxSize1)
    expect_true(length(spreadState[["indices"]]) <= maxSize1)

    ## Test that spreadState with a data.table works
    fires <- list()
    fires[[1]] <- spread(
      a,
      loci = as.integer(sample(1:ncell(a), 10)),
      returnIndices = TRUE,
      spreadProb = 0.235,
      persistence = 0,
      mask = NULL,
      maxSize = 1e8,
      8,
      iterations = 2,
      id = TRUE
    )
    stopped <- list()
    stopped[[1]] <- fires[[1]][, sum(active), by = id][V1 == 0, id]
    for (wwn in 2:4) {
      j <- sample(1:1000, 1)
      withr::local_seed(j)
      fires[[wwn]] <- spread(
        a,
        loci = as.integer(sample(1:ncell(a), 10)),
        returnIndices = TRUE,
        spreadProb = 0.235,
        0,
        NULL,
        1e8,
        8,
        iterations = 2,
        id = TRUE,
        spreadState = fires[[wwn - 1]]
      )
      stopped[[wwn]] <- fires[[wwn]][, sum(active), by = id][V1 == 0, id]

      ## Test that any fire that stopped previously is not rekindled
      expect_true(all(stopped[[wwn - 1]] %in% stopped[[wwn]]))
    }

    ## Test that passing NA to loci returns a correct data.table
    withr::local_seed(125)

    fires <- spread(
      a,
      loci = as.integer(sample(1:ncell(a), 10)),
      returnIndices = TRUE,
      0.235,
      0,
      NULL,
      1e8,
      8,
      iterations = 2,
      id = TRUE
    )
    fires2 <- spread(
      a,
      loci = NA_real_,
      returnIndices = TRUE,
      0.235,
      0,
      NULL,
      1e8,
      8,
      iterations = 2,
      id = TRUE,
      spreadState = fires
    )
    expect_true(all(fires2[, unique(id)] %in% fires[, unique(id)]))
    expect_true(all(fires[, unique(id)] %in% fires2[, unique(id)]))

    if (getRversion() <= "4.5.0" && tolower(.Platform$OS.type) != "windows") {
      if (packageVersion("dqrng") < "0.4.0") {
        expect_true(all(
          fires2[, length(initialLocus), by = id][, V1] ==
            c(15L, 11L, 8L, 5L, 16L, 7L, 2L, 8L, 6L, 17L)
        ))
      } else {
        expect_true(all(
          fires2[, length(initialLocus), by = id][, V1] ==
            c(13L, 8L, 3L, 4L, 8L, 4L, 5L, 17L, 6L, 17L)
        ))
      }
    }
  }
})

test_that("allowOverlap -- produces exact result", {
  testInit(c("dqrng", "terra", "withr"))
  rastDF <- needTerraAndRaster()
  N <- 10
  smallExt <- terra::ext(1, N - 1, 1, N - 1)
  smallExtRas <- terra::rast(smallExt)
  lrgExt <- terra::extend(smallExt, 1)
  a <- terra::rast(lrgExt, resolution = 1)

  for (ii in seq_len(NROW(rastDF))) {
    type <- rastDF$pkg[ii]
    cls <- rastDF$class[ii]
    pkg <- rastDF$read[ii]
    read <- eval(parse(text = rastDF$read[ii]))

    if (pkg == "raster::raster") {
      a <- read(a)
      smallExtRas <- read(smallExtRas)
    }

    ao <- c(FALSE, TRUE)
    mp <- middlePixel(a)
    mps <- mp + (-3:3)

    # ._spread_3 <- ._spread_19 <-._spread_14 <- 1;
    b <- list()
    withr::local_seed(123445)
    Nreps <- 100
    sams <- sample(1e7, Nreps)
    for (jjj in seq_along(ao)) {
      b[[jjj]] <- list()
      for (j in seq_len(Nreps)) {
        withr::local_seed(sams[j])
        b[[jjj]][[j]] <- spread(
          a,
          loci = mp,
          spreadProb = 0.22,
          id = TRUE,
          allowOverlap = ao[jjj],
          returnIndices = TRUE
        )
      }
    }
    bs <- lapply(b, function(x) rbindlist(x, idcol = "rep"))
    expect_true(all.equal(bs[[1]], bs[[2]]))

    ##################################################
    b <- list()
    Nreps <- 100
    sams <- sample(1e7, Nreps)

    #._spread_14 <- 1
    for (wte in seq_along(ao)) {
      b[[wte]] <- list()
      for (j in seq_len(Nreps)) {
        withr::local_seed(sams[j])
        b[[wte]][[j]] <- spread(
          a,
          loci = mps,
          spreadProb = 0.22,
          id = TRUE,
          allowOverlap = ao[wte],
          returnIndices = TRUE
        )
      }
    }
    bs <- lapply(b, function(x) rbindlist(x, idcol = "rep"))
    ras <- list()
    for (wti in seq_along(bs)) {
      ras[[wti]] <- read(a)
      ras[[wti]][] <- 0
      v <- bs[[wti]][, .N, by = "indices"]
      ras[[wti]][v$indices] <- v$N
    }
    if (pkg == "raster::raster") {
      stk <- raster::stack(ras)
    } else {
      stk <- terra::rast(ras)
    }
    stk <- terra::crop(stk, smallExtRas)
    if (pkg == "raster::raster") {
      o <- raster::calc(stk, function(x) x[2] >= x[1])
    } else {
      o <- terra::app(stk, function(x) x[2] >= x[1])
    }

    expect_true(sum(o[] == 1) > (ncell(stk) - 10))
  }
})

test_that("spread stopRule does not work correctly", {
  testInit(c("terra", "withr"))
  rastDF <- needTerraAndRaster()

  for (ii in seq_len(NROW(rastDF))) {
    type <- rastDF$pkg[ii]
    cls <- rastDF$class[ii]
    pkg <- rastDF$read[ii]
    read <- eval(parse(text = rastDF$read[ii]))

    hab <- read(system.file("extdata", "hab.tif", package = "SpaDES.tools"))
    names(hab) <- "hab"
    hab2 <- hab > 0
    maxRadius <- 25
    maxVal <- 50

    ## stopRule examples
    ##  examples with stopRule, which means that the eventual size is driven by the
    ##  values on the raster passed in to the landscape argument
    withr::local_seed(1234)
    startCells <- as.integer(sample(1:ncell(hab), 10))
    stopRule1 <- function(landscape) sum(landscape) > maxVal
    stopRuleA <- spread(
      hab,
      loci = startCells,
      spreadProb = 1,
      persistence = 0,
      mask = NULL,
      maxSize = 1e6,
      directions = 8,
      iterations = 1e6,
      id = TRUE,
      circle = TRUE,
      stopRule = stopRule1
    )
    foo <- cbind(vals = as.numeric(hab[stopRuleA > 0]), id = as.numeric(stopRuleA[stopRuleA > 0]))
    expect_true(all(tapply(foo[, "vals"], foo[, "id"], sum) > maxVal))

    ## using stopRuleBehavior = "excludePixel"
    withr::local_seed(1234)
    stopRuleB <- spread(
      hab,
      loci = startCells,
      1,
      0,
      NULL,
      maxSize = 1e6,
      8,
      1e6,
      id = TRUE,
      circle = TRUE,
      stopRule = stopRule1,
      stopRuleBehavior = "excludePixel"
    )
    foo <- cbind(vals = as.numeric(hab[stopRuleB > 0]), id = as.numeric(stopRuleB[stopRuleB > 0]))
    expect_true(all(tapply(foo[, "vals"], foo[, "id"], sum) <= maxVal))

    ## If boolean, then it is exact
    stopRuleB <- spread(
      hab2,
      loci = startCells,
      1,
      0,
      NULL,
      maxSize = 1e6,
      8,
      1e6,
      id = TRUE,
      circle = TRUE,
      stopRule = stopRule1,
      stopRuleBehavior = "excludePixel"
    )
    foo <- cbind(vals = as.numeric(hab2[stopRuleB]), id = as.numeric(stopRuleB[stopRuleB > 0]))
    expect_true(all(tapply(foo[, "vals"], foo[, "id"], sum) == maxVal))

    ## Test vector maxSize and stopRule when they interfere
    maxSizes <- sample(maxVal * 2, length(startCells))
    stopRuleB <- spread(
      hab2,
      loci = startCells,
      1,
      0,
      NULL,
      maxSize = maxSizes,
      8,
      1e6,
      id = TRUE,
      circle = TRUE,
      stopRule = stopRule1,
      stopRuleBehavior = "excludePixel"
    )
    if (interactive()) {
      terra::plot(stopRuleB, new = TRUE)
    }
    foo <- cbind(vals = as.numeric(hab2[stopRuleB > 0]), id = as.numeric(stopRuleB[stopRuleB > 0]))
    expect_true(all(tapply(foo[, "vals"], foo[, "id"], sum) == pmin(maxSizes, maxVal)))

    ## Test non integer maxSize and stopRule when they interfere
    maxSizes <- runif(length(startCells), 1, maxVal * 2)
    stopRuleB <- spread(
      hab2,
      loci = startCells,
      1,
      0,
      NULL,
      maxSize = maxSizes,
      8,
      1e6,
      id = TRUE,
      circle = TRUE,
      stopRule = stopRule1,
      stopRuleBehavior = "excludePixel"
    )
    if (interactive()) {
      terra::plot(stopRuleB, new = TRUE)
    }
    foo <- cbind(vals = as.numeric(hab2[stopRuleB > 0]), id = as.numeric(stopRuleB[stopRuleB > 0]))
    expect_true(all(tapply(foo[, "vals"], foo[, "id"], sum) == pmin(floor(maxSizes), maxVal)))

    ## Test for stopRuleBehavior ------------------------------------------------
    withr::local_seed(53432)
    stopRule2 <- function(landscape) sum(landscape) > maxVal
    startCells <- as.integer(sample(1:ncell(hab), 2))

    withr::local_seed(53432)
    circs <- spread(
      hab,
      spreadProb = 1,
      circle = TRUE,
      loci = startCells,
      id = TRUE,
      stopRule = stopRule2,
      stopRuleBehavior = "includeRing"
    )
    cirs <- as.numeric(terra::values(circs))
    vals <- tapply(hab[circs > 0], cirs[cirs > 0], sum)
    expect_true(all(vals >= maxVal))

    withr::local_seed(53432)
    circs2 <- spread(
      hab,
      spreadProb = 1,
      circle = TRUE,
      loci = startCells,
      id = TRUE,
      stopRule = stopRule2,
      stopRuleBehavior = "excludeRing"
    )
    cirs <- as.numeric(terra::values(circs2))
    vals <- tapply(hab[circs2 > 0], cirs[cirs > 0], sum)
    expect_true(all(vals <= maxVal))

    withr::local_seed(53432)
    circs3 <- spread(
      hab,
      spreadProb = 1,
      circle = TRUE,
      loci = startCells,
      id = TRUE,
      stopRule = stopRule2,
      stopRuleBehavior = "includePixel"
    )
    cirs <- as.numeric(terra::values(circs3))
    vals <- tapply(hab[circs3 > 0], cirs[cirs > 0], sum)
    expect_true(all(vals <= (maxVal + reproducible::maxFn(hab))))

    withr::local_seed(53432)
    circs4 <- spread(
      hab,
      spreadProb = 1,
      circle = TRUE,
      loci = startCells,
      id = TRUE,
      stopRule = stopRule2,
      stopRuleBehavior = "excludePixel"
    )
    cirs <- as.numeric(terra::values(circs4))
    vals <- tapply(hab[circs4 > 0], cirs[cirs > 0], sum)
    expect_true(all(vals >= (maxVal - reproducible::maxFn(hab))))

    ## There should be 1 extra cell
    expect_true(
      sum(as.numeric(terra::values(circs4)) > 0) + length(startCells) ==
        sum(as.numeric(terra::values(circs3)) > 0)
    )
    ## Order should be includeRing, includePixel, excludePixel, excludeRing
    expect_true(
      sum(as.numeric(terra::values(circs)) > 0) > sum(as.numeric(terra::values(circs3)) > 0)
    )
    expect_true(
      sum(as.numeric(terra::values(circs3)) > 0) > sum(as.numeric(terra::values(circs4)) > 0)
    )
    expect_true(
      sum(as.numeric(terra::values(circs4)) > 0) > sum(as.numeric(terra::values(circs2)) > 0)
    )

    ## Test for circles using maxDist -------------------------------------------

    withr::local_seed(53432)
    stopRule2 <- function(landscape) sum(landscape) > maxVal
    startCells <- as.integer(sample(1:ncell(hab), 1))

    circs <- spread(
      hab2,
      spreadProb = 1,
      circle = TRUE,
      loci = startCells,
      id = TRUE,
      circleMaxRadius = maxRadius
    )
    cells <- which(as.numeric(terra::values(circs)) == 1)
    centre <- terra::xyFromCell(hab2, startCells)
    allCells <- terra::xyFromCell(hab2, cells)
    pd <- as.numeric(terra::distance(centre, allCells, lonlat = FALSE))
    expect_true(maxRadius == max(pd))

    ## Test for circles using maxDist
    withr::local_seed(543345)
    numCircs <- 4

    withr::local_seed(53432)
    stopRule2 <- function(landscape) sum(landscape) > maxVal
    startCells <- as.integer(sample(1:ncell(hab), numCircs))

    circs <- spread(
      hab2,
      spreadProb = 1,
      circle = TRUE,
      loci = startCells,
      id = TRUE,
      circleMaxRadius = maxRadius
    )
    if (interactive()) {
      terra::plot(circs, new = TRUE)
    }

    for (whCirc in seq(numCircs)) {
      cells <- which(as.numeric(terra::values(circs)) == whCirc)
      centre <- xyFromCell(hab2, startCells)
      allCells <- xyFromCell(hab2, cells)
      pd <- as.numeric(terra::distance(centre[whCirc, , drop = FALSE], allCells, lonlat = FALSE))
      circEdge <- circs
      circEdge[] <- 0
      circEdge[cells[pd == maxRadius]] <- 1
      expect_true(all(circs[cells[pd == maxRadius]] == whCirc))
      if (!is.null(circs[as.vector(adj(hab2, cells[pd == maxRadius], pairs = FALSE))])) {
        ## Test that there are both 0 and whCirc values, i.e,. it is on an edge
        expect_true(all(
          c(0, whCirc) %in%
            as.numeric(terra::values(circs)[as.vector(adj(
              hab2,
              cells[pd == maxRadius],
              pairs = FALSE
            ))])
        ))
      }
      if (interactive()) {
        circEdge[circEdge == 0] <- NA
        terra::plot(circEdge, add = TRUE, col = rainbow(numCircs)[whCirc])
        # Plot(circEdge, add = TRUE) # addTo = "circs",
        # cols = c("transparent", rainbow(numCircs)[whCirc]))
      }
    }

    ## Test complex functions
    initialLoci <- (ncell(hab) - ncol(hab)) / 2 + c(4, -4)
    endSizes <- seq_along(initialLoci) * 200
    stopRule3 <- function(landscape, id, endSizes) sum(landscape) > endSizes[id]

    twoCirclesDiffSize <- spread(
      hab,
      spreadProb = 1,
      loci = initialLoci,
      circle = TRUE,
      directions = 8,
      id = TRUE,
      stopRule = stopRule3,
      endSizes = endSizes,
      stopRuleBehavior = "excludePixel"
    )
    if (interactive()) {
      terra::plot(twoCirclesDiffSize, new = TRUE)
    }
    cirs <- as.numeric(terra::values(twoCirclesDiffSize))
    vals <- tapply(hab[twoCirclesDiffSize > 0], cirs[cirs > 0], sum)
    expect_true(all(vals < endSizes))

    ## Test allowOverlap
    initialLoci <- as.integer(sample(1:ncell(hab), 10))

    circs <- spread(
      hab2,
      spreadProb = 1,
      circle = TRUE,
      loci = initialLoci,
      id = TRUE,
      circleMaxRadius = maxRadius,
      allowOverlap = TRUE
    )

    circs <- spread(hab2, spreadProb = 1, loci = initialLoci, maxSize = 10, allowOverlap = TRUE)

    circs <- spread(
      hab2,
      spreadProb = 1,
      loci = initialLoci,
      maxSize = seq_along(initialLoci) * 3,
      allowOverlap = TRUE
    )

    ## Test allowOverlap and stopRule
    for (iti in 1:6) {
      maxVal <- sample(10:300, 1)
      stopRule2 <- function(landscape, maxVal) sum(landscape) > maxVal

      circs <- spread(
        hab,
        spreadProb = 1,
        circle = TRUE,
        loci = initialLoci,
        stopRule = stopRule2,
        maxVal = maxVal,
        returnIndices = TRUE,
        id = TRUE,
        allowOverlap = TRUE,
        stopRuleBehavior = "includeRing"
      )

      if (getRversion() >= "4.3.0") {
        ## TODO: misc error on R 4.2:
        ## Error in `tapply(hab[circs$indices], circs$id, sum)`: arguments must have same length
        vals <- tapply(hab[circs$indices], circs$id, sum)
        expect_true(all(vals > maxVal))
      }
    }

    ## stopRuleBehavior the allowOverlap
    maxVal <- 20
    stopRule2 <- function(landscape, maxVal) sum(landscape) > maxVal
    circs <- spread(
      hab,
      spreadProb = 1,
      circle = TRUE,
      loci = initialLoci,
      stopRule = stopRule2,
      maxVal = maxVal,
      returnIndices = TRUE,
      id = TRUE,
      allowOverlap = TRUE,
      stopRuleBehavior = "excludePixel"
    )
    if (getRversion() >= "4.3.0") {
      ## TODO: misc error on R 4.2:
      ## Error in `tapply(hab[circs$indices], circs$id, sum)`: arguments must have same length
      vals <- tapply(hab[circs$indices], circs$id, sum)
      expect_true(all(vals <= maxVal))
    }

    maxVal <- sample(10:100, 10)
    stopRule2 <- function(landscape, id, maxVal) sum(landscape) > maxVal[id]
    circs <- spread(
      hab,
      spreadProb = 1,
      circle = TRUE,
      loci = initialLoci,
      stopRule = stopRule2,
      id = TRUE,
      allowOverlap = TRUE,
      stopRuleBehavior = "excludePixel",
      maxVal = maxVal,
      returnIndices = TRUE
    )
    if (getRversion() >= "4.3.0") {
      ## TODO: misc error on R 4.2:
      ## Error in `tapply(hab[circs$indices], circs$id, sum)`: arguments must have same length
      vals <- tapply(hab[circs$indices], circs$id, sum)
      expect_true(all(vals <= maxVal))
    }

    ## Test that maxSize can be a non integer value (i.e, Real)

    ## Test arbitrary raster as part of stopRule
    ## Stop if sum of landscape is big or mean of quality is too small
    withr::local_seed(4561)
    for (itj in 1:6) {
      initialLoci <- as.integer(sample(seq(ncell(hab)), 10))
      quality <- read(hab)
      quality[] <- runif(ncell(quality), 0, 1)
      sumLandscapeRule <- 100
      meanHabitatRule <- 0.4
      stopRule4 <- function(landscape, quality, cells, sumLandscapeRule, meanHabitatRule) {
        (sum(landscape) > sumLandscapeRule) |
          (mean(terra::values(quality)[cells]) < meanHabitatRule)
      }

      circs <- spread(
        hab,
        spreadProb = 1,
        loci = initialLoci,
        circle = TRUE,
        directions = 8,
        id = TRUE,
        stopRule = stopRule4,
        quality = quality,
        sumLandscapeRule = sumLandscapeRule,
        meanHabitatRule = meanHabitatRule,
        stopRuleBehavior = "includePixel",
        returnIndices = TRUE
      )

      ras <- read(quality)
      ras[] <- 0
      circsVals <- circs[, numEvents := sum(unique(id)), by = indices]
      ras[circsVals$indices] <- circsVals$numEvents
      a1 <- cbind(
        quality = as.numeric(quality[ras > 0]),
        hab = as.numeric(hab[ras > 0]),
        id = as.numeric(ras[ras > 0])
      )
      a2 <- tapply(a1[, "hab"], a1[, "id"], sum)
      a3 <- tapply(a1[, "quality"], a1[, "id"], mean)
      wh <- which(a3 < meanHabitatRule)
      a4 <- tapply(a1[, "quality"], a1[, "id"], length)
      expect_true(all(a2[wh] < sumLandscapeRule))
      expect_true(all(a2[-wh] >= sumLandscapeRule))
      expect_true(all(a3[-wh] >= meanHabitatRule))
      expect_true(all(a3[wh] < meanHabitatRule))
      if (interactive()) {
        terra::plot(ras)
      }
    }
  }

  withr::deferred_run()
})

test_that("asymmetry doesn't work properly", {
  testInit(c("terra", "withr"))
  rastDF <- needTerraAndRaster()

  aOrig <- terra::rast(terra::ext(0, 100, 0, 100), resolution = 1)

  for (ii in seq_len(NROW(rastDF))) {
    read <- eval(parse(text = rastDF$read[ii]))

    hab <- read(system.file("extdata", "hab.tif", package = "SpaDES.tools"))
    names(hab) <- "hab"
    hab2 <- hab > 0
    maxRadius <- 25
    maxVal <- 50
    withr::local_seed(53432)

    stopRule2 <- function(landscape) sum(landscape) > maxVal
    startCells <- as.integer(sample(1:ncell(hab), 1))

    n <- 16
    avgAngles <- numeric(n)
    lenAngles <- numeric(n)

    ## function to calculate mean angle -- returns in degrees
    meanAngle <- function(angles) {
      deg2(atan2(mean(sin(rad2(angles))), mean(cos(rad2(angles)))))
    }

    # if (interactive()) clearPlot()
    seed <- sample(1e6, 1)
    withr::local_seed(seed)

    for (asymAng in (2:n)) {
      circs <- spread(
        hab,
        spreadProb = 0.25,
        loci = ncell(hab) / 2 - ncol(hab) / 2,
        id = TRUE,
        returnIndices = TRUE,
        asymmetry = 40,
        asymmetryAngle = asymAng * 20
      )
      ci <- read(hab)
      ci[] <- 0
      ci[circs$indices] <- circs$id
      ciCentre <- read(ci)
      ciCentre[] <- 0
      ciCentre[unique(circs$initialLocus)] <- 1
      newName <- paste0("ci", asymAng * 20)
      assign(newName, ci)

      where2 <- function(name, env = parent.frame()) {
        ## simplified from pryr::where
        if (exists(name, env, inherits = FALSE)) {
          env
        } else {
          where2(name, parent.env(env))
        }
      }
      env <- where2(newName)
      if (interactive()) {
        obj <- get(newName, envir = env)
        terra::plot(obj)
        ciCentre[ciCentre == 0] <- NA
        terra::plot(ciCentre, add = TRUE, col = "black", legend = FALSE)
      }
      a <- cbind(id = circs$id, to = circs$indices, xyFromCell(hab, circs$indices))
      initialLociXY <- cbind(id = unique(circs$id), xyFromCell(hab, unique(circs$initialLocus)))
      dirs <- directionFromEachPoint(from = initialLociXY, to = a)
      dirs[, "angles"] <- deg2(dirs[, "angles"])
      avgAngles[asymAng] <- tapply(dirs[, "angles"], dirs[, "id"], meanAngle) %% 360
      lenAngles[asymAng] <- tapply(dirs[, "angles"], dirs[, "id"], length)
    }

    whBig <- which(lenAngles > 50)
    pred <- (1:n)[whBig] * 20
    expect_true(abs(coef(lm(avgAngles[whBig] ~ pred))[[2]] - 1) < 0.1)
  }

  withr::deferred_run()
})

test_that("rings and cir", {
  ## TODO: also need tests with polygons/vect
  testInit(c("terra", "withr"))
  rastDF <- needTerraAndRaster()

  for (ii in seq_len(NROW(rastDF))) {
    type <- rastDF$pkg[ii]
    cls <- rastDF$class[ii]
    pkg <- rastDF$read[ii]
    read <- eval(parse(text = rastDF$read[ii]))

    hab <- rast(system.file("extdata", "hab.tif", package = "SpaDES.tools")) #


    utm <- "+proj=utm +zone=28 +datum=WGS84 +units=m +no_defs"
    terra::crs(hab) <- utm
    # terra::crs(hab) <- "epsg:23028"

    names(hab) <- "hab"
    hab2 <- hab > 0
    n <- 2
    caribou <- terra::vect(cbind(
      x = stats::runif(n, xmin(hab), xmax(hab)),
      y = stats::runif(n, xmin(hab), xmax(hab))
    ))
    terra::crs(caribou) <- utm
    # terra::crs(caribou) <- "epsg:23028"

    radius <- 15
    cirsEx <- cir(
      hab,
      caribou,
      maxRadius = radius * 1.5,
      minRadius = radius,
      simplify = TRUE,
      includeBehavior = "excludePixels"
    )
    cirsIncl <- cir(
      hab,
      caribou,
      maxRadius = radius * 1.5,
      minRadius = radius,
      simplify = TRUE,
      includeBehavior = "includePixels"
    )

    expect_true(NROW(cirsEx) < NROW(cirsIncl))

    ## With including pixels, then distances are not strictly within the bounds of minRadius
    ##   and maxRadius, because every cell is included if it has a point anywhere within
    ##   the cell, causing cells whose centres are beyond maxRadius or shorter than minRadius
    ##   to be accepted
    b <- cbind(terra::crds(caribou), id = seq_along(caribou))
    a <- as.matrix(cirsIncl)
    colnames(a)[match(c("id", "indices"), colnames(a))] <- c("id", "to")
    dists <- distanceFromEachPoint(b, a)
    expect_true(radius * 1.49 < max(dists[, "dists"]))
    expect_true((radius * 1.01) > min(dists[, "dists"]))

    ## With excluding pixels, then distances are strictly within the bounds
    b <- cbind(terra::crds(caribou), id = seq_along(caribou))
    a <- as.matrix(cirsEx)
    colnames(a)[match(c("id", "indices"), colnames(a))] <- c("id", "to")
    dists <- distanceFromEachPoint(b, a)
    expect_true((radius * 1.5) %>=% max(dists[, "dists"]))
    expect_true(radius %<=% min(dists[, "dists"]))

    ras1 <- read(hab)
    ras1[] <- 0
    cirsOverlap <- data.table(cirsEx)[, list(sumIDs = sum(id)), by = indices]
    ras1[cirsOverlap$indices] <- cirsOverlap$sumIDs
    if (interactive()) {
      terra::plot(ras1)
    }

    ras3 <- read(hab)
    ras3[] <- 0
    cirsOverlap <- data.table(cirsIncl)[, list(sumIDs = sum(id)), by = indices]
    ras3[cirsOverlap$indices] <- cirsOverlap$sumIDs
    ras3 <- ras1 * 10 + ras3
    if (interactive()) {
      terra::plot(ras3)
    }
    expect_true(all(as.numeric(terra::values(ras3)) != 10)) # None should have only ras1, i.e., only circEx cells
    expect_true(all(as.numeric(terra::values(ras3)) != 20)) # None should have only ras1, i.e., only circEx cells

    cirsExSkinny <- data.table(cir(
      hab,
      caribou,
      maxRadius = radius,
      simplify = TRUE,
      includeBehavior = "excludePixels"
    ))
    expect_true(NROW(cirsExSkinny) == 0)

    ## Compare rings and cir -- if start in centre of cells, then should be identical
    n <- 2
    caribou <- terra::vect(cbind(
      x = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5,
      y = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5
    ))

    terra::crs(caribou) <- utm
    # terra::crs(caribou) <- "epsg:23028"

    loci <- terra::cellFromXY(hab, matrix(terra::crds(caribou)[1, ], ncol = 2))
    cirs <- data.table(cir(
      hab,
      caribou[1, ],
      maxRadius = radius * 1.5001,
      minRadius = radius,
      simplify = TRUE,
      allowOverlap = TRUE,
      includeBehavior = "excludePixels",
      returnDistances = TRUE
    ))
    cirs2 <- rings(
      hab,
      loci,
      minRadius = radius,
      maxRadius = radius * 1.5001,
      allowOverlap = TRUE,
      returnIndices = TRUE,
      includeBehavior = "includeRing"
    )
    setkey(cirs, dists, indices)
    ras1 <- read(hab)
    ras1[] <- 0
    cirsOverlap <- cirs[, list(sumIDs = sum(id)), by = indices]
    ras1[cirsOverlap$indices] <- cirsOverlap$sumIDs
    if (interactive()) {
      terra::plot(ras1, new = TRUE)
    }

    ras2 <- read(hab)
    ras2[] <- 0

    n <- 2
    withr::local_seed(1234) # sometimes seems to be different; set seed to avoid stochastic differences

    caribou <- terra::vect(cbind(
      x = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5,
      y = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5
    ))

    terra::crs(caribou) <- utm
    # terra::crs(caribou) <- "epsg:23028"

    loci <- cellFromXY(hab, terra::crds(caribou))

    dists1 <- rings(
      hab,
      loci,
      minRadius = 0,
      maxRadius = ncol(hab),
      returnDistances = TRUE,
      includeBehavior = "includeRing"
    )
    # dists2 <- distanceFromPoints(hab, terra::crds(caribou))
    dists2 <- distance(hab, caribou)
    dists3 <- cir(
      landscape = hab,
      loci = loci,
      minRadius = 0,
      maxRadius = ncol(hab),
      includeBehavior = "includePixels",
      allowOverlap = FALSE,
      returnIndices = FALSE,
      closest = TRUE,
      returnDistances = TRUE
    )
    if (interactive()) {
      terra::plot(dists1)
      terra::plot(dists2)
      terra::plot(dists3)
    }
    diffDists12 <- abs(dists1 - dists2)
    tabs12 <- table(round(as.numeric(terra::values(diffDists12))))
    expect_true(tabs12[names(tabs12) == 0] / ncell(diffDists12) > 0.99)
    if (interactive()) {
      terra::plot(diffDists12)
    }
    diffDists23 <- abs(dists2 - dists3)
    tabs23 <- table(round(as.numeric(terra::values(diffDists23))))

    ## This tests that the two approaches are 99% similar
    expect_true(tabs23[names(tabs23) == 0] / ncell(diffDists23) > 0.99)

    if (interactive()) {
      terra::plot(diffDists23)
    }
  }

  withr::deferred_run()
})

test_that("distanceFromPoints does not work correctly", {
  ## TODO: also need tests with polygons/vect
  testInit(c("terra", "withr"))

  hab <- terra::rast(system.file("extdata", "hab.tif", package = "SpaDES.tools"))
  names(hab) <- "hab"
  utm <- "+proj=utm +zone=28 +datum=WGS84 +units=m +no_defs"
  terra::crs(hab) <- utm
  # terra::crs(hab) <- "epsg:23028"

  n <- 1
  coords <- cbind(
    x = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5,
    y = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5
  )
  coordsVect <- terra::vect(coords[1, , drop = FALSE])
  terra::crs(coordsVect) <- terra::crs(hab)
  distsDFP1Pt <- terra::distance(hab, coordsVect)
  # distsDFP1Pt <- distanceFromPoints(hab, coords[1, , drop = FALSE])
  distsDFEP1Pt <- distanceFromEachPoint(coords[1, , drop = FALSE], landscape = hab)
  ras1 <- terra::rast(hab)
  ras1[] <- distsDFEP1Pt[, "dists"]
  expect_identical(0, unique(round(as.numeric(terra::values(distsDFP1Pt - ras1)), 7)))
  if (interactive()) {
    terra::plot(distsDFP1Pt)
    terra::plot(ras1)
  }

  maxDistance <- 30
  distsDFEPMaxD <- dists6 <- distanceFromEachPoint(
    coords,
    landscape = hab,
    maxDistance = maxDistance
  )

  ## test that maxDistance arg is working
  expect_true(round(max(distsDFEPMaxD[, "dists"]), 7) == maxDistance)

  ## evaluate cumulativeFn
  n <- 5
  hab <- terra::rast(terra::ext(0, 10, 0, 10), resolution = 1)
  coords <- cbind(
    x = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5,
    y = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5
  )
  dfep20 <- distanceFromEachPoint(coords[, xycolNames, drop = FALSE], landscape = hab)
  idw <- tapply(dfep20[, c("dists")], cellFromXY(hab, dfep20[, xycolNames]), function(x) {
    sum(1 / (1 + x))
  })
  dfep <- distanceFromEachPoint(
    coords[, xycolNames, drop = FALSE],
    landscape = hab,
    cumulativeFn = `+`
  )
  expect_true(sum(idw - dfep[, "dists"]) %==% 0)

  withr::deferred_run()
})

test_that("simple cir does not work correctly", {
  testInit(c("terra", "withr"))
  withr::local_seed(1234)
  rastDF <- needTerraAndRaster()

  hab <- terra::rast(terra::ext(0, 1e1, 0, 1e1), resolution = 1)

  for (ii in seq_len(NROW(rastDF))) {
    type <- rastDF$pkg[ii]
    if (type == "raster") {
      hab <- raster::raster(hab)
    }
    circleRas <- cir(hab, maxRadius = 1, includeBehavior = "excludePixels")
    expect_true(NROW(circleRas) == 4)
    expect_true(all(circleRas[, "indices"] == c(35, 44, 55, 46)))
    expect_true(all(mean(circleRas[, "x"]) == (ncol(hab) / 2 - 0.5)))
    expect_true(all(mean(circleRas[, "y"]) == (nrow(hab) / 2 + 0.5)))

    n <- 1
    coords <- cbind(
      x1 = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5,
      y1 = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5
    )
    expect_error(cir(hab, coords = coords), "coords must have columns named x and y")

    ## test id column in coords
    n <- 2
    coords <- cbind(
      x = (stats::runif(n, xmin(hab) + 0.5, xmax(hab) - 0.5)),
      y = (stats::runif(n, xmin(hab) + 0.5, xmax(hab) - 0.5)),
      id = c(45, 56)
    )
    cirs <- cir(
      hab,
      coords = coords,
      maxRadius = 1,
      minRadius = 0,
      includeBehavior = "includePixels",
      returnIndices = TRUE
    )
    expect_true(all(unique(cirs[, "id"]) == c(45, 56)))
    expect_true(all(distanceFromEachPoint(coords, cirs)[, "dists"] %<=% 1))

    ## test closest
    n <- 1
    coords <- cbind(x = c(5, 6), y = c(5, 5))
    cirsClosestT <- cir(
      hab,
      coords = coords,
      maxRadius = 2,
      minRadius = 0,
      includeBehavior = "includePixels",
      closest = TRUE,
      returnIndices = TRUE,
      allowOverlap = FALSE
    )
    cirsClosestF <- cir(
      hab,
      coords = coords,
      maxRadius = 2,
      minRadius = 0,
      includeBehavior = "includePixels",
      closest = FALSE,
      returnIndices = TRUE,
      allowOverlap = FALSE
    )
    expect_true(all(table(cirsClosestF[, "id"]) == c(17, 4)))
    expect_true(all(table(cirsClosestT[, "id"]) - table(cirsClosestF[, "id"]) == c(-5, 5)))

    cirs2 <- cir(
      hab,
      coords = coords,
      maxRadius = 2,
      minRadius = 0,
      includeBehavior = "includePixels",
      closest = FALSE,
      returnIndices = FALSE,
      allowOverlap = FALSE,
      returnDistances = FALSE
    )
    expect_s4_class(cirs2, rastDF$class[ii])
    expect_true(max(as.numeric(terra::values(cirs2))) == 2)
    expect_true(min(as.numeric(terra::values(cirs2))) == 0)

    cirs2 <- cir(
      hab,
      coords = coords,
      maxRadius = 2,
      minRadius = 0,
      includeBehavior = "includePixels",
      closest = FALSE,
      returnIndices = FALSE,
      allowOverlap = TRUE,
      returnDistances = FALSE
    )
    expect_s4_class(cirs2, rastDF$class[ii])
    expect_true(min(as.numeric(terra::values(cirs2))) == 0)

    cirs2 <- cir(
      hab,
      coords = coords,
      maxRadius = 2,
      minRadius = 0,
      includeBehavior = "includePixels",
      closest = FALSE,
      returnIndices = FALSE,
      allowOverlap = TRUE,
      returnDistances = TRUE
    )
    expect_s4_class(cirs2, rastDF$class[ii])
    expect_true(min(as.numeric(terra::values(cirs2))) == 0)

    hab <- terra::rast(terra::ext(0, 1e1, 0, 1e1), resolution = c(1, 2))
    expect_error(
      cir(hab, maxRadius = 1, includeBehavior = "excludePixels"),
      "cir function only accepts rasters with identical resolution in x and y dimensions"
    )

    hab <- terra::rast(terra::ext(0, 1e1, 0, 1e1), resolution = 1)
    expect_error(
      cir(hab, maxRadius = 1, includeBehavior = "excludeRings"),
      "includeBehavior can only be \"includePixels\" or \"excludePixels\""
    )
  }

  withr::deferred_run()
})

test_that("wrap does not work correctly", {
  testInit(c("terra", "withr"))

  xrange <- yrange <- c(-50, 50)
  hab <- rast(ext(c(xrange, yrange)))
  hab[] <- 0

  # initialize caribou agents
  n <- 10

  ## previous points
  x1 <- rep(0, n)
  y1 <- rep(0, n)
  ## initial points, outside of range
  starts <- cbind(
    x = stats::runif(n, xrange[1] - 10, xrange[1]),
    y = stats::runif(n, yrange[1] - 10, yrange[1])
  )

  expect_false(all(SpaDES.tools::wrap(starts, bounds = ext(hab)) == starts))
  expect_false(all(SpaDES.tools::wrap(starts, bounds = hab) == starts))
  expect_error(
    SpaDES.tools::wrap(starts, bounds = starts),
    "Unable to determine extent of object of type 'matrix'."
  )

  ## using sf
  if (requireNamespace("sf", quietly = TRUE)) {
    sf <- sf::st_as_sf(data.frame(starts, x1, y1), coords = xycolNames)
    expect_true(all(
      coords(SpaDES.tools::wrap(sf, bounds = hab)) == SpaDES.tools::wrap(starts, hab)
    ))
    expect_true(all(
      coords(SpaDES.tools::wrap(sf, bounds = hab, withHeading = FALSE)) ==
        SpaDES.tools::wrap(starts, hab)
    ))
    expect_true(all(
      coords(SpaDES.tools::wrap(sf, bounds = terra::ext(hab), withHeading = FALSE)) ==
        SpaDES.tools::wrap(starts, hab)
    ))
    expect_error(
      SpaDES.tools::wrap(sf, bounds = starts, withHeading = FALSE),
      "Unable to determine extent of object of type 'matrix'."
    )
  }
  ## errors
  starts <- cbind(
    x = stats::runif(n, xrange[1] - 10, xrange[1]),
    y = stats::runif(n, yrange[1] - 10, yrange[1])
  )
  spdf <- terra::vect(data.frame(starts, x1, y1), geom = xycolNames) #
  expect_false(all(abs(terra::ext(spdf)[]) <= 50))
  out <- SpaDES.tools::wrap(spdf, bounds = terra::ext(hab))
  expect_true(all(abs(terra::ext(out)[]) <= 50))

  withr::deferred_run()
})

test_that("cir angles arg doesn't work", {
  testInit(c("terra", "fpCompare", "withr"))

  ras <- terra::rast(terra::ext(0, 100, 0, 100), resolution = 1)
  ras[] <- 0
  n <- 2
  withr::local_seed(321)
  coords <- cbind(
    x = stats::runif(n, terra::xmin(ras), terra::xmax(ras)),
    y = stats::runif(n, terra::xmin(ras), terra::xmax(ras))
  )
  angles <- seq(0, 2 * pi, length.out = 21)[-21]
  circ <- cir(
    ras,
    coords,
    angles = angles,
    maxRadius = 3,
    minRadius = 0,
    returnIndices = TRUE,
    allowOverlap = TRUE,
    returnAngles = TRUE
  )
  anglesTab <- table(circ[, "angles"])
  expect_true(all(as.numeric(names(anglesTab)) %==% angles))
  expect_true(all(length(anglesTab) == (length(angles))))

  withr::deferred_run()
})

test_that("multi-core version of distanceFromEachPoints does not work correctly", {
  ## Only raster -- beginCluster is a mechanism only in raster AFAIK (Eliot)
  skip_on_cran()
  skip_on_ci()

  if (interactive()) {
    testInit(c("raster", "parallel", "DEoptim", "withr"))

    hab <- randomPolygons(terra::rast(terra::ext(0, 1e2, 0, 1e2)), resolution = 1)

    ## evaluate cumulativeFn
    n <- 50
    coords <- cbind(
      x = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5,
      y = round(stats::runif(n, xmin(hab), xmax(hab))) + 0.5
    )
    dfep <- distanceFromEachPoint(
      coords[, xycolNames, drop = FALSE],
      landscape = hab,
      cumulativeFn = `+`
    )

    ## using parallel package cluster
    system.time({
      cl1 <- makeCluster(1, rscript_args = "--vanilla --no-environ")
      clusterEvalQ(cl1, {
        library(SpaDES.tools)
      })
    })
    system.time({
      dfepCluster <- distanceFromEachPoint(
        coords[, xycolNames, drop = FALSE],
        landscape = hab,
        cumulativeFn = `+`,
        cl = cl1
      )
    })
    stopCluster(cl1)
    expect_true(all.equal(dfep, dfepCluster))

    ## using raster package cluster
    system.time({
      raster::beginCluster(1, type = "PSOCK")
    })
    system.time({
      dfepCluster2 <- distanceFromEachPoint(
        coords[, xycolNames, drop = FALSE],
        landscape = hab,
        cumulativeFn = `+`
      )
    })
    raster::endCluster()
    expect_true(all.equal(dfep, dfepCluster2))
  }

  withr::deferred_run()
})

test_that("spreadProb with relative values does not work correctly", {
  testInit(c("terra", "withr"))
  rastDF <- needTerraAndRaster()

  ext1 <- terra::ext(0, 1e2, 0, 1e2)
  extRas <- terra::rast(ext1, resolution = 1)
  for (ii in seq_len(NROW(rastDF))) {
    type <- rastDF$pkg[ii]
    cls <- rastDF$class[ii]
    pkg <- rastDF$read[ii]
    read <- eval(parse(text = rastDF$read[ii]))

    seed <- 64350
    withr::local_seed(seed)
    emptyRas <- read(extRas)
    hab <- randomPolygons(emptyRas, numTypes = 40)
    names(hab) <- "hab"

    hab3 <- (hab > 20) * 200 + 1
    sam <- sample(which(hab3[] == 1), 1)
    withr::local_seed(seed)

    events1 <- spread(
      hab3,
      spreadProb = hab3,
      loci = sam,
      directions = 8,
      neighProbs = c(0, 1),
      maxSize = c(100),
      exactSizes = TRUE
    )

    # Compare to absolute probability version
    withr::local_seed(seed)

    events2 <- spread(
      hab3,
      id = TRUE,
      loci = sam,
      directions = 8,
      neighProbs = c(0, 1),
      maxSize = c(100),
      exactSizes = TRUE
    )

    #if (as.numeric_version(paste0(R.version$major, ".", R.version$minor)) < "3.6.4") {
    ## many more high value hab pixels spread to in event1
    #  expect_true(sum(hab3[events1[] > 0]) > sum(hab3[events2[] > 0]))
    #} else {
    ## equal number on R-devel
    expect_equal(sum(hab3[events1[] > 0]), sum(hab3[events2[] > 0]))
    #}

    # Check numeric vector with NAs is equivalent to raster with NAs
    numNAs <- 50
    sps <- sample(c(rep(NA_real_, numNAs), runif(ncell(hab3) - numNAs, 0, 0.5)))
    ras <- read(hab3)
    ras[] <- sps
    withr::local_seed(seed)

    out1 <- spread(hab3, loci = ncell(hab3) / 2, spreadProb = ras)
    expect_s4_class(out1, rastDF$class[ii])
    withr::local_seed(seed)

    out2 <- spread(hab3, loci = ncell(hab3) / 2, spreadProb = sps)
    expect_s4_class(out2, rastDF$class[ii])
    if (is(out1, "Raster")) {
      raster::compareRaster(
        out1,
        out2,
        extent = TRUE,
        rowcol = TRUE,
        crs = TRUE,
        res = TRUE,
        orig = TRUE,
        rotation = TRUE,
        values = TRUE,
        stopiffalse = FALSE
      )
    } else {
      terra::compareGeom(
        out1,
        out2,
        crs = TRUE,
        ext = TRUE,
        rowcol = TRUE,
        res = TRUE,
        stopOnError = FALSE
      )
    }
  }
})
