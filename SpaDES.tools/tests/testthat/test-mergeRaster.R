test_that("mergeRaster will return a message if tiles are resampled", {
  rastDF <- needTerraAndRaster()
  nx <- ny <- 3

  for (ii in seq_len(NROW(rastDF))) {
    pkg <- rastDF$pkg[ii]
    cls <- rastDF$class[ii]
    read <- eval(parse(text = rastDF$read[ii]))

    withr::local_package(pkg)

    rastArgs <- list(xmn = -30^2, xmx = 30^2, ymn = -60^2, ymx = 60^2)
    if (pkg == "terra") {
      names(rastArgs) <- c("xmin", "xmax", "ymin", "ymax")
    }
    rastArgs <- append(rastArgs,
                       list(crs = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                            resolution = c(30, 30),
                            vals = round(runif(n = 14400, min = 1, max = 10))))

    ras <- do.call(read, rastArgs)

    splitted <- splitRaster(r = ras, nx = nx, ny = ny, buffer = c(3, 3))
    expect_type(splitted, "list")
    expect_length(splitted, nx * ny)

    splitted <- lapply(X = seq_along(splitted), FUN = function(tiles, pkg, ras) {
      rastArgs <- list(xmn = xmin(splitted[[tiles]]),
                       xmx = xmax(splitted[[tiles]]),
                       ymn = ymin(splitted[[tiles]]),
                       ymx = ymax(splitted[[tiles]]))
      if (pkg == "terra") {
        names(rastArgs) <- c("xmin", "xmax", "ymin", "ymax")
      }
      rastArgs <- append(rastArgs, list(resolution = c(250, 250), crs = crs(splitted[[tiles]])))
      y <- do.call(read, rastArgs)

      expect_false(any(res(y) == res(ras)))
      ## always use `terra`, it's compatible with RasterLayer  objects;
      ##   getFromNameSpace("resample", "raster") doesn't work.
      r <- terra::resample(x = ras, y = y)
      return(r)
    }, pkg = pkg, ras = ras)

    expect_type(splitted, "list")
    expect_message({
      merged <- mergeRaster(x = splitted)
    })
    expect_s4_class(merged, cls)
  }
})

test_that("mergeRaster will produce a raster layer", {
  rastDF <- needTerraAndRaster()
  nx <- ny <- 3

  for (ii in seq_len(NROW(rastDF))) {
    pkg <- rastDF$pkg[ii]
    cls <- rastDF$class[ii]
    read <- eval(parse(text = rastDF$read[ii]))

    withr::local_package(pkg)

    rastArgs <- list(xmn = -30^2, xmx = 30^2, ymn = -60^2, ymx = 60^2)
    if (pkg == "terra") {
      names(rastArgs) <- c("xmin", "xmax", "ymin", "ymax")
    }
    rastArgs <- append(rastArgs,
                       list(crs = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                            resolution = c(30, 30),
                            vals = round(runif(n = 14400, min = 1, max = 10))))

    ras <- do.call(read, rastArgs)

    splitted <- splitRaster(r = ras, nx = nx, ny = ny, buffer = c(5, 5))
    expect_type(splitted, "list")
    expect_length(splitted, nx * ny)
    merged <- mergeRaster(x = splitted)
    expect_s4_class(merged, cls)
  }
})

test_that("mergeRaster will produce error if only one raster passed", {
  rastDF <- needTerraAndRaster()

  for (ii in seq_len(NROW(rastDF))) {
    pkg <- rastDF$pkg[ii]
    read <- eval(parse(text = rastDF$read[ii]))

    withr::local_package(pkg)

    rastArgs <- list(xmn = -30^2, xmx = 30^2, ymn = -60^2, ymx = 60^2)
    if (pkg == "terra") {
      names(rastArgs) <- c("xmin", "xmax", "ymin", "ymax")
    }
    rastArgs <- append(rastArgs,
                       list(crs = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                            resolution = c(30, 30),
                            vals = round(runif(n = 14400, min = 1, max = 10))))

    ras <- do.call(read, rastArgs)
    expect_error({
      merged <- mergeRaster(x = ras)
    })
  }
})

test_that("mergeRaster uses mosaic with default mean if rasters are resampled and fun if passed", {
  rastDF <- needTerraAndRaster()
  nx <- ny <- 3

  for (ii in seq_len(NROW(rastDF))) {
    pkg <- rastDF$pkg[ii]
    cls <- rastDF$class[ii]
    read <- eval(parse(text = rastDF$read[ii]))

    withr::local_package(pkg)

    rastArgs <- list(xmn = -30^2, xmx = 30^2, ymn = -60^2, ymx = 60^2)
    if (pkg == "terra") {
      names(rastArgs) <- c("xmin", "xmax", "ymin", "ymax")
    }
    rastArgs <- append(rastArgs,
                       list(crs = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                            resolution = c(30, 30),
                            vals = round(runif(n = 14400, min = 1, max = 10))))

    ras <- do.call(read, rastArgs)

    splitted <- splitRaster(r = ras, nx = nx, ny = ny, buffer = c(10, 10))
    expect_type(splitted, "list")
    expect_length(splitted, nx * ny)
    splitted <- lapply(X = seq_along(splitted), FUN = function(tiles, pkg) {
      rastArgs <- list(xmn = xmin(splitted[[tiles]]),
                       xmx = xmax(splitted[[tiles]]),
                       ymn = ymin(splitted[[tiles]]),
                       ymx = ymax(splitted[[tiles]]))
      if (pkg == "terra") {
        names(rastArgs) <- c("xmin", "xmax", "ymin", "ymax")
      }
      rastArgs <- append(rastArgs, list(resolution = c(250, 250),
                                        crs = terra::crs(splitted[[tiles]])))
      y <- do.call(read, rastArgs)
      expect_false(any(res(y) == res(ras)))
      r <- terra::resample(x = ras, y = y) ## use terra always. compatible with RasterLayer
      return(r)
    }, pkg = pkg)
    expect_type(splitted, "list")
    expect_message({
      merged <- mergeRaster(x = splitted)
    })
    expect_message({
      merged2 <- mergeRaster(x = splitted, fun = max)
    })
    expect_s4_class(merged, cls)
    expect_s4_class(merged2, cls)
  }
})
