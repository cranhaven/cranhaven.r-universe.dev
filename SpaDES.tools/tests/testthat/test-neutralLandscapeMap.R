test_that("neutralLandscapeMap produces consistent rasters", {
  testInit(c("NLMR"))
  skip_if_not_installed("NLMR", "1.1.1")
  rastDF <- needTerraAndRaster()

  # inputs for x
  aOrig <- terra::rast(system.file("extdata", "a.tif", package = "SpaDES.tools"))
  bOrig <- terra::rast(aOrig)

  bSimple <- terra::rast(terra::ext(0, 10, 0, 10), res = 1)

  spRas <- terra::rast(system.file("extdata", "spRas.tif", package = "SpaDES.tools"))


  for (ii in seq_len(NROW(rastDF))) {
    pkg <- rastDF$pkg[ii]
    cls <- rastDF$class[ii]
    read <- eval(parse(text = rastDF$read[ii]))


    n <- 20
    nx <- sample.int(300, n)
    ny <- sample.int(300, n)
    rlist <- list()
    crslist <- list(
      latlon = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
      lcc = paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0",
                  "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"),
      aea = paste("+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0",
                  "+datum=NAD83 +units=m +no_defs +type=crs")
    )

    # for (i in seq_len(NROW(df))) {
    #   pkg <- df$pkg[i]
    #   read <- df$read[i]

    withr::local_package(pkg)

    for (i in seq_along(n)) {
      rastArgs <- list(xmn = -nx[i] / 2, xmx = nx[i] / 2,
                       ymn = -ny[i] / 2, ymx = ny[i] / 2)
      if (pkg == "terra") {
        names(rastArgs) <-c("xmin", "xmax", "ymin", "ymax")
      }
      rastArgs <- append(rastArgs, list(nrows = ny[i], ncols = nx[i]))

      r <- do.call(read, rastArgs)

      ## give some a projection
      if (i %in% seq_along(length(crslist))) {
        crs(r) <- crslist[[i]]
      }

      rlist[[i]] <- suppressWarnings({
        neutralLandscapeMap(r, roughness = 0.65, rand_dev = 100, rescale = FALSE, verbose = FALSE)
      })

      expect_equal(ncol(rlist[[i]]), nx[i])   ## not identical (ncol/nrow can output numeric with terra)
      expect_equal(nrow(rlist[[i]]), ny[i])
      expect_false(any(is.na(rlist[[i]][])))

      compareFun <- function(x, y, pkg) {
        if (pkg == "terra") {
          terra::compareGeom(x, y, res = TRUE)
        } else {
          compareRaster(x, y, res = TRUE, orig = TRUE)
        }
      }

      expect_true(compareFun(r, rlist[[i]], pkg = pkg))
    }
  }
})
