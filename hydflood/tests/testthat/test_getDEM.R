library(testthat)
library(hydflood)

context("getDEM")

test_that("General tests", {
    
    utm32n <- st_crs(25832)
    utm33n <- st_crs(25833)
    wgs <- st_crs(4326)
    
    # input data checks
    expect_error(getDEM(), 
                 paste0("Error 1: If you don't provide an existing 'filename',",
                        " you have to specify 'ext' and 'crs'."))
    expect_error(getDEM(c(1,2,3)), "'filename' must be type 'character'")
    expect_error(getDEM(c(1,2,3)), "'filename' must have length 1")
    expect_error(getDEM(ext = c(1,2,3)), "ovide a crs, you must specify 'crs'.")
    expect_error(getDEM(ext = c(1,2,3), crs = utm32n),
                 "'ext' must be type 'SpatExtent'")
    expect_error(getDEM(ext = ext(1,2,3,4), crs = utm32n),
                 "'ext' does NOT overlap with the active floodplain of River R")
    expect_error(getDEM(ext = ext(1,2,3,4), crs = utm33n),
                 "'ext' does NOT overlap with the active floodplain of River E")
    expect_error(getDEM(ext = ext(1,2,3,4), crs = "c"),
                 "'crs' must be type 'crs'.")
    expect_error(getDEM(ext = ext(1,2,3,4), crs = wgs),
                 "'crs' must be either 'ETRS 1989 UTM 32N' or")
    
    if (Sys.info()["nodename"] == "pvil-r") {
        # input data
        filename <- paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/RH_3",
                           "36_867_UFD/data/tiff/r002_PLITTERSDORF1_DEM.tif")
        ext <- ext(436500, 438000, 5415000, 5416500)
        
        # tests
        d <- getDEM(filename = filename)
        
        expect_message(getDEM(filename = filename, ext = ext),
                       "'ext' will be used to crop the supplied raster file.",
                       fixed = TRUE)
        expect_message(getDEM(filename = filename, ext = ext, crs = utm32n),
                       "'ext' will be used to crop the supplied raster file.",
                       fixed = TRUE)
        expect_error(getDEM(filename = filename, ext = ext, crs = utm33n),
                       "does not agree with the crs of the raster suppl",
                       fixed = TRUE)
        
        tmp_dem1 <- tempfile(fileext = ".tif")
        if (file.exists(tmp_dem1)) {unlink(tmp_dem1)}
        d <- getDEM(filename = tmp_dem1, ext = ext, crs = utm32n)
        
        expect_equal(file.exists(tmp_dem1), TRUE)
        expect_equal(ext(d), ext)
        expect_equal(st_crs(d) == st_crs(utm32n), TRUE)
        
        unlink(tmp_dem1)
        
        # the same extents and crs, but different data sources
        hf3 <- Sys.getenv("hydflood")
        filename <- paste0(hf3, "/data-raw/raster.dem.tif")
        d <- getDEM(filename = filename)
        
        expect_equal(dim(d), c(1000, 1000, 1))
        expect_equal(res(d), c(1, 1))
        
        expect_error(getDEM(filename, ext = ext(308000, 310000,
                                                5749000, 5750000)),
                     "'ext' must be totally within the raster")
        
        expect_message(getDEM(filename, ext = ext(309200, 310000,
                                                  5749000, 5750000)),
                       "'ext' will be used to crop the supplied raster file.")
        expect_error(getDEM(ext = ext(295000, 340000, 5744000, 5753000),
                            crs = utm33n),
                     "'ext' is very large and covers more than 5 ")
        expect_warning(getDEM(ext = ext(300000, 330000, 5744000, 5753000),
                              crs = utm33n),
                     "'ext' is large and covers more than 3 ")
    }
})

