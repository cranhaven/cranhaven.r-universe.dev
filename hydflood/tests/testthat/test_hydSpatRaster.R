library(testthat)
library(hydflood)

context("hydSpatRaster")

utm32n <- st_crs(25832)
utm33n <- st_crs(25833)
wgs <- st_crs(4326)

test_that("General tests", {
    
    # smaller extent (crop)
    if (Sys.info()["nodename"] == "pvil-r") {
        # input data
        filename_dem <- paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/",
                               "RH_336_867_UFD/data/ascii/r002_PLITTERSDORF1_D",
                               "EM.asc")
        filename_csa <- paste0("/home/WeberA/freigaben/U/U3/Auengruppe_INFORM/",
                               "RH_336_867_UFD/data/ascii/r002_PLITTERSDORF1_C",
                               "SA.asc")
        ext <- ext(436500, 438000, 5415000, 5416500)
        
        # tests
        expect_message(a <- hydSpatRaster(filename_dem, filename_csa, ext,
                                          crs = utm32n),
                       "'ext' will be used to crop the supplied raster file(s).",
                       fixed = TRUE)
        # expect_message(b <- hydSpatRaster(filename_csa = filename_csa, 
        #                                    ext = ext, crs = utm32n),
        #                "'ext' will be used to crop the supplied raster file",
        #                fixed = TRUE)
        expect_equal(ext(hydSpatRaster(ext = ext, crs = utm32n)), ext)
        expect_equal(crs(hydSpatRaster(ext = ext, crs = utm32n)), utm32n$wkt)
        
        # the same extents and crs, but different data sources
        hf3 <- Sys.getenv("hydflood")
        filename_dem <- paste0(hf3, "/data-raw/raster.dem.tif")
        filename_csa <- paste0(hf3, "/data-raw/raster.csa.tif")
        ext_csa <- ext(rast(filename_csa))
        crs_csa <- sf::st_crs(crs(rast(filename_csa)))
        expect_equal(dim(hydSpatRaster(filename_dem = filename_dem, 
                                       filename_csa = filename_csa)),
                     c(1000, 1000, 2))
        expect_equal(res(hydSpatRaster(filename_dem = filename_dem, 
                                       filename_csa = filename_csa)),
                     c(1,1))
        
        expect_equal(dim(hydSpatRaster(filename_csa = filename_csa)),
                     c(1000, 1000, 2))
        expect_equal(minmax(hydSpatRaster(filename_dem = filename_dem,
                                          filename_csa = filename_csa)),
                     minmax(hydSpatRaster(filename_dem = filename_dem)))
        expect_equal(minmax(hydSpatRaster(filename_dem = filename_dem,
                                          filename_csa = filename_csa)),
                     minmax(hydSpatRaster(filename_csa = filename_csa)))
        expect_equal(minmax(hydSpatRaster(filename_dem = filename_dem,
                                          filename_csa = filename_csa)),
                     minmax(hydSpatRaster(ext = ext_csa,
                                          crs = crs_csa)))
        
        # store files
        tmp_dem1 <- tempfile(fileext = ".tif")
        tmp_csa1 <- tempfile(fileext = ".tif")
        a <- hydSpatRaster(filename_dem = tmp_dem1, filename_csa = tmp_csa1,
                           ext = ext_csa, crs = crs_csa)
        expect_equal(file.exists(tmp_dem1), TRUE)
        expect_equal(file.exists(tmp_csa1), TRUE)
        b <- hydSpatRaster(filename_dem = tmp_dem1, filename_csa = tmp_csa1,
                           ext = ext_csa, crs = crs_csa)
        # expect_equal(a, b)
        expect_message(c <- hydSpatRaster(tmp_dem1, tmp_csa1,
                                          ext(309200, 310000,
                                              5749000, 5749500)),
                       "'ext' will be used to crop the supplied raster file(s)",
                       fixed = TRUE)
        tmp_dem2 <- tempfile(fileext = ".tif")
        tmp_dem3 <- tempfile(fileext = ".tif")
        tmp_csa2 <- tempfile(fileext = ".tif")
        writeRaster(c$dem, tmp_dem2)
        writeRaster(terra::aggregate(rast(tmp_csa1), fact = 2), tmp_csa2,
                    overwrite = TRUE)
        d <- c$dem
        crs(d) <- "EPSG:25832"
        writeRaster(d, tmp_dem3)
        
        expect_error(hydSpatRaster(filename_dem = tmp_dem2,
                                   filename_csa = tmp_csa1),
                     "[compareGeom] extents do not match", fixed = TRUE)
        expect_error(hydSpatRaster(filename_dem = tmp_dem2,
                                   filename_csa = tmp_csa2),
                     "[compareGeom] extents do not match", fixed = TRUE)
        expect_error(hydSpatRaster(filename_dem = tmp_dem2,
                                   filename_csa = tmp_csa2),
                     "[compareGeom] extents do not match", fixed = TRUE)
        expect_error(hydSpatRaster(filename_dem = tmp_dem3,
                                   filename_csa = tmp_csa2),
                     "[compareGeom] extents do not match", fixed = TRUE)
        expect_error(hydSpatRaster(ext = ext(d)),
                     "The 'crs' argument has to be supplied.")
        expect_error(hydSpatRaster(ext = ext(d), crs = 1),
                     "'crs' must be type 'crs'")
        expect_error(hydSpatRaster(filename_dem = tmp_dem1,
                                    filename_csa = tmp_csa1,
                                    crs = wgs),
                     "supplied 'crs' does not agree with the crs supplied through")
        expect_error(hydSpatRaster(ext = ext(d), crs = wgs),
                     "crs must be either 'ETRS 1989 UTM 32N' or 'ETRS 1989 UTM 33N")
        expect_error(hydSpatRaster(ext = ext(200000, 201000, 5749000, 5749500),
                                   crs = crs_csa),
                     "does NOT overlap with the active floodplain of River Elbe")
        expect_error(hydSpatRaster(ext = ext(200000, 201000, 5749000, 5749500),
                                   crs = utm32n),
                     "does NOT overlap with the active floodplain of River Rhine")
        
        unlink(tmp_dem1)
        unlink(tmp_dem2)
        unlink(tmp_dem3)
        unlink(tmp_csa1)
        unlink(tmp_csa2)
    }
    
    # input data files
    expect_error(hydSpatRaster(c(1,2,3)), 
                 "'filename_dem' must be type 'character'")
    expect_error(hydSpatRaster(c(1,2,3)), 
                 "'filename_dem' must have length 1")
    expect_error(hydSpatRaster(filename_csa = c(1,2,3)),
                 "'filename_csa' must be type 'character'")
    expect_error(hydSpatRaster(filename_csa = c(1,2,3)),
                 "'filename_csa' must have length 1")
    
})

