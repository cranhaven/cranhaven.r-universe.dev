library(testthat)
library(hydflood)

context("flood1")

test_that("flood1: checks", {
    if (Sys.info()["nodename"] == "pvil-r") {
        # Elbe
        hf <- Sys.getenv("hydflood")
        x <- hydSpatRaster(
            filename_dem = paste0(hf, "/data-raw/raster.dem.tif"),
            filename_csa = paste0(hf, "/data-raw/raster.csa.tif"))
        
        # create a temporal sequence
        seq <- seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by = "day")
        
        # errors
        # x missing
        expect_error(flood1(seq = seq), "The 'x' argument has to be supplied.")
        
        # x class
        expect_error(flood1(x = seq), "'x' must be type 'SpatRaster'")
        
        # seq missing
        expect_error(flood1(x), "The 'seq' argument has to be supplied.")
        
        # seq class
        expect_error(flood1(x, c("a", "b")),
                     "'seq' must be either type 'Date' or c('POSIXct', 'POSIXt",
                     fixed = TRUE)
        
        # seq length
        expect_error(flood1(x, numeric()),
                     "'seq' must have length larger 0.")
        
        # seq NA
        expect_error(flood1(x, as.Date(NA)),
                     "'seq' or elements of it must not be NA.")
        
        # seq range Date
        seq <- seq(as.Date("1958-12-21"), as.Date("1958-12-22"), by = "days")
        expect_error(flood1(x, seq),
                     "'seq' must be between 1960-01-01 and yesterday")
        seq <- seq(as.Date("2050-12-21"), as.Date("2050-12-22"), by = "days")
        expect_error(flood1(x, seq),
                     "'seq' must be between 1960-01-01 and yesterday")
        
        # seq range POSIX
        now <- Sys.time()
        months_ago <- now - 3600 * 24 * 30 * 2
        seq <- c(months_ago - 3600, months_ago - 3600 * 2)
        expect_error(flood1(x, seq),
                     "00:00:00 and now, if type of 'seq' is c('POSIXct', 'POSI",
                     fixed = TRUE)
        seq <- seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by = "day")
        
        # gauging station and uuid missing
        expect_error(flood1(x, seq), "The 'gauging_station' or 'uuid' argument")
        
        # gauging station class
        expect_error(flood1(x, seq, 1), "'gauging_station' must be type 'chara")
        
        # gauging station length
        expect_error(flood1(x, seq, c("a", "b")),
                     "'gauging_station' must have length 1.")
        
        # gauging station value
        expect_error(flood1(x, seq, "DES"),
                     " must be an element of c('SCHOENA'", fixed = TRUE)
        expect_error(flood1(x, seq, "DRESDEN"),
                     "has to be in the river stretch", fixed = TRUE)
        
        # uuid class
        expect_error(flood1(x, seq, uuid = 1), "'uuid' must be type 'character")
        
        # uuid length
        expect_error(flood1(x, seq, uuid = c("a", "b")),
                     "'uuid' must have length 1.")
        
        # uuid value
        expect_error(flood1(x, seq, uuid = "DES"),
                     "element of c('7cb7461b-3530-", fixed = TRUE)
        expect_error(flood1(x, seq,
                            uuid = "b8567c1e-8610-4c2b-a240-65e8a74919fa"),
                     "has to be in the river stretch", fixed = TRUE)
        
        # filename length
        expect_error(flood1(x, seq, "DESSAU", filename = 1),
                     "'filename' must be type 'character'.")
        expect_error(flood1(x, seq, "DRESDEN", filename = c("a", "b")),
                     "'filename' must have length 1.")
    }
})

test_that("flood1: Elbe", {
    if (Sys.info()["nodename"] == "pvil-r") {
        # Elbe
        hf <- Sys.getenv("hydflood")
        x <- hydSpatRaster(
            filename_dem = paste0(hf, "/data-raw/raster.dem.tif"),
            filename_csa = paste0(hf, "/data-raw/raster.csa.tif"))
        
        # create a temporal sequence
        seq <- seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by = "day")
        
        # compute a flood duration
        fd <- flood1(x = x, seq = seq, gauging_station = "DESSAU")
        
        expect_equal(class(fd)[1], "SpatRaster")
        expect_equal(minmax(fd)["max",], length(seq))
        expect_equal(minmax(fd)["min",], 0)
    }
})


test_that("flood1: Rhine", {
    if (Sys.info()["nodename"] == "pvil-r") {
        # Rhine
        hf <- Sys.getenv("hydflood")
        x <- hydSpatRaster(
            filename_dem = paste0(hf, "/data-raw/raster.dem_plittersdorf.tif"),
            filename_csa = paste0(hf, "/data-raw/raster.csa_plittersdorf.tif"))
        
        # create a temporal sequence
        seq <- seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by = "day")
        
        # compute a flood duration
        fd <- flood1(x = x, seq = seq, gauging_station = "PLITTERSDORF")
        
        expect_equal(class(fd)[1], "SpatRaster")
        expect_equal(minmax(fd)["max",], length(seq))
        expect_equal(minmax(fd)["min",], 0)
    }
})

