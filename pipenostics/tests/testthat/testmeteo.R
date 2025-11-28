library(pipenostics)

test_that("*meteos* has wrong data", {
  expect_equal(
    meteos()[c(87,75, 40, 15), ],
    structure(
      list(
        station_id = c(34163L, 31915L, 29023L, 24908L)
       ,name = c("Oktyabrskiy-Gorodok", "Pogranichnyi", "Napas", "Vanavara")
       ,lat = c(51.63, 44.4, 59.85, 60.33)
       ,lon = c(45.45, 131.38, 81.95, 102.27)
       ,alt = c(201, 217, 77, 259)
       ,avg = c(8.06452230500802, 6.12987251658832, 4.7549349140692, 1.77414578587699)
      )
      ,row.names = c(34163L, 31915L, 29023L, 24908L)
      ,class = "data.frame"
    ),
    tolerance = 5e-6
  )
})


test_that("*mgtdhid* produces wrong results", {
  expect_equal(
    mgtdhid(id = c(22217L, 26094L, 30433L), tau = 2400L, depth = 2.4),
    c(0.929297595932886, 3.45956953933863, -0.192953283493775)
  )

  expect_equal(
    mgtdhid(id = c(22217L, 26094L, 30433L), tau = as.POSIXct("2023-04-11", tz = "UTC"), depth = 2.4),
    c(0.929297595932886, 3.45956953933863, -0.192953283493775)
  )
})


test_that("*mgtdhidt* produces wrong results", {
  hours <- c(10L, 100L, 1000L, 8000L)
  d24 <- 2.4

  expect_equal(
    mgtdhidt(tau = as.integer(seq.int(0, 8736, by = 1)), depth = d24),
    mgtdhidt(tau = as.POSIXct(seq.int(1672531200, 1703980800, 3600), tz = "UTC", origin = "1970-01-01"), depth = d24)
  )

  expect_equal(
    sort(c(
      mgtdhidt(tau = hours, id = 28434L, depth = d24),
      mgtdhidt(tau = hours, id = 28418L, depth = d24),
      mgtdhidt(tau = hours, id = 28630L, depth = d24)
    )),
    sort(c(
      mgtdhid(id = c(28434L, 28418L, 28630L), tau = hours[[1]], depth = d24),
      mgtdhid(id = c(28434L, 28418L, 28630L), tau = hours[[2]], depth = d24),
      mgtdhid(id = c(28434L, 28418L, 28630L), tau = hours[[3]], depth = d24),
      mgtdhid(id = c(28434L, 28418L, 28630L), tau = hours[[4]], depth = d24)
    ))
  )
  rm(d24, hours)
})


test_that("*mgtdhgeot* produces wrong results", {
  lat <- c(s28434 = 56.65, s28418 = 56.47, s23711 = 62.70, CP1 = 57.00)
  lon <- c(s28434 = 57.78, s28418 = 53.73, s23711 = 56.20, CP1 = 57.00)
  d24 <- 2.4

  # Test inside STATION_RADIUS
  expect_equal(
    c(
       mgtdhgeot(tau = 1440L, lat = lat[["s28434"]], lon = lon[["s28434"]], depth = d24)
      ,mgtdhgeot(tau = 1440L, lat = lat[["s28418"]], lon = lon[["s28418"]], depth = d24)
      ,mgtdhgeot(tau = 1440L, lat = lat[["s23711"]], lon = lon[["s23711"]], depth = d24)
    ),
    mgtdhid(id = c(28434L, 28418L, 23711L), tau = as.POSIXct("2023-03-02", tz = "UTC"), depth = d24)
  )

# Test out of triangle
  expect_equal(
    mgtdhgeot(
      tau = as.POSIXct("2023-03-02", tz = "UTC") + 3600 * c(0, 10, 100),
      lat[["CP1"]], lon[["CP1"]], depth = d24
    ),
    c(
       mgtdhid(id = 28434L, tau = 1440L, depth = d24)
      ,mgtdhid(id = 28434L, tau = 1450L, depth = d24)
      ,mgtdhid(id = 28434L, tau = 1540L, depth = d24)
    )
  )
  rm(d24, lon, lat)
})

test_that("*mgtdhgeot* and others produce wrong results without execution parallelization", {
  lat <- c(s28434 = 56.65, s28418 = 56.47, s23711 = 62.70, CP1 = 57, CP2 = 56.00)
  lon <- c(s28434 = 57.78, s28418 = 53.73, s23711 = 56.20, CP1 = 57, CP2 = 57.00)
  r   <- c(s28434 = 86986.91, s28418 = 209396.94, s28630 = 192034.70)  # Google maps distances from CP2 to the nearest stations, [m]
  d24 <- 2.4

  # Test inside STATION_RADIUS
  expect_equal(
    mgtdhgeo(head(lat, 3), head(lon, 3), tau = 1440L, depth = d24),
    mgtdhid(id = c(28434L, 28418L, 23711L), tau = as.POSIXct("2023-03-02", tz = "UTC"), depth = d24)
  )

  # Test out of triangle
  expect_equal(
    mgtdhgeo(lat[["CP1"]], lon[["CP1"]], tau = as.POSIXct("2023-03-02", tz = "UTC"), depth = d24),
    mgtdhid(id = 28434L, tau = 1440L, depth = d24)
  )

  # Test inside triangle
  expect_equal(
    mgtdhgeo(lat[["CP2"]], lon[["CP2"]], tau = 1440L, depth = d24),
    drop(
      mgtdhid(
        id = c(28434L, 28418L, 28630L), tau = as.POSIXct("2023-03-02", tz = "UTC"), depth = d24
      ) %*% drop(1/sapply(r, function(x) sum((x/r)^2)))
    )  ,
    tolerance = 1e-2
  )

  # Test inside triangle
  expect_equal(
    mgtdhgeot(tau = 1440L, lat[["CP2"]], lon[["CP2"]], depth = d24),
    mgtdhgeo(lat[["CP2"]], lon[["CP2"]], tau = as.POSIXct("2023-03-02", tz = "UTC"), depth = d24)
  )

  expect_equal(
    mgtdhgeot(tau = c(1440L, 1450L, 1540L), lat[["CP2"]], lon[["CP2"]], depth = d24),
    c(
       mgtdhgeo(lat[["CP2"]], lon[["CP2"]], tau = as.POSIXct("2023-03-02", tz = "UTC"), depth = d24)
      ,mgtdhgeo(lat[["CP2"]], lon[["CP2"]], tau = as.POSIXct("2023-03-02", tz = "UTC") + 3600 * 10, depth = d24)
      ,mgtdhgeo(lat[["CP2"]], lon[["CP2"]], tau = as.POSIXct("2023-03-02", tz = "UTC") + 3600 * 100, depth = d24)
    )
  )
  rm(d24, r, lon, lat)
})


test_that("*mgtdhgeot* and others produce wrong results utilizing parallel execution (if possible)", {
  lat <- c(s28434 = 56.65, s28418 = 56.47, s23711 = 62.70, CP1 = 57, CP2 = 56.00)
  lon <- c(s28434 = 57.78, s28418 = 53.73, s23711 = 56.20, CP1 = 57, CP2 = 57.00)
  r   <- c(s28434 = 86986.91, s28418 = 209396.94, s28630 = 192034.70)  # Google maps distances from CP2 to the nearest stations, [m]
  d24 <- 2.4

  # Test inside STATION_RADIUS
  expect_equal(
    mgtdhgeo(head(lat, 3), head(lon, 3), tau = 1440L, depth = d24, use_cluster = !nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))),
    mgtdhid(id = c(28434L, 28418L, 23711L), tau = as.POSIXct("2023-03-02", tz = "UTC"), depth = d24)
  )

  # Test out of triangle
  expect_equal(
    mgtdhgeo(lat[["CP1"]], lon[["CP1"]], tau = as.POSIXct("2023-03-02", tz = "UTC"), depth = d24, use_cluster = !nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))),
    mgtdhid(id = 28434L, tau = 1440L, depth = d24)
  )

  # Test inside triangle
  expect_equal(
    mgtdhgeo(lat[["CP2"]], lon[["CP2"]], tau = 1440L, depth = d24, use_cluster = !nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))),
    drop(
      mgtdhid(
        id = c(28434L, 28418L, 28630L), tau = as.POSIXct("2023-03-02", tz = "UTC"), depth = d24
      ) %*% drop(1/sapply(r, function(x) sum((x/r)^2)))
    )  ,
    tolerance = 1e-2
  )

  # Test inside triangle
  expect_equal(
    mgtdhgeot(tau = 1440L, lat[["CP2"]], lon[["CP2"]], depth = d24),
    mgtdhgeo(lat[["CP2"]], lon[["CP2"]], tau = as.POSIXct("2023-03-02", tz = "UTC"), depth = d24, use_cluster = !nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_", "")))
  )

  expect_equal(
    mgtdhgeot(tau = c(1440L, 1450L, 1540L), lat[["CP2"]], lon[["CP2"]], depth = d24),
    c(
       mgtdhgeo(lat[["CP2"]], lon[["CP2"]], tau = as.POSIXct("2023-03-02", tz = "UTC"), depth = d24, use_cluster = !nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_", "")))
      ,mgtdhgeo(lat[["CP2"]], lon[["CP2"]], tau = as.POSIXct("2023-03-02", tz = "UTC") + 3600 * 10, depth = d24, use_cluster = !nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_", "")))
      ,mgtdhgeo(lat[["CP2"]], lon[["CP2"]], tau = as.POSIXct("2023-03-02", tz = "UTC") + 3600 * 100, depth = d24, use_cluster = !nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_", "")))
    )
  )
  rm(d24, r, lon, lat)
})
