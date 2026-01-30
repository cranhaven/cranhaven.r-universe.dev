test_that("calc_bai returns correct structure", {
 # generate data
  dt.rw <- data.table::data.table(
    radius_id = rep(paste0("R", 1:3), each = 5),
    year      = rep(2001:2005, times = 3),
    rw_mm     = round(runif(15, 0.5, 3.5),2)
  )
  setorder(dt.rw, radius_id, year)
  dt.rw <- calc_bai(dt.rw, "radius_id", "rw_mm")
  testthat::expect_s3_class(dt.rw, "data.table")
  # Check for required columns
  testthat::expect_contains(names(dt.rw), c( "ageC", "ba_cm2_t_1", "bai_cm2"))
  testthat::expect_false(anyNA(dt.rw[, .(ageC, ba_cm2_t_1, bai_cm2)]))
  })

test_that("read_rwl returns correct structure", {
  # read data from ITRDB
  # not test the online version, due to timeout issue occasionally
  # dir.src <- "https://www.ncei.noaa.gov/pub/data/paleo/treering/measurements/northamerica/canada"
  # rwl <- "cana615.rwl"

  file <- system.file("extdata", "cana615.rwl", package = "growthTrendR")
  stopifnot(file != "")
  dir.src <- dirname(file)
  rwl <- basename(file)
  dt.rwl <- read_rwl(dir.src, rwl)
  testthat::expect_true(is.list(dt.rwl))
  # Check for required columns
  testthat::expect_contains(names(dt.rwl), c( "dt.header",  "dt.rw_long"))
  testthat::expect_contains(names(dt.rwl$dt.rw_long), c( "id.core", "year" ,   "rw_mm",   "fn"     ))
  testthat::expect_false(anyNA(dt.rwl$dt.rw_long[, c("id.core","year","fn")]))
})


test_that("prepare_samples_clim returns correct structure", {

  ## load processed test data
  dt.samples_trt <- get_test_samples_trt()

  ## input class
  expect_s3_class(dt.samples_trt, "cfs_format")

  ## load climate data
  dt.clim <- data.table::fread(
    system.file("extdata", "dt.clim.csv", package = "growthTrendR")
  )

  ## run function
  dt.samples_clim <- prepare_samples_clim(
    dt.samples_trt,
    dt.clim = dt.clim,
    calbai = TRUE
  )

  ## output class
  expect_true(
    inherits(dt.samples_clim, "data.frame") ||
      inherits(dt.samples_clim, "data.table")
  )


  ## required structural columns
  expect_true(all(c("uid_radius", "year") %in% names(dt.samples_clim)))

  ## BAI computed
  testthat::expect_contains(names(dt.samples_clim), c( "ageC", "ba_cm2_t_1", "bai_cm2"))

  ## climate variables merged
  expect_true(all(names(dt.clim) %in% names(dt.samples_clim)))

  ## no missing keys after merge
  expect_false(anyNA(dt.samples_clim$uid_radius))
  expect_false(anyNA(dt.samples_clim$year))
})
