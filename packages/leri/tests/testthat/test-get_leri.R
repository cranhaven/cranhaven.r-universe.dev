context('get_leri')

test_that("parse_product errors when given multiple products", {
  expect_error(parse_product(c('one thing', 'two thing')),
               regexp = 'must have length one')
})

test_that("parse_product errors when given invalid products", {
  expect_error(parse_product('invalid'), regexp = 'must be one of')
})

test_that("parse_product correctly finds 8 day products", {
  yes_8d <- parse_product('8 day ac')
  not_8d <- parse_product('7 month')
  expect_true(yes_8d$is_8_day)
  expect_false(not_8d$is_8_day)
})

test_that("date args that can't be coerced to dates raise errors", {
  expect_error(get_leri(date = 'banana', product = '8 day ac'),
               regexp = 'Try formatting')
})

test_that("dates in the future raise errors", {
  expect_error(get_leri(date = '2999-01-01', product = '1 month'),
               regexp = 'must be <=')
})

test_that("dates prior to 2000 raise errors", {
  expect_error(get_leri(date = '1999-12-31', product = '1 month'),
               regexp = 'prior to 2000')
})

test_that("dates prior to April raise errors for 8 day products", {
  expect_error(get_leri(date = '2008-03-14', product = '8 day nac'),
               regexp = 'available from April 01')
})

test_that("dates after October raise errors for 8 day products", {
  expect_error(get_leri(date = '2008-11-01', product = '8 day ac'),
               regexp = 'to October 31')
})

test_that("find_8d_interval matches known LERI date intervals", {
  date <- as.Date(rep(c('2018-04-12', '2018-08-29', '2018-07-03'), each = 2))
  interval <- c('Apr01-Apr14', 'Apr07-Apr14',
                'Apr01-Sep05', 'Aug29-Sep05',
                'Apr01-Jul03', 'Jun26-Jul03')
  prod <- rep(c('8 day ac', '8 day nac'), length.out = length(date))
  for (i in seq_along(date)) {
    expect_equal(find_8d_interval(date[i], product = prod[i]), interval[i])
  }
})

test_that("make_url works for 8 day products", {
  parsed_product <- parse_product('8 day nac')
  parsed_date <- parse_date('2018-05-25', parsed_product)
  expected_url <- paste0('ftp://ftp.cdc.noaa.gov/Projects/LERI/CONUS_archive/',
                         '8day/data/2018/LERI_8day-nac_May25-Jun01_2018.nc')
  expect_equal(make_url(parsed_date, parsed_product), expected_url)
})

test_that("make_url works for monthly products", {
  parsed_product <- parse_product('12 month')
  parsed_date <- parse_date('2018-05-25', parsed_product)
  expected_url <- paste0('ftp://ftp.cdc.noaa.gov/Projects/LERI/CONUS_archive/',
                         'data/2018/LERI_12mn_20180501.nc')
  expect_equal(make_url(parsed_date, parsed_product), expected_url)
})

test_that("make_url returns one url when only one file is needed", {
  parsed_product <- parse_product('12 month')
  parsed_date <- parse_date(c('2018-05-25', '2018-05-26'), parsed_product)
  expected_url <- paste0('ftp://ftp.cdc.noaa.gov/Projects/LERI/CONUS_archive/',
                         'data/2018/LERI_12mn_20180501.nc')
  expect_equal(make_url(parsed_date, parsed_product), expected_url)
})

test_that("make_url returns multiple urls when needed", {
  parsed_product <- parse_product('12 month')
  parsed_date <- parse_date(c('2018-05-25', '2018-06-25'), parsed_product)
  expected_urls <- paste0('ftp://ftp.cdc.noaa.gov/Projects/LERI/CONUS_archive/',
                         'data/2018/LERI_12mn_20180', c(5, 6), '01.nc')
  urls <- make_url(parsed_date, parsed_product)
  expect_length(urls, 2)
  expect_equal(urls, expected_urls)
})

test_that("Single date returns a RasterLayer", {
  skip_on_cran()
  skip_on_ci()
  r <- get_leri(date = "2018-10-31", product = '8 day nac', dir = ".")
  expect_is(r, 'RasterLayer')
  expect_equal(raster::nlayers(r), 1)
  expect_equal(names(r), "LERI_8day.nac_Oct24.Oct31_2018.nc")
  unlink(list.files(pattern = ".nc"))
})

test_that("Multiple dates return a RasterBrick", {
  skip_on_cran()
  skip_on_ci()
  r <- get_leri(date = c('2018-01-04', '2018-02-13'), product = '1 month')
  expect_is(r, 'RasterBrick')
  expect_equal(raster::nlayers(r), 2)
  expected_names <- c("LERI_01mn_20180101.nc", "LERI_01mn_20180201.nc")
  expect_equal(names(r), expected_names)
})
