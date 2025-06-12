context("Testing functions in utils.R")

ram_url <- "https://depts.washington.edu/ramlegac/wordpress/databaseVersions"

# test_that("net_check doesn't error behind proxy server with show_error as F", {
#   skip_on_cran()
#   httr::with_config(httr::use_proxy(url = "http://google.com", port = 1234), {
#     expect_silent(net_check(ram_url, show_error = FALSE))
#     expect_false(net_check(ram_url, show_error = FALSE))
#   })
# })
#
# test_that("net_check errors behind proxy server with show_error as TRUE", {
#   skip_on_cran()
#   httr::with_config(httr::use_proxy(url = "http://google.com", port = 1234), {
#     expect_error(
#       net_check(ram_url, show_error = TRUE),
#       paste(
#         "Could not connect to the internet.",
#         "Please check your connection settings and try again."
#       )
#     )
#   })
# })
#
#
# test_that("net_check doesn't error when no net connection w/ show_error as F", {
#   skip_on_cran()
#   httptest::without_internet({
#     expect_silent(net_check(ram_url, show_error = FALSE))
#     expect_false(net_check(ram_url, show_error = FALSE))
#   })
# })
#
# test_that("net_check errors if no internet connection with show_error as T", {
#   skip_on_cran()
#   httptest::without_internet({
#     expect_error(
#       net_check(ram_url, show_error = TRUE),
#       paste(
#         "Could not connect to the internet.",
#         "Please check your connection settings and try again."
#       )
#     )
#   })
# })

test_that("find_latest behaves correctly", {
  skip_on_cran()
  test_url1 <- "http://httpbin.org/status/300"
  test_url3 <- "http://httpbin.org/status/404"
  test_url4 <- "http://httpbin.org/status/500"

  # test find_latest with no internet connection
  httptest::without_internet({
    expect_silent(find_latest(ram_url))
    expect_equal(find_latest(ram_url), "4.44")
  })
  # test find_latest behaves as expected with proxy connection issues
  httr::with_config(httr::use_proxy(url = "http://google.com", port = 1234), {
    expect_silent(find_latest(ram_url))
    expect_equal(find_latest(ram_url), "4.44")
  })
  # test find_latest behaves as expected when response has failed
  expect_equal(find_latest(test_url1), "4.44")
  expect_equal(find_latest(test_url3), "4.44")
  expect_equal(find_latest(test_url4), "4.44")
})


test_that("ram_dir returns a path", {
  skip_on_cran()
  expect_is(ram_dir(), "character")
  expect_is(ram_dir(vers = 2.0), "character")
  expect_silent(ram_dir())
})

test_that("ram_dir returns the right path when RAM_HOME is set", {
  skip_on_cran()
  temp_path <- tempdir()
  Sys.setenv(RAM_HOME = temp_path)
  expect_equal(ram_dir(), temp_path)
})

test_that("ram_dir returns the right path when RAM_HOME is not set", {
  skip_on_cran()
  Sys.setenv(RAM_HOME = "")
  expect_equal(ram_dir(), rappdirs::user_data_dir("ramlegacy"))
})

test_that("check_version_arg works with valid versions", {
  skip_on_cran()
  expect_true(check_version_arg(3.0))
  expect_true(check_version_arg(2.5))
  expect_true(check_version_arg(2.0))
  expect_true(check_version_arg(1.0))
  expect_true(check_version_arg(4.3))
})

test_that("check_version_arg fails with invalid versions", {
  skip_on_cran()
  expect_error(
    check_version_arg(1.1),
    "Invalid version number."
  )
  expect_error(
    check_version_arg(1.5),
    "Invalid version number."
  )
  expect_error(
    check_version_arg(2.4),
    "Invalid version number."
  )
  expect_error(
    check_version_arg(3.5),
    "Invalid version number."
  )
  expect_error(
    check_version_arg(4.0),
    "Invalid version number."
  )
  expect_error(
    check_version_arg(c("3.5", "2.0")),
    "Please pass in only one version number."
  )
})

test_that("check_path works with valid paths", {
  skip_on_cran()
  expect_true(check_path(path = ram_dir()))
  expect_true(check_path(path = ram_dir(vers = 1.0)))
  expect_true(check_path(path = ram_dir(vers = 2.0)))
  expect_true(check_path(path = ram_dir(vers = 2.5)))
  expect_true(check_path(path = ram_dir(vers = 3.0)))
  expect_true(check_path(path = ram_dir(vers = 4.3)))
  expect_true(check_path(path = tempfile("ramlegacy", tempdir())))
})

test_that("check_path errors out with invalid paths", {
  skip_on_cran()
  expect_error(check_path(2.0))
})
