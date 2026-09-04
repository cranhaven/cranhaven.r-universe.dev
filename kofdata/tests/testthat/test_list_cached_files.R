context("list_cached_files")

# TODO: listing contents of directories?
test_that("list_cached_files works", {
  apikey <- Sys.getenv("KOFDATA_TEST_SECRET")
  skip_if(nchar(apikey) == 0)

  listing <- list_cached_files("testuser", apikey)

  expect_equal(listing, c("file.txt", "subdir"))
})

test_that("list_cached_files errors with invalid username", {
  expect_error(list_cached_files("GandalfTheWhite", "myapikey"), "Username not found")
})

test_that("list_cached_files errors with invalid api key", {
  expect_error(list_cached_files("testuser", "notmyapikey"), "Invalid API key")
})
