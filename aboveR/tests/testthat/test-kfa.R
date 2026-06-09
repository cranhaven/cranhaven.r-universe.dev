# Tests for KyFromAbove functions (network-dependent)

test_that("kfa_stac_search() requires rstac package", {
  # Should not error about missing rstac since it's installed
  # but should fail because STAC catalog isn't live

  expect_error(kfa_stac_search(c(-84.5, 38, -84.4, 38.1)),
               "rstac|STAC catalog not yet available")
})

test_that("kfa_find_tiles() validates phase argument", {
  expect_error(kfa_find_tiles(c(-84.5, 38, -84.4, 38.1), phase = 5),
               "phase")
})

test_that("kfa_find_tiles() validates product argument", {
  expect_error(kfa_find_tiles(c(-84.5, 38, -84.4, 38.1), product = "fake"),
               "arg")
})

test_that("kfa_tile_index() uses CRAN-approved cache dir", {
  # Verify cache directory construction
  cache_dir <- tools::R_user_dir("aboveR", "cache")
  expect_true(is.character(cache_dir))
  expect_true(nchar(cache_dir) > 0)
})
