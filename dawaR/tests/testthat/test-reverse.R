vcr::use_cassette("reverse_geocode_regioner_1", {
  test_that("reverse geocode regioner 1", {
    expect_snapshot(reverse("regioner", x = 12.58515, y = 55.68324))
  })
})

vcr::use_cassette("reverse_geocode_afstemningsomraader", {
  test_that("reverse geocode afstemningsomraader", {
    expect_snapshot(reverse(
      "afstemningsomraader",
      x = 10.214710200889165,
      y = 56.15353822157923
    ))
  })
})

vcr::use_cassette("reverse_geocode_regioner_2", {
  test_that("reverse geocode regioner 2", {
    expect_snapshot(reverse(
      "regioner",
      y = 56.96054611500569,
      x = 9.284235987473544
    ))
  })
})

vcr::use_cassette("reverse_geocode_politikredse", {
  test_that("reverse geocode politikredse", {
    expect_snapshot(reverse(
      "politikredse",
      y = 55.32309442067793,
      x = 11.5534117505933
    ))
  })
})

test_that("reverse geocode regioner error", {
  skip_on_cran()
  ## Coords from austria - Fails softly with message
  expect_snapshot(reverse(
    "regioner",
    y = 47.870337094048274,
    x = 15.74573536789984,
    verbose = FALSE
  ))
})
