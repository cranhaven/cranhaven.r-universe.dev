test_that("get_data() returns data and in the proper format", {
  vcr::use_cassette("regioner_get_data", {
    response <- get_data("regioner")
  })
  vcr::use_cassette("regioner_get_data_no_list", {
    response_no_list <- get_data("regioner", as_list = FALSE)
  })
  vcr::use_cassette("regioner_get_data_list", {
    response_list <- get_data("regioner", as_list = TRUE)
  })

  expected_list <- list(
    dagi_id = 389098,
    kode = 1081,
    navn = "Region Nordjylland",
    nuts2 = "DK05",
    ændret = "2024-10-04T21:02:54.978Z",
    geo_ændret = "2024-10-04T21:02:54.978Z",
    geo_version = 32,
    bbox_xmin = 8.1895166,
    bbox_ymin = 56.534547,
    bbox_xmax = 11.2259914,
    bbox_ymax = 57.760255,
    visueltcenter_x = 10.1128291,
    visueltcenter_y = 57.30716
  )

  expected <- as.data.frame(expected_list)
  expected_no_list <- expected

  expect_equal(response[1, ], expected[1, ])
  expect_equal(response_no_list[1, ], expected_no_list[1, ])
  expect_equal(response_list[[1]], expected_list)
})

test_that("get_data(section = 'afstemningsomraader') snapshot", {
  skip_on_cran()
  expect_snapshot(get_data(section = "afstemningsomraader"))
})

vcr::use_cassette("get_data_kommuner_snapshot", {
  test_that("get_data(section = 'kommuner') snapshot", {
    expect_snapshot(get_data(section = "kommuner"))
  })
})

vcr::use_cassette("get_data_politikredse_snapshot", {
  test_that("get_data(section = 'politikredse') snapshot", {
    expect_snapshot(get_data(section = "politikredse"))
  })
})

vcr::use_cassette("get_data_regioner_snapshot", {
  test_that("get_data(section = 'regioner') snapshot", {
    expect_snapshot(get_data(section = "regioner"))
  })
})

test_that("get_data(section = 'section that does not exists') error", {
  expect_error(get_data(section = "section that does not exists"))
})
