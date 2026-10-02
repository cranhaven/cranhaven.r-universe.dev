test_that("status_check parses the services currently present in the RSS feed", {
  local_mocked_bindings(
    tidyfeed = function(...) {
      data.frame(
        item_title = c(
          "/adresser? - Operational",
          "/rest/arkivmeta/v3/kort? - Operational",
          "Dataforsyningen.dk - Operational"
        )
      )
    },
    .package = "tidyRSS"
  )

  observed_df <- status_check(return_df = TRUE)

  expected_df <- data.frame(
    service = c(
      "/adresser?",
      "/rest/arkivmeta/v3/kort?",
      "Dataforsyningen.dk"
    ),
    status = "OK"
  )

  expect_equal(observed_df, expected_df)
})

cli::test_that_cli("status_check reports an operational feed", {
  local_mocked_bindings(
    tidyfeed = function(...) {
      data.frame(
        item_title = c(
          "/adresser? - Operational",
          "Dataforsyningen.dk - Operational"
        )
      )
    },
    .package = "tidyRSS"
  )

  testthat::expect_snapshot({
    status_check()
  })
})

test_that("status_check reports unavailable services", {
  local_mocked_bindings(
    tidyfeed = function(...) {
      data.frame(
        item_title = c(
          "/adresser? - Operational",
          "Dataforsyningen.dk - Major Outage"
        )
      )
    },
    .package = "tidyRSS"
  )

  expect_message(status_check(), "Dataforsyningen.dk is not operational")
  expect_error(
    status_check(error_if_unavailable = TRUE),
    "Dataforsyningen.dk is not operational"
  )
})

test_that("status_check handles missing RSS item titles", {
  local_mocked_bindings(
    tidyfeed = function(...) {
      data.frame(
        item_title = c(NA_character_, "Dataforsyningen.dk - Operational")
      )
    },
    .package = "tidyRSS"
  )

  expect_equal(
    suppressMessages(status_check(return_df = TRUE)),
    data.frame(service = "Dataforsyningen.dk", status = "OK")
  )
})
