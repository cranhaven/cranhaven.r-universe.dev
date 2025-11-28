test_that("empty please_install()", {

  expect_null(please_install(character(0L)))

  if (!interactive()) {
    expect_error(please_install("stats"),
      "Please run in interactive session", class = "usethis_error"
    )
  }
})
