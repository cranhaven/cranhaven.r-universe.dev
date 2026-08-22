# Vintage layer tests. Mostly offline; one integration test for pinning.

test_that("obr_efo_vintages() returns a structured table", {
  v <- obr_efo_vintages()
  expect_s3_class(v, "data.frame")
  expect_named(v, c("publication", "vintage", "date", "slug"))
  expect_true(all(v$publication == "EFO"))
  expect_s3_class(v$date, "Date")
  expect_match(v$slug, "-economic-and-fiscal-outlook$", all = TRUE)

  # 33 EFOs from June 2010 to March 2026 inclusive
  expect_gte(nrow(v), 30L)
  expect_true("June 2010"   %in% v$vintage)
  expect_true("October 2024" %in% v$vintage)
  expect_true("March 2026"  %in% v$vintage)
  # Dates are unique and ordered ascending in the table source
  expect_equal(anyDuplicated(v$vintage), 0L)
  expect_equal(anyDuplicated(v$date),    0L)
})

test_that("obr_as_of() returns the most recent EFO published on or before a date", {
  expect_equal(obr_as_of("2024-10-31"), "October 2024")
  expect_equal(obr_as_of("2024-10-29"), "March 2024")
  expect_equal(obr_as_of("2025-01-01"), "October 2024")
  expect_equal(obr_as_of("2026-04-25"), "March 2026")

  # Date object also accepted
  expect_equal(obr_as_of(as.Date("2024-10-30")), "October 2024")
})

test_that("obr_as_of() errors when no EFO had been published yet", {
  expect_error(obr_as_of("2010-05-01"), regexp = "No EFO had been published")
})

test_that("obr_as_of() errors on un-coercible date input", {
  expect_error(obr_as_of("not-a-date"), regexp = "Date")
})

test_that("obr_pin() and obr_unpin() round-trip the global option", {
  on.exit(options(obr.efo_vintage = NULL), add = TRUE)

  expect_null(obr_pinned())

  res <- suppressMessages(obr_pin("October 2024"))
  expect_equal(res, "October 2024")
  expect_equal(obr_pinned(), "October 2024")
  expect_equal(getOption("obr.efo_vintage"), "October 2024")

  prev <- suppressMessages(obr_unpin())
  expect_equal(prev, "October 2024")
  expect_null(obr_pinned())
  expect_null(getOption("obr.efo_vintage"))
})

test_that("obr_pin(NULL) is equivalent to obr_unpin()", {
  on.exit(options(obr.efo_vintage = NULL), add = TRUE)
  suppressMessages(obr_pin("March 2025"))
  expect_equal(obr_pinned(), "March 2025")
  suppressMessages(obr_pin(NULL))
  expect_null(obr_pinned())
})

test_that("obr_pin() rejects unknown vintages", {
  on.exit(options(obr.efo_vintage = NULL), add = TRUE)
  expect_error(obr_pin("Banana 2024"), regexp = "Unknown EFO vintage")
})

test_that("efo_url_for_vintage() builds the standard OBR slug pattern", {
  url <- efo_url_for_vintage("March 2026", "detailed-forecast-tables-aggregates")
  expect_equal(
    url,
    "https://obr.uk/download/march-2026-economic-and-fiscal-outlook-detailed-forecast-tables-aggregates/"
  )
  url2 <- efo_url_for_vintage("October 2024", "detailed-forecast-tables-economy")
  expect_equal(
    url2,
    "https://obr.uk/download/october-2024-economic-and-fiscal-outlook-detailed-forecast-tables-economy/"
  )
})

test_that("vintage_cache_tag() produces filename-safe slugs", {
  expect_equal(vintage_cache_tag("March 2026"),    "march_2026")
  expect_equal(vintage_cache_tag("October 2024"),  "october_2024")
  expect_equal(vintage_cache_tag("July 2015"),     "july_2015")
})

test_that("resolve_efo_vintage() respects precedence: arg > pin > NULL", {
  on.exit(options(obr.efo_vintage = NULL), add = TRUE)

  # Both NULL
  options(obr.efo_vintage = NULL)
  expect_null(resolve_efo_vintage(NULL))

  # Only pin set
  options(obr.efo_vintage = "October 2024")
  expect_equal(resolve_efo_vintage(NULL), "October 2024")

  # Explicit arg overrides pin
  expect_equal(resolve_efo_vintage("March 2025"), "March 2025")
})

test_that("get_efo_fiscal(vintage = ...) downloads the pinned vintage", {
  skip_on_cran()
  skip_if_offline()
  op <- options(obr.cache_dir = tempfile("obr_vintage_test_"))
  on.exit(options(op), add = TRUE)

  res <- get_efo_fiscal(vintage = "March 2026")
  expect_s3_class(res, "obr_tbl")
  prov <- obr_provenance(res)
  expect_equal(prov$publication, "EFO")
  expect_equal(prov$vintage, "March 2026")
  expect_match(prov$source_url, "march-2026-economic-and-fiscal-outlook")
})

test_that("efo_url_for_vintage() constructs slugs for well-formed unknown vintages", {
  expect_warning(
    url <- efo_url_for_vintage("November 2026", "detailed-forecast-tables-aggregates"),
    regexp = "not in this version's EFO calendar"
  )
  expect_equal(
    url,
    "https://obr.uk/download/november-2026-economic-and-fiscal-outlook-detailed-forecast-tables-aggregates/"
  )
  expect_error(
    efo_url_for_vintage("Nonsense 2026", "detailed-forecast-tables-aggregates"),
    regexp = "Unknown EFO vintage"
  )
})

test_that("obr_pin() accepts a well-formed unknown vintage with a warning", {
  on.exit(options(obr.efo_vintage = NULL), add = TRUE)
  expect_warning(obr_pin("November 2026"),
                 regexp = "not in this version's EFO calendar")
  expect_equal(obr_pinned(), "November 2026")
  obr_unpin()
  expect_error(obr_pin("NotAMonth 2026"), regexp = "Unknown EFO vintage")
})
