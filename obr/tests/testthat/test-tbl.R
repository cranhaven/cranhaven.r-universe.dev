# Tests for the obr_tbl class itself. Offline so they run on CRAN.

make_test_tbl <- function() {
  new_obr_tbl(
    data        = data.frame(year = c("2024-25", "2025-26"),
                             value = c(100, 110),
                             stringsAsFactors = FALSE),
    publication = "PFD",
    vintage     = "March 2026",
    source_url  = "https://obr.uk/download/public-finances-databank/",
    retrieved   = as.POSIXct("2026-04-25 12:00:00", tz = "UTC"),
    file_md5    = "abcdef0123456789"
  )
}

test_that("new_obr_tbl() returns an obr_tbl with expected attributes", {
  x <- make_test_tbl()
  expect_s3_class(x, "obr_tbl")
  expect_s3_class(x, "data.frame")

  prov <- attr(x, "obr_provenance")
  expect_false(is.null(prov))
  expect_equal(prov$publication, "PFD")
  expect_equal(prov$vintage, "March 2026")
  expect_equal(prov$source_url,
               "https://obr.uk/download/public-finances-databank/")
  expect_s3_class(prov$retrieved, "POSIXt")
  expect_equal(prov$file_md5, "abcdef0123456789")
})

test_that("obr_provenance() returns the attached metadata", {
  x <- make_test_tbl()
  prov <- obr_provenance(x)
  expect_type(prov, "list")
  expect_equal(prov$publication, "PFD")
  expect_equal(prov$vintage, "March 2026")
})

test_that("obr_provenance() returns NULL on an unwrapped data.frame", {
  expect_null(obr_provenance(data.frame(x = 1)))
})

test_that("subsetting an obr_tbl preserves the class and provenance", {
  x <- make_test_tbl()
  y <- x[x$value > 100, ]
  expect_s3_class(y, "obr_tbl")
  expect_equal(obr_provenance(y)$publication, "PFD")
  expect_equal(nrow(y), 1L)
})

test_that("as.data.frame() strips the obr_tbl class and provenance", {
  x <- make_test_tbl()
  y <- as.data.frame(x)
  expect_s3_class(y, "data.frame")
  expect_false(inherits(y, "obr_tbl"))
  expect_null(attr(y, "obr_provenance"))
})

test_that("print.obr_tbl() prints a provenance header", {
  x <- make_test_tbl()
  out <- capture.output(print(x))
  expect_true(any(grepl("obr_tbl", out)))
  expect_true(any(grepl("Public Finances Databank", out)))
  expect_true(any(grepl("March 2026", out)))
  expect_true(any(grepl("public-finances-databank", out)))
})

test_that("summary.obr_tbl() prints a provenance card", {
  x <- make_test_tbl()
  out <- capture.output(summary(x))
  expect_true(any(grepl("Publication:", out)))
  expect_true(any(grepl("Vintage:", out)))
  expect_true(any(grepl("Source URL:", out)))
  expect_true(any(grepl("Rows:", out)))
})

test_that("obr_url_vintage() extracts month-year from EFO slug", {
  expect_equal(
    obr_url_vintage(
      "https://obr.uk/download/march-2026-economic-and-fiscal-outlook-detailed-forecast-tables-aggregates/"
    ),
    "March 2026"
  )
  expect_equal(
    obr_url_vintage(
      "https://obr.uk/download/october-2024-economic-and-fiscal-outlook/"
    ),
    "October 2024"
  )
  expect_true(is.na(obr_url_vintage(
    "https://obr.uk/download/public-finances-databank/"
  )))
})

test_that("obr_publication_label() resolves the codes used in this package", {
  expect_equal(obr_publication_label("PFD"), "Public Finances Databank")
  expect_equal(obr_publication_label("HFD"),
               "Historical Official Forecasts Database")
  expect_equal(obr_publication_label("EFO"),
               "Economic and Fiscal Outlook")
  expect_equal(obr_publication_label("WTR"), "Welfare Trends Report")
  expect_equal(obr_publication_label("FSR"),
               "Fiscal Risks and Sustainability Report")
})

test_that("strip_footnote() preserves names that legitimately end in a digit", {
  expect_equal(strip_footnote("Income tax2"), "Income tax")
  expect_equal(strip_footnote("VAT3"), "VAT")
  # Year-bearing labels (not used in OBR receipts but worth guarding)
  expect_equal(strip_footnote("2024"), "2024")
  expect_equal(strip_footnote("Income tax"), "Income tax")
})
