# obr_headroom() tests. Validation is offline; the derivation test hits
# the live EFO aggregates workbook and is skipped on CRAN.

test_that("obr_headroom() validates target_year", {
  expect_error(obr_headroom(target_year = "2029"),
               regexp = "fiscal-year string")
  expect_error(obr_headroom(target_year = 2029),
               regexp = "fiscal-year string")
})

test_that("obr_headroom() flips the sign of the current budget deficit", {
  skip_on_cran()
  skip_if_offline()
  op <- options(obr.cache_dir = tempdir())
  on.exit(options(op), add = TRUE)

  hr  <- obr_headroom()
  efo <- get_efo_table("6.5")

  expect_s3_class(hr, "obr_tbl")
  expect_true(all(hr$series == "Current budget surplus"))
  expect_true(all(hr$unit == "gbp_bn"))

  cbd <- efo[efo$series == "Current budget deficit", ]
  joined <- merge(hr, cbd, by = "period")
  expect_gt(nrow(joined), 0L)
  expect_equal(joined$value.x, -joined$value.y)
})

test_that("obr_headroom() flags the target year and warns when absent", {
  skip_on_cran()
  skip_if_offline()
  op <- options(obr.cache_dir = tempdir())
  on.exit(options(op), add = TRUE)

  years <- obr_headroom()$period
  ty    <- years[length(years)]
  hr    <- obr_headroom(target_year = ty)
  expect_true("is_target_year" %in% names(hr))
  expect_equal(sum(hr$is_target_year), 1L)
  expect_equal(hr$period[hr$is_target_year], ty)

  expect_warning(obr_headroom(target_year = "1999-00"),
                 regexp = "not a year in this EFO forecast")
})
