setDTthreads(1)

fn_zip <- system.file("extdata", "inv-test-files-cmip5.zip", package = "eurocordexr")
tmpdir <- tempdir()
unzip(fn_zip, exdir = tmpdir)


test_that("no errors for basic functionality", {
  expect_no_error(get_inventory_cmip5(fs::path(tmpdir, "testdata-cmip5", "basic")))
})


dat_inv <- get_inventory_cmip5(fs::path(tmpdir, "testdata-cmip5", "basic"))
test_that("basic functionality", {
  expect_equal(nrow(dat_inv), 15)
  expect_setequal(dat_inv$timefreq, c("fx", "day", "Amon"))
  expect_true(dat_inv[timefreq == "day" & gcm == "NorESM1-M", period_contiguous])
  expect_false(dat_inv[timefreq == "day" & gcm == "HadGEM2-ES", period_contiguous])
  expect_true(dat_inv[timefreq == "Amon" & gcm == "CNRM-CM5-2", period_contiguous])
  expect_false(dat_inv[timefreq == "Amon" & gcm == "CMCC-CESM", period_contiguous])
})


test_that("aux only", {
  expect_no_error(get_inventory_cmip5(fs::path(tmpdir, "testdata-cmip5", "aux-only")))
})
