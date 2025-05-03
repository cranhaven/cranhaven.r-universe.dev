setDTthreads(1)

fn_zip <- system.file("extdata", "inv-test-files-cmip5.zip", package = "eurocordexr")
tmpdir <- tempdir()
unzip(fn_zip, exdir = tmpdir)

dat_inv <- get_inventory_cmip5(fs::path(tmpdir, "testdata-cmip5", "basic"))

test_that("cmip5 vs eurocordex", {
  expect_error(check_inventory(dat_inv), "CMIP5")
  expect_no_error(check_inventory_cmip5(dat_inv))
})

test_that("simple checks", {
  expect_snapshot(check_inventory_cmip5(get_inventory_cmip5(fs::path(tmpdir, "testdata-cmip5", "basic"))))
  expect_snapshot(check_inventory_cmip5(get_inventory_cmip5(fs::path(tmpdir, "testdata-cmip5", "mult-ens"))))
  expect_snapshot(check_inventory_cmip5(get_inventory_cmip5(fs::path(tmpdir, "testdata-cmip5", "incomplete-period"))))
  expect_snapshot(check_inventory_cmip5(get_inventory_cmip5(fs::path(tmpdir, "testdata-cmip5")),
                                        check_hist = TRUE))
})


