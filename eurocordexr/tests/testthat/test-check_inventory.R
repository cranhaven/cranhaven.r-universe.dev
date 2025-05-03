setDTthreads(1)

fn_zip <- system.file("extdata", "inv-test-files.zip", package = "eurocordexr")
tmpdir <- tempdir()
unzip(fn_zip, exdir = tmpdir)

test_that("simple checks", {
  expect_snapshot(check_inventory(get_inventory(fs::path(tmpdir, "testdata", "mixed-vars"))))
  expect_snapshot(check_inventory(get_inventory(fs::path(tmpdir, "testdata", "dup-ens"))))
  expect_snapshot(check_inventory(get_inventory(fs::path(tmpdir, "testdata", "incomplete-period"))))
})

