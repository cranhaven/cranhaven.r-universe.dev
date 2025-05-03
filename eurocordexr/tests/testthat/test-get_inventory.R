setDTthreads(1)

fn_zip <- system.file("extdata", "inv-test-files.zip", package = "eurocordexr")
tmpdir <- tempdir()
unzip(fn_zip, exdir = tmpdir)

test_that("basic functionality", {
  expect_equal(nrow(get_inventory(fs::path(tmpdir, "testdata", "mixed-vars"))), 5)
})







