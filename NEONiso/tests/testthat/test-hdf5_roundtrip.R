# test-hdf5_roundtrip.R
# Tests that write calibration output to HDF5, read it back, and verify
# structure, attributes, and data values.

fin <- system.file("extdata",
  "NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5",
  package = "NEONiso", mustWork = TRUE)

# ---- Carbon round-trip ----

test_that("carbon calibration output file has correct HDF5 structure", {

  fout <- tempfile(fileext = ".h5")
  on.exit(unlink(fout), add = TRUE)

  co2 <- calibrate_carbon(fin, fout, "ONAQ",
    method = "linreg", calibration_half_width = 0.5,
    write_to_file = TRUE)

  # Verify top-level group
  top <- NEONiso:::h5_ls(fout)
  expect_true("ONAQ" %in% top$name)

  # Verify dp01 children
  dp01_children <- NEONiso:::h5_ls_group(fout, "ONAQ/dp01")
  expect_true(all(c("data", "qfqm", "ucrt") %in% dp01_children))

  # Verify isoCo2 data children include calData and ambient heights
  iso_children <- NEONiso:::h5_ls_group(fout, "ONAQ/dp01/data/isoCo2")
  expect_true("calData" %in% iso_children)
  # Should have at least one ambient height group
  height_groups <- iso_children[grepl("^000_", iso_children)]
  expect_gt(length(height_groups), 0)
})

test_that("carbon output file has correct site attributes", {

  fout <- tempfile(fileext = ".h5")
  on.exit(unlink(fout), add = TRUE)

  co2 <- calibrate_carbon(fin, fout, "ONAQ",
    method = "linreg", calibration_half_width = 0.5,
    write_to_file = TRUE)

  # Read attributes from input and output files
  attrs_in <- NEONiso:::h5_read_attrs(fin, "ONAQ")
  attrs_out <- NEONiso:::h5_read_attrs(fout, "ONAQ")

  # Attributes should match between input and output
  expect_equal(sort(names(attrs_out)), sort(names(attrs_in)))
  expect_equal(attrs_out$LvlMeasTow, attrs_in$LvlMeasTow)
})

test_that("carbon output file calData has correct dataset", {

  fout <- tempfile(fileext = ".h5")
  on.exit(unlink(fout), add = TRUE)

  co2 <- calibrate_carbon(fin, fout, "ONAQ",
    method = "linreg", calibration_half_width = 0.5,
    write_to_file = TRUE)

  # Check calData children
  cal_children <- NEONiso:::h5_ls_group(fout, "ONAQ/dp01/data/isoCo2/calData")
  expect_true("calRegressions" %in% cal_children)
})

test_that("carbon output file passes validate_output_file", {

  fout <- tempfile(fileext = ".h5")
  on.exit(unlink(fout), add = TRUE)

  co2 <- calibrate_carbon(fin, fout, "ONAQ",
    method = "linreg", calibration_half_width = 0.5,
    write_to_file = TRUE)

  # This should not error
  expect_no_error(
    NEONiso:::validate_output_file(fin, fout, "ONAQ", "Co2")
  )
})

test_that("carbon gainoffset output passes validate_output_file", {

  fout <- tempfile(fileext = ".h5")
  on.exit(unlink(fout), add = TRUE)

  suppressWarnings({
    co2 <- calibrate_carbon(fin, fout, "ONAQ",
      method = "gainoffset", calibration_half_width = 0.5,
      write_to_file = TRUE)
  })

  expect_no_error(
    NEONiso:::validate_output_file(fin, fout, "ONAQ", "Co2")
  )
})

# ---- Water round-trip ----

test_that("water calibration output file has correct HDF5 structure", {

  fout <- tempfile(fileext = ".h5")
  on.exit(unlink(fout), add = TRUE)

  h2o <- calibrate_water(fin, fout, "ONAQ", write_to_file = TRUE)

  # Verify top-level group
  top <- NEONiso:::h5_ls(fout)
  expect_true("ONAQ" %in% top$name)

  # Verify dp01 children
  dp01_children <- NEONiso:::h5_ls_group(fout, "ONAQ/dp01")
  expect_true(all(c("data", "qfqm", "ucrt") %in% dp01_children))

  # Verify isoH2o data children include calData
  iso_children <- NEONiso:::h5_ls_group(fout, "ONAQ/dp01/data/isoH2o")
  expect_true("calData" %in% iso_children)
})

test_that("water output file has correct site attributes", {

  fout <- tempfile(fileext = ".h5")
  on.exit(unlink(fout), add = TRUE)

  h2o <- calibrate_water(fin, fout, "ONAQ", write_to_file = TRUE)

  attrs_in <- NEONiso:::h5_read_attrs(fin, "ONAQ")
  attrs_out <- NEONiso:::h5_read_attrs(fout, "ONAQ")

  expect_equal(sort(names(attrs_out)), sort(names(attrs_in)))
  expect_equal(attrs_out$LvlMeasTow, attrs_in$LvlMeasTow)
})

test_that("water output file passes validate_output_file", {

  fout <- tempfile(fileext = ".h5")
  on.exit(unlink(fout), add = TRUE)

  h2o <- calibrate_water(fin, fout, "ONAQ", write_to_file = TRUE)

  expect_no_error(
    NEONiso:::validate_output_file(fin, fout, "ONAQ", "H2o")
  )
})

# ---- Standalone output function calls (fid = NULL) ----

test_that("write_carbon_calibration_data works standalone (fid = NULL)", {

  fout <- tempfile(fileext = ".h5")
  on.exit(unlink(fout), add = TRUE)

  co2 <- calibrate_carbon(fin, fout, "ONAQ",
    method = "linreg", calibration_half_width = 0.5,
    write_to_file = FALSE)

  # Set up the output file first
  NEONiso:::setup_output_file(fin, fout, "ONAQ", "Co2")

  # Write calibration data standalone (no fid passed)
  cal_df <- co2$cal_df
  expect_no_error(
    NEONiso:::write_carbon_calibration_data(fout, "ONAQ", cal_df,
                                             method = "linreg")
  )

  # Verify data was written
  cal_children <- NEONiso:::h5_ls_group(fout, "ONAQ/dp01/data/isoCo2/calData")
  expect_true("calRegressions" %in% cal_children)
})

test_that("write_carbon_ambient_data works standalone (fid = NULL)", {

  fout <- tempfile(fileext = ".h5")
  on.exit(unlink(fout), add = TRUE)

  co2 <- calibrate_carbon(fin, fout, "ONAQ",
    method = "linreg", calibration_half_width = 0.5,
    write_to_file = FALSE)

  NEONiso:::setup_output_file(fin, fout, "ONAQ", "Co2")

  # Write ambient data standalone
  expect_no_error(
    NEONiso:::write_carbon_ambient_data(fout, "ONAQ", co2$ciso_subset_cal)
  )

  # Verify ambient height groups were written
  iso_children <- NEONiso:::h5_ls_group(fout, "ONAQ/dp01/data/isoCo2")
  height_groups <- iso_children[grepl("^000_", iso_children)]
  expect_gt(length(height_groups), 0)
})

test_that("write_water_calibration_data works standalone (fid = NULL)", {

  fout <- tempfile(fileext = ".h5")
  on.exit(unlink(fout), add = TRUE)

  h2o <- calibrate_water(fin, fout, "ONAQ", write_to_file = FALSE)

  NEONiso:::setup_output_file(fin, fout, "ONAQ", "H2o")

  expect_no_error(
    NEONiso:::write_water_calibration_data(fout, "ONAQ", h2o$cal_df)
  )

  cal_children <- NEONiso:::h5_ls_group(fout, "ONAQ/dp01/data/isoH2o/calData")
  expect_true("calRegressions" %in% cal_children)
})

test_that("write_water_ambient_data works standalone (fid = NULL)", {

  fout <- tempfile(fileext = ".h5")
  on.exit(unlink(fout), add = TRUE)

  h2o <- calibrate_water(fin, fout, "ONAQ", write_to_file = FALSE)

  NEONiso:::setup_output_file(fin, fout, "ONAQ", "H2o")

  expect_no_error(
    NEONiso:::write_water_ambient_data(fout, "ONAQ", h2o$wiso_subset_cal)
  )

  iso_children <- NEONiso:::h5_ls_group(fout, "ONAQ/dp01/data/isoH2o")
  height_groups <- iso_children[grepl("^000_", iso_children)]
  expect_gt(length(height_groups), 0)
})

# ---- validate_output_file error path ----

test_that("validate_output_file errors on incomplete output file", {

  fout <- tempfile(fileext = ".h5")
  on.exit(unlink(fout), add = TRUE)

  # Create an output file with only the skeleton (no ambient data)
  NEONiso:::setup_output_file(fin, fout, "ONAQ", "Co2")

  # Write just the calibration data but skip ambient data,
  # so the output file will be missing height groups
  co2 <- calibrate_carbon(fin, fout, "ONAQ",
    method = "linreg", calibration_half_width = 0.5,
    write_to_file = FALSE)

  fid <- NEONiso:::h5_open(fout)
  NEONiso:::write_carbon_calibration_data(fout, "ONAQ", co2$cal_df,
                                           method = "linreg", fid = fid)
  NEONiso:::h5_close(fid)

  expect_error(
    NEONiso:::validate_output_file(fin, fout, "ONAQ", "Co2"),
    "Output file structure has diverged"
  )
})
