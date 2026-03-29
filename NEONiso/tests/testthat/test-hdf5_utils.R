# test-hdf5_utils.R
# Direct unit tests for HDF5 wrapper functions in hdf5_utils.R

test_that(".hdf5_backend returns a valid backend string", {
  backend <- NEONiso:::.hdf5_backend()
  expect_true(backend %in% c("hdf5r", "rhdf5"))
})

test_that("h5_create_file creates a new HDF5 file and returns a handle", {
  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp), add = TRUE)

  fid <- NEONiso:::h5_create_file(tmp)
  expect_true(file.exists(tmp))

  # Clean up handle
  NEONiso:::h5_close(fid)
})

test_that("h5_create_file overwrites an existing file", {
  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp), add = TRUE)

  # Create file, add a group, close
  fid <- NEONiso:::h5_create_file(tmp)
  NEONiso:::h5_create_group(fid, "testgroup")
  NEONiso:::h5_close(fid)

  # Recreate - should start fresh
  # rhdf5 warns about concurrent file handles; suppress since this is expected
  suppressWarnings({
    fid2 <- NEONiso:::h5_create_file(tmp)
    contents <- NEONiso:::h5_ls(tmp)
  })
  # Fresh file should have no groups
  expect_equal(nrow(contents), 0)
  NEONiso:::h5_close(fid2)
})

test_that("h5_create_group and h5_open_group work", {
  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp), add = TRUE)

  fid <- NEONiso:::h5_create_file(tmp)

  # Create nested groups
  grp <- NEONiso:::h5_create_group(fid, "level1")
  NEONiso:::h5_close_group(grp)
  grp2 <- NEONiso:::h5_create_group(fid, "level1/level2")
  NEONiso:::h5_close_group(grp2)

  # Open and verify
  grp_opened <- NEONiso:::h5_open_group(fid, "level1")
  # Should not error

  NEONiso:::h5_close_group(grp_opened)
  NEONiso:::h5_close(fid)

  # Verify with h5_ls
  contents <- NEONiso:::h5_ls(tmp)
  expect_true("level1" %in% contents$name)
})

test_that("h5_write_attr and h5_read_attrs round-trip correctly", {
  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp), add = TRUE)

  fid <- NEONiso:::h5_create_file(tmp)
  grp <- NEONiso:::h5_create_group(fid, "mygroup")

  # Write attributes
  NEONiso:::h5_write_attr(grp, "site", "ONAQ")
  NEONiso:::h5_write_attr(grp, "nheights", 4L)
  NEONiso:::h5_write_attr(grp, "pi_value", 3.14159)

  NEONiso:::h5_close_group(grp)
  NEONiso:::h5_close(fid)

  # Read back
  attrs <- NEONiso:::h5_read_attrs(tmp, "mygroup")
  expect_type(attrs, "list")
  expect_true("site" %in% names(attrs))
  expect_true("nheights" %in% names(attrs))
  expect_true("pi_value" %in% names(attrs))
  # rhdf5 returns attributes as arrays with dim=1, so ignore_attr = TRUE
  expect_equal(attrs$site, "ONAQ", ignore_attr = TRUE)
  expect_equal(attrs$nheights, 4L, ignore_attr = TRUE)
  expect_equal(attrs$pi_value, 3.14159, tolerance = 1e-5, ignore_attr = TRUE)
})

test_that("h5_write_dataset writes data.frames correctly", {
  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp), add = TRUE)

  fid <- NEONiso:::h5_create_file(tmp)
  grp <- NEONiso:::h5_create_group(fid, "data")

  # Write a data.frame
  df <- data.frame(
    timeBgn = c("2019-05-01T00:00:00.000Z", "2019-05-01T00:09:00.000Z"),
    timeEnd = c("2019-05-01T00:09:00.000Z", "2019-05-01T00:18:00.000Z"),
    mean = c(410.5, 412.3),
    vari = c(0.1, 0.2),
    stringsAsFactors = FALSE
  )

  NEONiso:::h5_write_dataset(grp, "test_dataset", df)
  NEONiso:::h5_close_group(grp)
  NEONiso:::h5_close(fid)

  # Verify the file has the dataset
  children <- NEONiso:::h5_ls_group(tmp, "data")
  expect_true("test_dataset" %in% children)
})

test_that("h5_ls returns top-level groups", {
  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp), add = TRUE)

  fid <- NEONiso:::h5_create_file(tmp)
  NEONiso:::h5_create_group(fid, "ONAQ")
  NEONiso:::h5_create_group(fid, "ONAQ/dp01")
  NEONiso:::h5_close(fid)

  result <- NEONiso:::h5_ls(tmp)
  expect_s3_class(result, "data.frame")
  expect_true("name" %in% names(result))
  # Should only return top-level group
  expect_true("ONAQ" %in% result$name)
})

test_that("h5_ls_group returns children of a specific group", {
  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp), add = TRUE)

  fid <- NEONiso:::h5_create_file(tmp)
  NEONiso:::h5_create_group(fid, "ONAQ")
  NEONiso:::h5_create_group(fid, "ONAQ/dp01")
  NEONiso:::h5_create_group(fid, "ONAQ/dp01/data")
  NEONiso:::h5_create_group(fid, "ONAQ/dp01/qfqm")
  NEONiso:::h5_create_group(fid, "ONAQ/dp01/ucrt")
  NEONiso:::h5_close(fid)

  children <- NEONiso:::h5_ls_group(tmp, "ONAQ/dp01")
  expect_type(children, "character")
  expect_setequal(children, c("data", "qfqm", "ucrt"))
})

test_that("h5_open opens file for read-write", {
  tmp <- tempfile(fileext = ".h5")
  on.exit(unlink(tmp), add = TRUE)

  # Create file with a group
  fid <- NEONiso:::h5_create_file(tmp)
  NEONiso:::h5_create_group(fid, "existing")
  NEONiso:::h5_close(fid)

  # Reopen and add another group
  fid2 <- NEONiso:::h5_open(tmp)
  NEONiso:::h5_create_group(fid2, "new_group")
  NEONiso:::h5_close(fid2)

  # Verify both groups exist
  contents <- NEONiso:::h5_ls(tmp)
  expect_true("existing" %in% contents$name)
  expect_true("new_group" %in% contents$name)
})

test_that("h5_ls on real NEON file returns site code", {
  fin <- system.file("extdata",
    "NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5",
    package = "NEONiso", mustWork = TRUE)

  result <- NEONiso:::h5_ls(fin)
  expect_true("ONAQ" %in% result$name)
})

test_that("h5_read_attrs on real NEON file returns site attributes", {
  fin <- system.file("extdata",
    "NEON.D15.ONAQ.DP4.00200.001.nsae.2019-05.basic.packed.h5",
    package = "NEONiso", mustWork = TRUE)

  attrs <- NEONiso:::h5_read_attrs(fin, "ONAQ")
  expect_type(attrs, "list")
  expect_true("LvlMeasTow" %in% names(attrs))
})
