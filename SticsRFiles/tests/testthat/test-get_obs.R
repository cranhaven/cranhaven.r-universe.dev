options(warn=-1)
stics_version <- get_stics_versions_compat()$latest_version


context("reading observations")

path <- file.path(
  get_examples_path(
    file_type = "obs",
    stics_version = stics_version
  ),
  "simple_example"
)

path_mixed <- file.path(get_examples_path(file_type = "obs",
                                          stics_version = stics_version),
                        "mixed")

# Get observations for all usms, but only banana has observations:
meas <- get_obs(workspace = path)
meas_mixed <- get_obs(path_mixed)

# Get observations only for banana:
meas_banana <- get_obs(path_mixed, "banana")

# Get more information using the usms.xml file:

meas_mixed2 <- get_obs(
  workspace = path_mixed,
  usms_file = file.path(path_mixed, "usms.xml")
)

# Using a usms.xml from outside the repo:
usms_path <- file.path(
  get_examples_path(
    file_type = "xml",
    stics_version = stics_version
  ),
  "usms.xml"
)
usms <- get_usms_list(file = usms_path)

meas_mixed3 <- get_obs(workspace = path_mixed, usms_file = usms_path)


test_that("filtering observation list works", {
  expect_equal(length(meas_banana), 1)
})

test_that("filtering observation list return a list of data.frame", {
  expect_true(is.list(meas_banana) & !is.data.frame(meas_banana))
  expect_true(is.data.frame(meas_banana$banana))
})

test_that("reading mixed usms works", {
  expect_length(meas_mixed, 3)
  expect_named(meas_mixed, c("sorghum", "banana", "IC_banana_sorghum"),
    ignore.order = TRUE
  )
  expect_equal(sum(meas_mixed$sorghum$lai_n, na.rm = TRUE), 19.62)
  expect_equal(sum(meas_mixed$banana$lai_n, na.rm = TRUE), 34.446)
  expect_equal(sum(meas_mixed$IC_banana_sorghum$lai_n, na.rm = TRUE), 54.066)
})

test_that("reading mixed usms with usms_filename to usms.xml", {
  expect_length(meas_mixed2, 3)
  expect_equal(sort(names(meas_mixed)), sort(names(meas_mixed2)))
  expect_equal(
    meas_mixed$IC_banana_sorghum$lai_n,
    meas_mixed2$IC_banana_sorghum$lai_n
  )
})

test_that(
  "reading mixed usms with usms_filename to usms.xml outside of folder",
  {
    expect_length(meas_mixed3, 2)
    # NB: only two because intecrop usms absent from this usms.xml
    expect_equal(meas_mixed$banana$lai_n, meas_mixed3$banana$lai_n)
  }
)

# Testing with .obs in different folders (e.g. one for each usm,
# but a common usms.xml)

test_that("reading mixed usms with usms_filename to usms.xml outside of folder,
          and usms in different folders", {
  path <- file.path(get_examples_path(file_type = "obs",
                                      stics_version = stics_version),
                    "usms_outside")
  paths <- list.dirs(path)[-1]
  meas_2 <- get_obs(workspace = paths, usms_file = file.path(path, "usms.xml"))

  expect_length(meas_mixed3, 2)
  # NB: only two because intecrop usms absent from this usms.xml
  expect_equal(meas_mixed$banana$lai_n, meas_mixed3$banana$lai_n)
})


# Testing empty obs:
test_that("reading empty usms returns a 0 row data", {
  path_empty <- file.path(get_examples_path(file_type = "obs",
                                            stics_version = stics_version),
                          "empty")
  meas <- get_obs(workspace = path_empty)
  expect_true(is.data.frame(meas$empty))
  expect_length(meas$empty, 0)
})


example_ic <- download_data(
  example_dirs = "study_case_intercrop",
  stics_version = stics_version
)

test_that("get obs with intercrops", {
  outputs <- get_obs(workspace = example_ic)
  # There are two USMs in the usms.xml file, but only one output file (banana):
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(all(names(outputs) %in%
    c(
      "IC_Wheat_Pea_2005-2006_N0", "SC_Pea_2005-2006_N0",
      "SC_Wheat_2005-2006_N0"
    )))
  expect_true(is.data.frame(outputs$`SC_Pea_2005-2006_N0`))
  expect_equal(
    unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Plant),
    c("plant_1", "plant_2")
  )
  expect_equal(
    unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Dominance),
    c("Principal", "Associated")
  )
  expect_equal(unique(outputs$`SC_Pea_2005-2006_N0`$Plant), "plant_1")
  expect_null(outputs$`SC_Pea_2005-2006_N0`$Dominance)
})

test_that("get obs with intercrops, giving usms.xml file", {
  outputs <- get_obs(
    workspace = example_ic,
    usms_file = file.path(example_ic, "usms.xml")
  )
  # There are two USMs in the usms.xml file, but only one output file (banana):
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(all(names(outputs) %in%
    c(
      "IC_Wheat_Pea_2005-2006_N0", "SC_Pea_2005-2006_N0",
      "SC_Wheat_2005-2006_N0"
    )))
  expect_true(is.data.frame(outputs$`SC_Pea_2005-2006_N0`))
  expect_equal(
    unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Plant),
    c("ble", "poi")
  )
  expect_equal(
    unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Dominance),
    c("Principal", "Associated")
  )
})

test_that("get obs with intercrops, giving usms.xml file as absolute path", {
  outputs <- get_obs(
    workspace = example_ic,
    usms_file = file.path(example_ic, "usms.xml")
  )
  # There are two USMs in the usms.xml file, but only one output file (banana):
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(all(names(outputs) %in%
    c(
      "IC_Wheat_Pea_2005-2006_N0", "SC_Pea_2005-2006_N0",
      "SC_Wheat_2005-2006_N0"
    )))
  expect_true(is.data.frame(outputs$`SC_Pea_2005-2006_N0`))
  expect_equal(
    unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Plant),
    c("ble", "poi")
  )
  expect_equal(
    unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Dominance),
    c("Principal", "Associated")
  )
})
