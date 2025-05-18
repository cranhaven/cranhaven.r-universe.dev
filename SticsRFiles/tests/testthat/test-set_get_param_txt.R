library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version

context("Getting and Setting values from txt files")

path <- get_examples_path("txt", stics_version = stics_version)
# Copy example to test in tempdir since the files will be modified by set_param
file.copy(
  from = file.path(path, list.files(path)), to = tempdir(),
  overwrite = TRUE
)
path <- tempdir()

tmp1 <- unlist(get_param_txt(
  workspace = path, param = "stlevamf",
  stics_version = stics_version
))
codevar <- unlist(get_param_txt(
  workspace = path, param = "codevar",
  stics_version = stics_version
))
tmp2 <- unlist(get_param_txt(
  workspace = path, param = "stlevamf",
  variety = codevar, stics_version = stics_version
))
variete <- unlist(get_param_txt(
  workspace = path, param = "variete",
  stics_version = stics_version
))
tmp3 <- unlist(get_param_txt(
  workspace = path, param = "stlevamf",
  variety = variete, stics_version = stics_version
))

test_that("variety argument can take NULL, integer or characters", {
  expect_equal(tmp1, tmp2)
  expect_equal(tmp2, tmp3)
})

varieties <- get_param_txt(
  workspace = path,
  stics_version = stics_version
)$plant$plant1$codevar[1:3]

tmp1 <- get_param_txt(
  workspace = path, param = "stlevamf",
  variety = varieties,
  stics_version = stics_version
)$plant$plant1$stlevamf
tmp2 <- get_param_txt(
  workspace = path,
  stics_version = stics_version
)$plant$plant1$stlevamf[1:3]
test_that("variety argument can be a vector of characters", {
  expect_equal(tmp1, tmp2)
})


# Using a specific plant id
tmp <- get_param_txt(
  workspace = path,
  param = "stlevamf",
  stics_version = stics_version
)$plant$plant1$stlevamf

# existing id
tmp2 <- get_param_txt(
  workspace = path,
  param = "stlevamf",
  plant_id = 1,
  stics_version = stics_version
)$plant$plant1$stlevamf

test_that("get for an existing plant id or not", {
  expect_equal(tmp, tmp2)
  expect_error(
    get_param_txt(
      workspace = path,
      param = "stlevamf",
      plant_id = 2,
      stics_version = stics_version
    )$plant$plant1$stlevamf
  )
})

# Using specific value_id, for existing id or not
# soil layer: soil, ini parameters
set_param_txt(workspace = path,
              param = "cailloux",
              value = 2)

tmp <- get_param_txt(
  workspace = path,
  param = "cailloux",
  stics_version = stics_version
)$soil$cailloux

set_param_txt(workspace = path,
              param = "cailloux",
              value = c(1, 3, 5),
              value_id = c(1, 3, 5))

tmp2 <- get_param_txt(
  workspace = path,
  param = "cailloux",
  stics_version = stics_version
)$soil$cailloux[c(1,3,5)]

tmp3 <- get_param_txt(
  workspace = path,
  param = "cailloux",
  stics_version = stics_version,
  value_id = c(1,3,5),
  exact = TRUE
)$soil$cailloux

test_that("get for layer id", {
  expect_equal(tmp, rep(2,5))
  expect_equal(tmp2, tmp3)
  expect_error(
    get_param_txt(
      workspace = path,
      param = "cailloux",
      stics_version = stics_version,
      value_id = c(1,3,5),
      exact = FALSE
    )$plant$plant1$stlevamf
  )
})

# technical operations (irrigation)
set_param_txt(workspace = path,
              param = "amount",
              value = 40)

tmp <- get_param_txt(
  workspace = path,
  param = "amount",
  stics_version = stics_version
)$tec$plant1$amount

set_param_txt(workspace = path,
              param = "amount",
              value = c(50, 60, 70),
              value_id = c(1, 9, 16))

tmp2 <- get_param_txt(
  workspace = path,
  param = "amount",
  stics_version = stics_version
)$tec$plant1$amount[c(1, 9, 16)]


tmp3 <- get_param_txt(
  workspace = path,
  param = "amount",
  stics_version = stics_version,
  value_id = c(1, 9, 16)
)$tec$plant1$amount


test_that("get for layer id", {
  expect_equal(tmp, rep(40,16))
  expect_equal(tmp2, tmp3)
  expect_error(
    get_param_txt(
      workspace = path,
      param = "amount",
      stics_version = stics_version,
      value_id = c(1, 9, 20)
    )$ptec$plant1$amount
  )
})

# Get and modify the non-varietal parameter "forme"
# (another parameter has a similar name : rapforme ...)

tmp <- unlist(get_param_txt(
  workspace = path, param = "forme", exact = TRUE,
  stics_version = stics_version
))
set_param_txt(
  workspace = path, param = "forme",
  value = tmp + 1,
  stics_version = stics_version
)

tmp2 <- unlist(get_param_txt(
  workspace = path, param = "forme", exact = TRUE,
  stics_version = stics_version
))
test_that("Set and get of a non-varietal parameter for a unique plant", {
  expect_equal(length(tmp), 1)
  expect_equal(tmp + 1, tmp2)
})

# Get and modify the varietal parameter "stlevamf" for the simulated variety
tmp <- unlist(get_param_txt(
  workspace = path, param = "stlevamf",
  stics_version = stics_version
))
set_param_txt(
  workspace = path, param = "stlevamf",
  value = tmp + 1,
  stics_version = stics_version
)

tmp2 <- unlist(get_param_txt(
  workspace = path, param = "stlevamf",
  stics_version = stics_version
))

test_that("Set and get of a varietal parameter for a unique plant
          for the simulated variety", {
  expect_equal(tmp + 1, tmp2)
})

# Get and modify the varietal parameter "stlevamf" for a given variety
tmp <- unlist(get_param_txt(
  workspace = path, param = "stlevamf", variety = 4,
  stics_version = stics_version
))
set_param_txt(
  workspace = path,
  param = "stlevamf",
  value = as.numeric(tmp) + 1,
  variety = 4,
  stics_version = stics_version
)

tmp2 <- unlist(get_param_txt(
  workspace = path, param = "stlevamf", variety = 4,
  stics_version = stics_version
))
test_that("Set and get of a varietal parameter for a unique plant
          for a given variety", {
  expect_equal(tmp + 1, tmp2)
})


# Now let's work on intercrops ...
path <- file.path(
  get_examples_path("txt", stics_version = stics_version),
  "intercrop_pea_barley"
)
# Copy example to test in tempdir since the files will be modified by set_param
file.copy(
  from = file.path(path, list.files(path)), to = tempdir(),
  overwrite = TRUE
)
path <- tempdir()

# Get and modify the non-varietal parameter "forme" for the simulated variety
tmp <- unlist(get_param_txt(
  workspace = path, param = "forme", exact = TRUE,
  stics_version = stics_version
))
plant <- 2
set_param_txt(
  workspace = path,
  param = "forme",
  value = tmp[plant] + 1,
  plant_id = plant,
  stics_version = stics_version
)
tmp2 <- unlist(get_param_txt(
  workspace = path, param = "forme", exact = TRUE,
  stics_version = stics_version
))
test_that("Set and get of a non-varietal parameter for an intercrop
          for the simulated variety", {
  expect_equal(tmp[[plant]] + 1, tmp2[[plant]])
})

# Get and modify the varietal parameter "stlevamf" for the simulated variety
tmp <- unlist(get_param_txt(
  workspace = path, param = "stlevamf",
  stics_version = stics_version
))
plant <- 2
set_param_txt(
  workspace = path, param = "stlevamf",
  value = tmp[[plant]] + 1,
  plant_id = 2,
  stics_version = stics_version
)
tmp2 <- unlist(get_param_txt(
  workspace = path, param = "stlevamf",
  stics_version = stics_version
))
test_that("Set and get of a varietal parameter for an intercrop
          for the simulated variety", {
  expect_equal(tmp[[plant]] + 1, tmp2[[plant]])
})




# Getting parameters using a version number
# that does not match the file content
path <- get_examples_path("txt", stics_version = stics_version)
test_that("get for NO3init, for a wrong version", {
  expect_error(
    get_param_txt(
      workspace = path,
      param = "NO3init",
      stics_version = "V8.5")
  )
})
path <- get_examples_path("txt", stics_version = "V8.5")
test_that("get for NO3init, for a wrong version", {
  expect_error(
    get_param_txt(
      workspace = path,
      param = "NO3init")
  )
})

path <- get_examples_path("txt", stics_version = "V9.2")
test_that("get for NO3init, for a wrong version", {
  expect_error(
    get_param_txt(
      workspace = path,
      param = "NO3init")
  )
})
