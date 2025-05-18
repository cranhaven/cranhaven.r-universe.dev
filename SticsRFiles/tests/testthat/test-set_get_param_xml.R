library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version

context("Getting and Setting values from xml files")

path <- get_examples_path("xml", stics_version = stics_version)

# Copy example to test in tempdir since the files will be modified by set_param
file.copy(
  from = file.path(path, list.files(path)), to = tempdir(),
  overwrite = TRUE
)

file <- file.path(tempdir(), "file_plt.xml")

tmp1 <- unlist(get_param_xml(file,
                             param = "stlevamf",
                             stics_version = stics_version
))
codevar <- unlist(get_param_xml(file,
                                param = "codevar",
                                stics_version = stics_version
))
tmp2 <- unlist(get_param_xml(file,
                             param = "stlevamf",
                             variety = codevar, stics_version = stics_version
))
variete <- unlist(get_param_xml(file,
                                param = "variete",
                                stics_version = stics_version# )
                                # path <- tempdir()
))
tmp3 <- unlist(get_param_xml(file,
                             param = "stlevamf",
                             variety = variete, stics_version = stics_version
))

test_that("variety argument can take NULL, integer or characters", {
  expect_equal(tmp1, tmp2)
  expect_equal(tmp2, tmp3)
})

varieties <- get_param_xml(file,
                           param = "variete",
                           stics_version = stics_version
)$file_plt.xml$variete[1:1]

tmp1 <- get_param_xml(file,
                      param = "stlevamf",
                      variety = varieties,
                      stics_version = stics_version
)$file_plt.xml$stlevamf
tmp2 <- get_param_xml(file,
                      stics_version = stics_version
)$file_plt.xml$stlevamf[1:1]
test_that("variety argument can be a vector of characters", {
  expect_equal(tmp1, tmp2)
})

# Get and modify the non-varietal parameter "forme"
# (another parameter has a similar name : rapforme ...)

tmp <- unlist(get_param_xml(file,
                            param = "forme", exact = TRUE,
                            stics_version = stics_version
))
set_param_xml(file,
              param = "forme",
              values = tmp + 1,
              overwrite = TRUE
)

tmp2 <- unlist(get_param_xml(file,
                             param = "forme", exact = TRUE,
                             stics_version = stics_version
))
test_that("Set and get of a non-varietal parameter for a unique plant", {
  expect_equal(length(tmp), 1)
  expect_equal(tmp + 1, tmp2)
})

# Get and modify the varietal parameter "stlevamf" for the simulated variety
tmp <- unlist(get_param_xml(file,
                            param = "stlevamf",
                            stics_version = stics_version
))
set_param_xml(file,
              param = "stlevamf",
              values = tmp + 1,
              overwrite = TRUE
)

tmp2 <- unlist(get_param_xml(file,
                             param = "stlevamf",
                             stics_version = stics_version
))
test_that("Set and get of a varietal parameter for a unique plant
          for the simulated variety", {
            expect_equal(tmp + 1, tmp2)
          })

# Get and modify the varietal parameter "stlevamf" for a given variety
tmp <- unlist(get_param_xml(file,
                            param = "stlevamf", variety = 1,
                            stics_version = stics_version
))
set_param_xml(file,
              param = "stlevamf",
              values = as.numeric(tmp) + 1,
              variety = 1,
              overwrite = TRUE
)

tmp2 <- unlist(get_param_xml(file,
                             workspace = path, param = "stlevamf", variety = 1,
                             stics_version = stics_version
))
test_that("Set and get of a varietal parameter for a unique plant
          for a given variety", {
            expect_equal(tmp + 1, tmp2)
          })


# Get and modify the non-varietal parameter "forme" for the simulated variety
tmp <- unlist(get_param_xml(file,
                            param = "forme", exact = TRUE,
                            stics_version = stics_version
))

set_param_xml(file,
              param = "forme",
              values = tmp + 1,
              overwrite = TRUE
)
tmp2 <- unlist(get_param_xml(file,
                             param = "forme", exact = TRUE,
                             stics_version = stics_version
))
test_that("Set and get of a non-varietal parameter for an intercrop
          for the simulated variety", {
            expect_equal(tmp + 1, tmp2)
          })

# Get and modify the varietal parameter "stlevamf" for the simulated variety
tmp <- unlist(get_param_xml(file,
                            param = "stlevamf",
                            stics_version = stics_version
))

set_param_xml(file,
              param = "stlevamf",
              values = tmp + 1,
              overwrite = TRUE
)

tmp2 <- unlist(get_param_xml(file,
                             param = "stlevamf",
                             stics_version = stics_version
))
test_that("Set and get of a varietal parameter for an intercrop
          for the simulated variety", {
            expect_equal(tmp + 1, tmp2)
          })



# Get parameters values using value_id (i.e. id of values to be
# retrieved from parameter values vector) for the soil parameter "HCCF"
# for a specific soil
file <- file.path(tempdir(), "sols.xml")
tmp <- get_param_xml(
  file = file,
  param = "HCCF",
  select = "sol",
  select_value= "solcanne",
  value_id = c(1,3,5)
)
tmp <- unlist(tmp)


set_param_xml(file,
              param = "HCCF",
              values = tmp + 1,
              select = "sol",
              select_value= "solcanne",
              value_id = c(1,3,5),
              overwrite = TRUE
)

tmp2 <- unlist(get_param_xml(file,
                             param = "HCCF",
                             select = "sol",
                             select_value= "solcanne",
                             value_id = c(1,3,5))
)

test_that("Set and get of a varietal parameter for an intercrop
          for the simulated variety", {
            expect_equal(tmp + 1, tmp2)
          })

