library(SticsRFiles)
context("searching variables information")

# fixing version to latest standard one
stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()
stics_prev_version <- get_stics_versions_compat(-1)


# creating an empty df
empty_df <- data.frame(
  name = character(0),
  definition = character(0),
  unit = character(0),
  type = character(0),
  stringsAsFactors = FALSE
)

# getting all parameters
p <- get_examples_path(file_type = "csv")
lines_outputs <- readLines(file.path(p, "outputs.csv"))
df_outputs <- get_var_info()
test_that("getting all variables from outputs.csv", {
  testthat::expect_equal(length(lines_outputs), dim(df_outputs)[1])
})

# Testing empty result
test_that("giving a unknown variable name returns a 0 row data", {
  empty_df_var <- get_var_info("myunknownvariable")
  empty_df_keyword <- get_var_info(keyword = "myunknownvariable")

  testthat::expect_equal(nrow(empty_df), nrow(empty_df_var))
  testthat::expect_equal(length(empty_df), length(empty_df_var))
  testthat::expect_equal(nrow(empty_df), nrow(empty_df_keyword))
  testthat::expect_equal(length(empty_df), length(empty_df_keyword))
})

var_lai_df <- data.frame(
  name = c("albedolai", "exolai"),
  definition = c(
    "albedo of the crop including soil and vegetation",
    "reduction factor on leaf growth due to water excess"
  ),
  unit = c("SD", "0-1"),
  type = c("real", "real"),
  stringsAsFactors = FALSE
)

keyword_lai_df <- data.frame(
  name = c("albedolai", "diftemp1intercoupe"),
  definition = c(
    "albedo of the crop including soil and vegetation",
    paste("mean difference between crop and air temperatures during",
          "the vegetative phase (emergence - maximum LAI)")
  ),
  unit = c("SD", "degreeC"),
  type = c("real", "real"),
  stringsAsFactors = FALSE
)

# Testing result for searching a variable name using lai
test_that("giving an existing partial variable name in var arg or keyword", {
  var_df <- get_var_info(
    var = "lai",
    stics_version = stics_version
  )[1:2, ]
  keyword_df <- get_var_info(
    keyword = "lai",
    stics_version = stics_version
  )[1:2, ]

  common_var_lai_df <- dplyr::filter(var_df, var_df$name %in% var_lai_df$name)
  common_keyword_lai_df <- dplyr::filter(keyword_df, keyword_df$name %in% keyword_lai_df$name)

  testthat::expect_equivalent(var_df, var_lai_df)
  testthat::expect_equivalent(keyword_df, keyword_lai_df)

  testthat::expect_equivalent(var_df, common_var_lai_df)
  testthat::expect_equivalent(keyword_df, common_keyword_lai_df)


})

var_etmetr_df <- data.frame(
  name = "cep2",
  definition =
    "cumulative transpiration over the cropping season of plants 1 and 2",
  unit = "mm",
  type = "real",
  stringsAsFactors = FALSE
)

# Testing with different versions: last , previous
# Testing result for searching a variable name using etm_etr1moy
# or etm as keyword testing returned df dim
test_that("giving different versions", {
  existing_var_df <- get_var_info("cep2", stics_version = stics_version)
  missing_var_df <- get_var_info("cep2", stics_version = stics_prev_version)

  testthat::expect_equivalent(missing_var_df, var_etmetr_df)
  testthat::expect_equivalent(existing_var_df, var_etmetr_df)

  var_df_last <- get_var_info("etm", stics_version = stics_version)
  var_df_prev_last <- get_var_info("etm", stics_version = stics_prev_version)

  expect_equivalent(var_df_last, var_df_prev_last)
})
