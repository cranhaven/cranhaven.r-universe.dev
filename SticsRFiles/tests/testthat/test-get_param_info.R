context("searching parameter information (get_param_info)")


stics_version <- get_stics_versions_compat()$latest_version
# creating an empty df
# `name`,`file`,`min`,`max`, `definition`
empty_df <- data.frame(
  name = character(0),
  file = character(0),
  min = character(0),
  max = character(0),
  definition = character(0),
  stringsAsFactors = FALSE
)


# getting all parameters
p <- get_examples_path(file_type = "csv")
lines_inputs <- readLines(file.path(p, "inputs.csv"))
df_inputs <- get_param_info()
test_that("getting all parameters from inputs.csv", {
  testthat::expect_equal(length(lines_inputs), dim(df_inputs)[1])
})

# Testing empty result
test_that("giving a unknown variable name returns a 0 row data", {
  empty_df_var <- get_param_info("myunknownvariable",
                                 stics_version = stics_version
  )
  empty_df_keyword <- get_param_info(
    keyword = "myunknownkeyword",
    stics_version = stics_version
  )

  testthat::expect_equal(nrow(empty_df), nrow(empty_df_var))
  testthat::expect_equal(length(empty_df), length(empty_df_var))
  testthat::expect_equal(nrow(empty_df), nrow(empty_df_keyword))
  testthat::expect_equal(length(empty_df), length(empty_df_keyword))
})

test_that("fuzzy name", {
  if (get_version_num() < 10) {
    lai_params <-
      c(
        "lai0", "codelaitr", "codlainet", "dlaimax", "dlaimaxbrut", "dlaimin",
        "laicomp", "laiplantule", "pentlaimax", "splaimax", "splaimin",
        "udlaimax", "vlaimax", "cielclair", "codeclaircie", "juleclair",
        "laidebeff", "laieffeuil", "lairesiduel", "flai"
      )
  } else {
    lai_params <-
      c(
        "cielclair", "codeclaircie", "codelaitr", "codlainet", "dlaimax",
        "dlaimaxbrut", "dlaimin", "flai", "inilai", "juleclair", "juleclair(1)",
        "juleclair(10)", "juleclair(2)", "juleclair(3)", "juleclair(4)",
        "juleclair(5)", "juleclair(6)", "juleclair(7)", "juleclair(8)",
        "juleclair(9)", "lai", "lai0", "laicomp", "laidebeff", "laieffeuil",
        "laiplantule", "lairesiduel", "lairesiduel(1)", "lairesiduel(10)",
        "lairesiduel(11)", "lairesiduel(12)", "lairesiduel(13)",
        "lairesiduel(14)", "lairesiduel(15)", "lairesiduel(16)",
        "lairesiduel(17)", "lairesiduel(18)", "lairesiduel(19)",
        "lairesiduel(2)", "lairesiduel(20)", "lairesiduel(3)", "lairesiduel(4)",
        "lairesiduel(5)", "lairesiduel(6)", "lairesiduel(7)", "lairesiduel(8)",
        "lairesiduel(9)", "pentlaimax", "seuilLAIapex", "splaimax", "splaimin",
        "udlaimax", "vlaimax"
      )
  }
  testthat::expect_equal(get_param_info("lai",
                                        stics_version = stics_version
  )$name, lai_params)
})

test_that("regex name", {
  testthat::expect_equal(
    get_param_info("^lai")$name,
    c(
      "lai", "lai0", "laicomp", "laidebeff", "laieffeuil", "laiplantule",
      "lairesiduel", "lairesiduel(1)", "lairesiduel(10)", "lairesiduel(11)",
      "lairesiduel(12)", "lairesiduel(13)", "lairesiduel(14)",
      "lairesiduel(15)", "lairesiduel(16)", "lairesiduel(17)",
      "lairesiduel(18)", "lairesiduel(19)", "lairesiduel(2)", "lairesiduel(20)",
      "lairesiduel(3)", "lairesiduel(4)", "lairesiduel(5)", "lairesiduel(6)",
      "lairesiduel(7)", "lairesiduel(8)", "lairesiduel(9)"
    )
  )
})


test_that("different STICS versions", {
  v_90 <- get_param_info("vlaimax", stics_version = "V9.0")
  v_92 <- get_param_info("vlaimax", stics_version = "V9.2")
  vlast <- get_param_info("vlaimax", stics_version = stics_version)

  testthat::expect_equal(attr(v_90, "version"), "V9.0")
  testthat::expect_equal(attr(v_92, "version"), "V9.2")
  testthat::expect_equal(attr(vlast, "version"), stics_version)

  # Set both versions to the same value for comparison:
  attr(v_90, "version") <- attr(v_92, "version")
  testthat::expect_equivalent(v_90, v_92)

  # breaking, parameter moved to cultivar parameters since V9.2
  testthat::expect_false(identical(vlast, v_92))
})
