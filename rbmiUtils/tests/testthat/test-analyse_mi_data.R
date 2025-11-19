library(dplyr)
library(rbmi)

set.seed(123)

# Sample data for testing
data("ADMI")

ADMI$IMPID <- rep(1:5, each = nrow(ADMI) / 5) # Mock IMPID for imputed datasets

vars <- rbmi::set_vars(
  subjid = "USUBJID",
  visit = "AVISIT",
  group = "TRT",
  outcome = "CHG",
  covariates = c("BASE", "STRATA", "REGION")
)

method <- rbmi::method_bayes(
  n_samples = 5,
  control = rbmi::control_bayes(
    warmup = 10,
    thin = 2
  )
)


dummy_analysis_fun <- function(data, vars, ...) {
  # A simple dummy function to simulate analysis
  return(mean(data[[vars$outcome]], na.rm = TRUE))
}

# Set of Unit Tests

test_that("Error when data is NULL", {
  expect_error(
    analyse_mi_data(data = NULL, vars = vars),
    "`data` cannot be NULL."
  )
})

test_that("Error when IMPID is missing from data", {
  ADMI_no_impid <- ADMI %>% select(-IMPID)
  expect_error(
    analyse_mi_data(data = ADMI_no_impid, vars = vars),
    "`data` must contain a variable `IMPID` to identify distinct imputation iterations."
  )
})

test_that("Error when vars is NULL", {
  expect_error(
    analyse_mi_data(data = ADMI, vars = NULL),
    "`vars` cannot be NULL. Specify key variables."
  )
})

test_that("Error when fun is not a function", {
  expect_error(
    analyse_mi_data(data = ADMI, vars = vars, fun = "not_a_function"),
    "`fun` must be a function"
  )
})

test_that("Error when delta is not NULL or a data.frame", {
  expect_error(
    analyse_mi_data(data = ADMI, vars = vars, delta = list()),
    "`delta` must be NULL or a data.frame"
  )
})

test_that("Error when delta does not contain required variables", {
  delta_invalid <- data.frame(subjid = ADMI$USUBJID, delta = rnorm(nrow(ADMI)))
  expect_error(
    analyse_mi_data(data = ADMI, vars = vars, delta = delta_invalid),
    "The following variables must exist within `delta`: `USUBJID`, `AVISIT`, `delta`"
  )
})


test_that("Proper class assignment for analysis object", {
  result <- analyse_mi_data(
    data = ADMI,
    vars = vars,
    method = method,
    fun = dummy_analysis_fun
  )
  expect_s3_class(result, "analysis")
  expect_true("rubin" %in% class(result$results))
})

test_that("analyse_mi_data throws error when fun is not a function", {
  data <- data.frame(
    IMPID = rep(1:3, each = 10),
    var1 = rnorm(30),
    var2 = rnorm(30)
  )
  vars <- data.frame(vars = "vars")

  # Expect an error when 'fun' is not a function
  expect_error(
    analyse_mi_data(data = data, vars = vars, fun = "not_a_function"),
    "`fun` must be a function"
  )
})
