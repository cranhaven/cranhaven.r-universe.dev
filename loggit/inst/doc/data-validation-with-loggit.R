## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(loggit)

## ----iris_head----------------------------------------------------------------
head(iris)

## ----iris_booboo, echo = FALSE------------------------------------------------
iris_0 <- iris[iris$Sepal.Length > 4.5, ]
colnames(iris_0) <- gsub('\\.', '_', tolower(colnames(iris_0)))

## ----iris_agg-----------------------------------------------------------------
head(iris_0)
iris_agg <- aggregate(. ~ species, data = iris_0, mean)
iris_agg

## ----validate_funcs, eval = FALSE---------------------------------------------
#  some_function <- function(df_in) {
#    # Do your regular transformations, modeling, etc.
#    df_out <- aggregate(in_some_way, df_in)
#  
#    # Just before returning from the function, call the validator, which logs out
#    # the result
#    validate_some_function(df_out, df_in)
#  
#    # Then, return or exit as usual
#    df_out
#  }
#  
#  validate_some_function <- function(df_out, df_in) {
#    df_in_expected <- some_code_to_get_df_in_to_look_like_df_out
#    if (df_out$value != df_in_expected$value) {
#      loggit("ERROR", sprintf("Actual (%s) != Expected (%s)"), df_out$value, df_in_expected$value)
#    }
#  }

## ----validate_at_end, eval = FALSE--------------------------------------------
#  logdata <- read_logs()
#  logdata <- logdata[logdata$log_lvl == "ERROR", ]
#  if (nrow(logdata) > 0) {
#    logdata
#    stop("Data validation failures detected! Review above!")
#  }

## ----validate_iris------------------------------------------------------------
validate_aggregate_iris <- function(iris_out, iris_in) {
  actual_mean <- mean(iris_out$sepal_length)
  expected_mean <- mean(iris_in$Sepal.Length)
  if (actual_mean != expected_mean) {
    loggit("ERROR", sprintf("Means differ! (actual = %.4f, expected = %.4f", actual_mean, expected_mean))
  }
}

validate_aggregate_iris(iris_agg, iris)

