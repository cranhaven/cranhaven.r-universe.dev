## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rmdl)

## -----------------------------------------------------------------------------
# Look at potential data from the `mtcars` dataset
head(mtcars)

baseFormula <- mpg ~ wt + hp
rFormula <- fmls(mpg ~ wt + hp)

# Similar to the base formula
rFormula

## -----------------------------------------------------------------------------
# Uses a custom fit function to return linear models
listModels <-
  rFormula |>
  fit(.fn = lm, data = mtcars, raw = TRUE)

## -----------------------------------------------------------------------------
# Uses a custom fit function 
rModel <-
  rFormula |>
  fit(.fn = lm, data = mtcars, raw = FALSE)

rModel

## -----------------------------------------------------------------------------
# An additional model to work with
r2Model <-
  fmls(am ~ cyl + hp, pattern = "sequential") |>
  fit(.fn = glm, family = "binomial", data = mtcars, raw = FALSE)

# Displays the two additional logistic regressions performed
r2Model

# Creation of a table of models
rTable <- model_table(mileage = rModel, automatic = r2Model)
rTable

## -----------------------------------------------------------------------------
fTable <-
  rTable |>
  flatten_models(exponentiate = TRUE, which = "automatic") 

# Display contents
fTable

# Filter down to relevant models
fTable |>
  dplyr::select(name, number, outcome, term, estimate, conf_low, conf_high, p_value, nobs)

