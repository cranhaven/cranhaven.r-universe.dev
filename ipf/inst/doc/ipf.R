## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = '#>'
)


## ----setup--------------------------------------------------------------------
library(ipf)
library(tibble)
data(anes24)

## ----data-overview------------------------------------------------------------
anes24

## ----levels-and-missing-------------------------------------------------------
table(anes24$sex, useNA = 'ifany')
table(anes24$race, useNA = 'ifany')
table(anes24$income, useNA = 'ifany')

## ----targets------------------------------------------------------------------
targets <- list(
  sex = c(Male = 0.472, Female = 0.528),
  race = c(
    White = 0.706,
    Black = 0.121,
    Hispanic = 0.107,
    Asian = 0.047,
    Other = 0.019
  ),
  income = c(
    'Under $50k' = 0.151,
    '$50k-$100k' = 0.294,
    'Over $100k' = 0.555
  )
)

## ----rake-basic---------------------------------------------------------------
result <- rake(anes24, targets, cap = NULL)
result

## ----bucket-------------------------------------------------------------------
bucketed <- rake(anes24, targets, cap = NULL, na_method = 'bucket')
bucketed

## ----base-weights-------------------------------------------------------------
base_w <- ifelse(anes24$sex == 'Female', 1.1, 0.9)
base_w[is.na(base_w)] <- 1

base_weighted <- rake(anes24, targets, base_weights = base_w, cap = NULL)
base_weighted

## ----deff---------------------------------------------------------------------
design_effect(result$weights)

## ----summary------------------------------------------------------------------
summary(result)

## ----tidy---------------------------------------------------------------------
# One row per variable-level
tidy(result)

# One-row summary
glance(result)

## ----augment------------------------------------------------------------------
weighted_data <- augment(result)
head(weighted_data)

## ----presidential-comparison--------------------------------------------------
presidential_data <- subset(weighted_data, !is.na(presidential))

presidential_unweighted <- prop.table(table(presidential_data$presidential))

presidential_weighted <- aggregate(
  .weight ~ presidential,
  presidential_data,
  sum
)
presidential_weighted$weighted_pct <- presidential_weighted$.weight /
  sum(presidential_weighted$.weight)

presidential_compare <- tibble::tibble(
  presidential = presidential_weighted$presidential,
  unweighted_pct = as.numeric(presidential_unweighted[
    presidential_weighted$presidential
  ]),
  weighted_pct = presidential_weighted$weighted_pct
)

presidential_compare

## ----cap----------------------------------------------------------------------
# Unbounded fit from above
range(result$weights)
design_effect(result$weights)

# Default cap
default_bounded <- rake(anes24, targets)
range(default_bounded$weights)
design_effect(default_bounded$weights)

# Tighter cap
tight <- rake(anes24, targets, cap = 3)
range(tight$weights)
design_effect(tight$weights)

# Or specify both min and max bounds
bounded <- rake(anes24, targets, bounds = c(0.3, 3))
range(bounded$weights)

## ----var-selection------------------------------------------------------------
targets_many <- list(
  sex = c(Male = 0.472, Female = 0.528),
  race = c(
    White = 0.706,
    Black = 0.121,
    Hispanic = 0.107,
    Asian = 0.047,
    Other = 0.019
  ),
  income = c('Under $50k' = 0.151, '$50k-$100k' = 0.294, 'Over $100k' = 0.555),
  married = c(
    Married = 0.58,
    Widowed = 0.06,
    Divorced = 0.10,
    Separated = 0.01,
    'Never married' = 0.25
  )
)

# Only rake on variables where discrepancy exceeds 5%
result_pct <- rake(anes24, targets_many, type = 'pctlim', pctlim = 0.05)
result_pct$vars_used

## ----discrepancy--------------------------------------------------------------
find_discrepant_vars(
  anes24,
  targets_many,
  weights = rep(1, nrow(anes24)),
  choosemethod = 'total'
)

