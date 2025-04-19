## ----setup, include=FALSE, message=FALSE, warning=FALSE-----------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.retina = 3,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(cbcTools)

profiles <- cbc_profiles(
  price     = seq(1, 5, 0.5), # $ per pound
  type      = c('Fuji', 'Gala', 'Honeycrisp'),
  freshness = c('Poor', 'Average', 'Excellent')
)

nrow(profiles)
head(profiles)
tail(profiles)

## -----------------------------------------------------------------------------
restricted_profiles <- cbc_restrict(
    profiles,
    type == "Gala" & price %in% c(1.5, 2.5, 3.5),
    type == "Honeycrisp" & price > 2,
    type == "Honeycrisp" & freshness %in% c("Poor", "Excellent"),
    type == "Fuji" & freshness == "Excellent"
)

restricted_profiles

## -----------------------------------------------------------------------------
set.seed(5678)

design_random <- cbc_design(
  profiles = profiles,
  n_resp   = 900, # Number of respondents
  n_alts   = 3,   # Number of alternatives per question
  n_q      = 6,   # Number of questions per respondent
  method   = 'random' # This is the default method
)

dim(design_random)  
head(design_random) 

## -----------------------------------------------------------------------------
design_full <- cbc_design(
  profiles = profiles,
  n_resp   = 900, # Number of respondents
  n_alts   = 3,   # Number of alternatives per question
  n_q      = 6,   # Number of questions per respondent
  method   = 'full'
)

dim(design_full) 
head(design_full)

## -----------------------------------------------------------------------------
design_orthogonal <- cbc_design(
  profiles = profiles,
  n_resp   = 900, # Number of respondents
  n_alts   = 3,   # Number of alternatives per question
  n_q      = 6,   # Number of questions per respondent
  method   = 'orthogonal'
)

dim(design_orthogonal) 
head(design_orthogonal)

## -----------------------------------------------------------------------------
design_dopt <- cbc_design(
  profiles = profiles,
  n_resp   = 900, # Number of respondents
  n_alts   = 3,   # Number of alternatives per question
  n_q      = 6,   # Number of questions per respondent
  method   = 'dopt'
)

dim(design_dopt)
head(design_dopt)

## -----------------------------------------------------------------------------
design_bayesian <- cbc_design(
  profiles  = profiles,
  n_resp    = 900, # Number of respondents
  n_alts    = 3, # Number of alternatives per question
  n_q       = 6, # Number of questions per respondent
  priors = list(
    price     = -0.1,
    type      = c(0.1, 0.2),
    freshness = c(0.1, 0.2)
  ),
  method = 'CEA'
)

dim(design_bayesian)
head(design_bayesian)

## -----------------------------------------------------------------------------
design_random_labeled <- cbc_design(
  profiles = profiles,
  n_resp   = 900, # Number of respondents
  n_alts   = 3,   # Number of alternatives per question
  n_q      = 6,   # Number of questions per respondent
  label    = "type" # Set the "type" attribute as the label
)

dim(design_random_labeled)
head(design_random_labeled)

## -----------------------------------------------------------------------------
design_nochoice <- cbc_design(
  profiles  = profiles,
  n_resp    = 900, # Number of respondents
  n_alts    = 3, # Number of alternatives per question
  n_q       = 6, # Number of questions per respondent
  no_choice = TRUE
)

dim(design_nochoice)
head(design_nochoice)

## -----------------------------------------------------------------------------
design_bayesian_no_choice <- cbc_design(
  profiles  = profiles,
  n_resp    = 900, # Number of respondents
  n_alts    = 3, # Number of alternatives per question
  n_q       = 6, # Number of questions per respondent
  no_choice = TRUE,
  priors = list(
    price     = -0.1,
    type      = c(0.1, 0.2),
    freshness = c(0.1, 0.2)
  ),
  prior_no_choice = -0.1,
  method = 'CEA'
)

dim(design_bayesian_no_choice)
head(design_bayesian_no_choice)

## -----------------------------------------------------------------------------
design <- cbc_design(
  profiles = profiles,
  n_resp   = 900, 
  n_alts   = 3,   
  n_q      = 6
)

cbc_balance(design)

## -----------------------------------------------------------------------------
cbc_overlap(design)

## -----------------------------------------------------------------------------
data <- cbc_choices(
  design = design,
  obsID  = "obsID"
)

head(data)

## -----------------------------------------------------------------------------
#  data <- cbc_choices(
#    design = design,
#    obsID = "obsID",
#    priors = list(
#      price     = -0.1,
#      type      = c(0.1, 0.2),
#      freshness = c(0.1, 0.2)
#    )
#  )

## -----------------------------------------------------------------------------
#  data <- cbc_choices(
#    design = design,
#    obsID = "obsID",
#    priors = list(
#      price = -0.1,
#      type = c(0.1, 0.2),
#      freshness = c(0.1, 0.2),
#      `price*type` = c(0.1, 0.5)
#    )
#  )

## -----------------------------------------------------------------------------
#  data <- cbc_choices(
#    design = design,
#    obsID = "obsID",
#    priors = list(
#      price = -0.1,
#      type = randN(mean = c(0.1, 0.2), sd = c(1, 2)),
#      freshness = c(0.1, 0.2)
#    )
#  )

## -----------------------------------------------------------------------------
power <- cbc_power(
  data    = data,
  pars    = c("price", "type", "freshness"),
  outcome = "choice",
  obsID   = "obsID",
  nbreaks = 10,
  n_q     = 6
)

head(power)
tail(power)

## ----power--------------------------------------------------------------------
plot(power)

## -----------------------------------------------------------------------------
models <- cbc_power(
  data    = data,
  pars    = c("price", "type", "freshness"),
  outcome = "choice",
  obsID   = "obsID",
  nbreaks = 10,
  n_q     = 6,
  return_models = TRUE
)

summary(models[[10]])

## -----------------------------------------------------------------------------
#  cbc_profiles(
#    price     = seq(1, 4, 0.5), # $ per pound
#    type      = c('Fuji', 'Gala', 'Honeycrisp'),
#    freshness = c('Poor', 'Average', 'Excellent')
#  ) |>
#  cbc_design(
#    n_resp   = 900, # Number of respondents
#    n_alts   = 3,   # Number of alternatives per question
#    n_q      = 6    # Number of questions per respondent
#  ) |>
#  cbc_choices(
#    obsID = "obsID",
#    priors = list(
#      price     = -0.1,
#      type      = c(0.1, 0.2),
#      freshness = c(0.1, 0.2)
#    )
#  ) |>
#  cbc_power(
#      pars    = c("price", "type", "freshness"),
#      outcome = "choice",
#      obsID   = "obsID",
#      nbreaks = 10,
#      n_q     = 6
#  ) |>
#  plot()

## ---- ref.label='power', echo=FALSE-------------------------------------------
plot(power)

## -----------------------------------------------------------------------------
# Make designs to compare: full factorial vs bayesian d-efficient
design_full <- cbc_design(
  profiles = profiles,
  n_resp = 300, n_alts = 3, n_q = 6
)
# Same priors will be used in bayesian design and simulated choices 
priors <- list( 
  price     = -0.1,
  type      = c(0.1, 0.2),
  freshness = c(0.1, 0.2)
)
design_bayesian <- cbc_design(
  profiles  = profiles,
  n_resp = 300, n_alts = 3, n_q = 6, n_start = 1, method = "CEA",
  priors = priors, parallel = FALSE
)

# Obtain power for each design by simulating choices
power_full <- design_full |>
cbc_choices(obsID = "obsID", priors = priors) |>
  cbc_power(
    pars = c("price", "type", "freshness"),
    outcome = "choice", obsID = "obsID", nbreaks = 10, n_q = 6, n_cores = 2
  )
power_bayesian <- design_bayesian |>
  cbc_choices(obsID = "obsID", priors = priors) |>
  cbc_power(
    pars = c("price", "type", "freshness"),
    outcome = "choice", obsID = "obsID", nbreaks = 10, n_q = 6, n_cores = 2
  )

# Compare power of each design
plot_compare_power(power_bayesian, power_full)

