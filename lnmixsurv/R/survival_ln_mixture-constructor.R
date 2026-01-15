new_survival_ln_mixture <- function(posterior, nobs, predictors_name, mixture_groups, blueprint, data) {
  hardhat::new_model(
    posterior = posterior,
    nobs = nobs,
    predictors_name = predictors_name,
    mixture_groups = mixture_groups,
    blueprint = blueprint,
    class = "survival_ln_mixture"
  )
}