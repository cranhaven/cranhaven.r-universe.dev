DAYS_IN_YEAR <- 365.25

# Extracts the variable names from a formula
# expr (language): Expression in the form entry(diagdate), where
# 'entry' is the known function name while the name inside brackets
# is the user's column name for that variable
.extract_var_name <- function(expr) {
    as.character(expr)[[2]]
}

# Checks that all the factors to stratify population survival by are in the registry data set.
validate_population_survival <- function(population_data, registry_data=NULL, population_covariates=NULL) {

    if (!'surv' %in% colnames(population_data))
        stop("Error: no 'surv' column found in population survival rates 'daily_survival'.")

    if (min(population_data$surv) < 0 | max(population_data$surv) > 1)
        stop("Error: population survival probabilities found in 'surv' are out of the range [0,1].")

    if (!'age' %in% colnames(population_data))
        stop("Error: no 'age' column found in population survival rates 'daily_survival'.")

    # Remove age from population covariates as must be there anyway
    population_covariates <- setdiff(population_covariates, 'age')

    pop_covariates_in_pop <- sapply(population_covariates, function(x) x %in% colnames(population_data))

    if (!all(pop_covariates_in_pop))
        stop(paste0("Error: not all values in population_covariates are in 'daily_survival'. Missing ",
                   paste(population_covariates[!pop_covariates_in_pop], collapse=","), "."))


    # Registry data doesn't need to be provided, for example if fitting cure model that just reverts to
    # population survival
    if (!is.null(registry_data)) {
        # Always need age
        if (!'age' %in% colnames(registry_data))
            stop("Error: no 'age' column found in registry data frame 'data'.")

        # Check have all population covariates
        pop_covariates_in_reg <- sapply(population_covariates, function(x) x %in% colnames(registry_data))
        if (!all(pop_covariates_in_reg))
            stop(paste0("Error: not all values in population_covariates are in 'data'. Missing ",
                       paste(population_covariates[!pop_covariates_in_reg], collapse=","), "."))

        # Check have all levels of population factors in registry data
        missing_covar_factors <- sapply(population_covariates, function(covar) {
            all(unique(registry_data[[covar]]) %in% population_data[[covar]])
        })

        if (!all(missing_covar_factors))
            stop(paste("Error: not all values of",
                       paste(population_covariates[!missing_covar_factors], collapse=','),
                       "are present in population survival."))
    }


}

# Calculates labels to use for the denominator of rates
proportion_label <- function(prop) {
    unit <- if (prop / 1e6 >= 1) 'M' else {
                              if (prop / 1e3 >= 1) 'K' else ''
                          }
    val <- if (prop / 1e6 >= 1) prop / 1e6 else {
                              if (prop / 1e3 >= 1) prop / 1e3 else prop
    }
    paste0(val, unit)
}