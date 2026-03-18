#' Builds survival models for diseases with cured fractions using population mortality tables
#'
#' Fits a cure model which assumes that if an individual has survived
#'  beyond a set time-point then they are considered cured and their mortality reverts
#'  to population levels.
#'  Please read the detailed description below for how to use this model.
#'
#' To model population survival, population mortality tables are required, as specified by the \code{daily_survival}
#' argument. If not provided, then the default population mortality is that of the UK population, which goes up
#' to 100 years of age. If a simulated individual has expected lifespan longer than the maximum age in the mortality table
#' then they are estimated to have died at this age limit,
#' which is why it is advantageous to provide as many accurate survival probabilities as possible.
#'
#' Due to the linking with the registry data and the ability for user-specified mortality tables, there are stricter
#' requirements on the survival models used in cure models than elsewhere. For example, the time-scale of the
#' survival model specified in \code{formula} \strong{must be in days} so that it matches up with the mortality tables.
#' Likewise, \strong{age in years must be included as a covariate} in the survival model
#'
#' @inheritParams prevalence
#' @param daily_survival A data frame comprising population survival as a daily probability for as long as possible,
#'   ideally 100 years (36525 days).
#'   Defaults to using UK population survival from the \code{UKmortality} data set.
#'   It \strong{must contain columns 'age' and 'surv'}, providing the age (in days) and survival
#'   probability at that age respectively.
#'   It can also be stratified by other variables that are found in the survival \code{formula} for this model,
#'   such as sex.
#' @param population_covariates A character vector containing fields to stratify population survival by in addition to
#'   age, as descripted in \code{Details} below. These \strong{must} be the names of columns in both \code{data} and
#'   \code{daily_survival}. If not provided then defaults to the fields that are present in both \code{data} and
#'   \code{daily_survival}.
#' @param cure_time Time-limit at which a patient is considered cured. Note that if this is 0 or negative then
#'   survival will be based purely off the population rates (anything passed into \code{formula} and \code{data} will
#'   be ignored).
#' @param formula Formula specifying survival function, as used in
#'   \code{\link{prevalence}} with the \code{surv_formula} argument.
#'   \strong{Must be in days}.
#'
#' @return An object of class \code{fixedcure} that can be passed
#'   into \code{\link{prevalence}}.
#' @export
fixed_cure <- function(formula=NULL, data=NULL, cure_time=10*365.25, daily_survival=NULL, population_covariates=NULL,
                       dist=c('exponential', 'weibull', 'lognormal')) {
    # R CMD CHECK
    sex <- NULL

    dist <- match.arg(dist)

    func_call <- match.call()
    func_call$cure_time <- eval(cure_time)
    func_call$population_covariates <- population_covariates

    if (cure_time > 0) {
        if (missing(formula) | missing(data)) {
            stop("Error: must provide a formula and data.")
        }
        obj <- build_survreg(formula, data, dist)
        func_call$formula <- eval(formula)
        func_call$dist <- eval(dist)
    } else {
        obj <- list()
    }

    miss_pop_data <- is.null(daily_survival)

    if (miss_pop_data) {
        utils::data('UKmortality', envir=environment())
        daily_survival <- get('UKmortality', envir=environment())
    }

    if (!'data.table' %in% class(daily_survival)) {
        daily_survival <- data.table::as.data.table(daily_survival)
    }

    # If don't provide population covariates then set to the ones in both data sets
    if (is.null(population_covariates)) {
        population_covariates <- setdiff(intersect(colnames(data), colnames(daily_survival)), 'age')
    }

    # If using default UKmortality data set then if don't have sex as a population covariate stratify it
    # to 'overall' mortality
    if (miss_pop_data & !'sex' %in% population_covariates) {
        daily_survival <- daily_survival[ sex == 'overall']
    }

    validate_population_survival(daily_survival, data, population_covariates)

    obj$call <- func_call
    obj$pop_covars <- population_covariates
    obj$pop_data <- daily_survival
    obj$cure_time <- cure_time

    class(obj) <- 'fixedcure'

    obj
}

#' @export
predict_survival_probability.fixedcure <- function(object, newdata, times) {

    # For R CMD CHECK
    time_to_index <- NULL
    age <- NULL
    surv_prob <- NULL
    adj_prob <- NULL
    time_calc_survival <- NULL
    id <- NULL
    cured <- NULL
    i.surv <- NULL
    surv <- NULL
    . <- NULL

    # Calculate raw survival time
    probs <- copy(newdata)
    probs[, c('id', 'time_to_index') := list(1:nrow(probs), times) ]
    probs[, cured := time_to_index > object$cure_time ]
    probs[, time_calc_survival := ifelse(cured, object$cure_time, time_to_index)]

    if (!is.null(object$coefs)) {
        probs[, surv_prob := predict_survival_probability.survregmin(object, .SD, time_calc_survival)]
    } else {
        # If have just population survival model then using 1 for survival here means
        # the following multiplications gives actual population survival rates
        probs[, surv_prob := 1]
    }

    if (sum(probs$cured) > 0) {
        # Cured survival probabilty is defined by Simon as
        # S(t) = S(curetime) * S*(t) / S*(curetime)
        # Where S*(.) is population survival rates calculated in terms of a person's age
        # S(curetime) was already calculated for these cured patients above and is held in 'surv_prob'

        pop_indices <- setNames(object$pop_covars, object$pop_covars)

        # Calculate the population survival rates by linking to the population survival rates
        cured_probs <- probs[cured == TRUE][, c('age_at_cure', 'age_at_index') := list(floor(age * DAYS_IN_YEAR + object$cure_time),
                                                                                       floor(age * DAYS_IN_YEAR + time_to_index))]

        # Ugly double join, just want to save having to make intermediary variables
        full_join <- object$pop_data[object$pop_data[cured_probs,
                                                     on=c('age'='age_at_cure', pop_indices)],   # First join to get survival at cure, i.surv
                                     .(id, adj_prob=surv_prob * (surv / i.surv)),               # Calculate adjusted survival probability
                                     on=c('age'='age_at_index', pop_indices)]                   # Second join to get survival at index

        # Assume that those who don't have a survival probability in life table are older than maximum age covered.
        # This is a large assumption, although the population mortality data has been already been checked that it includes
        # all possible non-age fields in the registry data, so these NAs can only be arising from age, in which case
        # we'd expect it to be too old rather than too young.
        # Doesn't matter too much anyway, as in the main prevalence function it sets all patients older than a set age to be dead
        # if the user wishes.
        full_join[is.na(adj_prob), adj_prob := 0]

        # Then join this back into probs, only updating those we want to
        probs[full_join, surv_prob := adj_prob, on='id']
    }

    # Return a vector
    probs[, surv_prob]
}

#' @export
extract_covars.fixedcure <- function(object) {
    # Always need 'age' in cure models!
    unique(c(object$covars, object$pop_covars, 'age'))
}

#' @exportS3Method base::print
print.fixedcure <- function(x, ...) {
    to_print <- x[c('coefs', 'covars', 'call', 'dist', 'terms', 'pop_covars', 'cure_time')]
    to_print$population_survival <- summary(x$pop_data)
    base::print(to_print)
}
