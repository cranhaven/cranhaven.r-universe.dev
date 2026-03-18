INCIDENCE_MARGIN <- 1.5
MIN_INCIDENCE <- 10

#' Estimate point prevalence at an index date.
#'
#' Point prevalence at a specific index date is estimated using contributions to
#' prevalence from both available registry data, and from Monte Carlo
#' simulations of the incidence and survival process, as outlined by Crouch et
#' al (2004) (see References).
#'
#' The most important parameter is \code{num_years_to_estimate}, which governs
#' the number of previous years of data to use when estimating the prevalence at
#' the index date. If this parameter is greater than the number of years of
#' known incident cases available in the supplied registry data (specified with
#' argument \code{num_registry_years}), then the remaining
#' \code{num_years_to_estimate - num_registry_years} years of incident data will
#' be simulated using Monte Carlo simulation.
#'
#' The larger \code{num_years_to_estimate}, the more accurate the prevalence
#' estimate will be, provided an adequate survival model can be fitted to the
#' registry data. It is therefore important to provide as much clean registry
#' data as possible.
#'
#' Prevalence arises from two stochastic processes: incidence and survival.
#' This is reflected in the function arguments by multiple options for
#' each of these processes.
#'
#' The incidence process is specified by an object
#' that has an associated \code{draw_incident_population} method, which produces the new
#' incident population. The default implementation is a homogeneous Poisson process,
#' whereby interarrival times are distributed according to an exponential distribution.
#' The \code{inc_formula} argument specifies the nature of this process, see the
#' description for more details. See the vignette for guidance on providing a custom incidence
#' object.
#'
#' The survival process is characterised by a method \code{predict_survival_probability},
#' that estimates the probability of a given individual being alive at the index date.
#' The default object is a parametric distribution with the functional form being specified
#' in \code{surv_formula} and distribution given in \code{dist}. See the vignette for guidance
#' on providing a custom survival model.
#'
#' @param index The date at which to estimate point prevalence as a string in the format
#' YYYY-MM-DD.
#' @param num_years_to_estimate Number of years of data to consider when
#'   estimating point prevalence; multiple values can be specified in a vector.
#'   If any values are greater than the number of years of registry data
#'   available before \code{index_date}, incident cases
#'   for the difference will be simulated.
#' @param data A data frame with the corresponding column names provided in
#'   \code{form}.
#' @param inc_formula A formula specifying the columns used in the incidence process.
#'     The LHS should be the name of the column holding the incident dates,
#'     with the RHS specifying any variables that should be stratified by, or 1 if no
#'     stratification. For example, with the supplied \code{prevsim} data set, it could
#'     be used as follows:
#'
#'     \code{entrydate ~ 1} for a non-stratified process.
#'     \code{entrydate ~ sex} for a process that will stratify incidence by sex.
#'
#' @param inc_model An object that has a \code{draw_incident_population}
#'     method. See the vignette for further guidance.
#' @param surv_formula A formula used to specify a survival model, where the
#' LHS a Surv object, as used by \code{flexsurvreg}.
#' @param dist The distribution used by the default parametric survival model.
#' @param surv_model An object that has a \code{predict_survival_probability}
#'     method. See the vignette for further guidance.
#' @param registry_start_date The starting date of the registry. If not supplied
#'   then defaults to the earliest incidence date in the supplied data set.
#' @param death_column A string providing the name of the column which holds the death
#'     date information. If not provided then prevalence cannot be counted and estimates
#'     will be solely derived from simulation.
#' @param incident_column A string providing the name of the column which holds the diagnosis
#'     date. If not provided either in this argument or in \code{inc_formula},
#'     then prevalence cannot be counted and estimates will be solely derived from simulation.
#' @param age_column A string providing the name of the column that holds patient age. If provided
#'     then patients alive at \code{age_dead} are set to die. This helps combat 'immortal' patients.
#' @param age_dead The age at which patients are set to be dead if they are still alive, to prevent
#'     'immortal' patients. Used in conjunction with \code{age_column}.
#' @param status_column A string providing the name of the column that holds patient event status at
#'     the event time. If not provided in \code{surv_formula} or in this argument then prevalence
#'     cannot be counted.
#' @param N_boot Number of bootstrapped calculations to perform.
#' @param population_size Integer corresponding to the size of the population at
#'   risk.
#' @param proportion The population ratio to estimate prevalence for.
#' @param level Double representing the desired confidence interval width.
#' @param precision Integer representing the number of decimal places required.
#'
#' @return A \code{prevalence} object containing the following attributes:
#'   \item{estimates}{Prevalence estimates at the specified years as both absolute and rates.}
#'   \item{simulated}{A \code{data.table} containing simulated incident cases from each bootstrap iteration
#'     Each row corresponds to a simulated incident case with their simulated attributes and survival status.
#'     Binary flags are provided beginning \code{prev_}, which indicate whether that person contributed
#'     to the prevalence for the specified time-period. The \code{prev_registry} flag indicates whether that
#'     person was incident during the registry time-span and alive at the index. These cases are used to
#'     assess the model fit, as the numbers can be simply compared to the known registry prevalence.}
#'   \item{counted}{The number of incident cases present in the registry data set.}
#'   \item{full_surv_model}{The survival model built on the complete registry data set.}
#'   \item{full_inc_model}{The incidence model built on the complete registry data set.}
#'   \item{surv_models}{A list of the survival models fitted to each bootstrap iteration.}
#'   \item{inc_models}{A list of the incidence models fitted to each bootstrap iteration.}
#'   \item{index_date}{The index date.}
#'   \item{est_years}{The years at which prevalence is estimated at.}
#'   \item{counted_incidence_rate}{The overall incidence rate in the registry data set.}
#'   \item{registry_start}{The date the registry was identified at starting at.}
#'   \item{proportion}{The denominator to use for estimating prevalence rates.}
#'   \item{status_col}{The column in the registry data containing the survival status.}
#'   \item{N_boot}{The number of bootstrap iterations that were run.}
#'   \item{means}{Covariate means, used when plotting Kaplan-Meier estimators using \code{survfit}.}
#'   \item{max_event_time}{The maximum time-to-event in the registry data. Again, used in
#'     \code{survfit} to scale the time-axis.}
#'   \item{pval}{The p-value resulting from a hypothesis test on the difference between the
#'   simulated and counted prevalence on the time-span covered by the registry. Tests the
#'   prevalence fit; if a significant result is found then further diagnostics are required.}
#'
#' @references Crouch, Simon, et al. "Determining disease prevalence from
#'   incidence and survival using simulation techniques." Cancer epidemiology
#'   38.2 (2014): 193-199.
#' @examples
#' data(prevsim)
#'
#' \dontrun{
#' data(prevsim)
#'
#' prevalence(index='2013-01-30',
#'            num_years_to_estimate=c(3, 5, 10, 20),
#'            data=prevsim,
#'            inc_formula = entrydate ~ sex,
#'            surv_formula = Surv(time, status) ~ age + sex,
#'            dist='weibull',
#'            population_size = 1e6,
#'            death_column = 'eventdate')
#' }
#'
#' @family prevalence functions
#' @import data.table
#' @export
prevalence <- function(index, num_years_to_estimate,
                       data,
                       inc_formula=NULL,
                       inc_model=NULL,
                       surv_formula=NULL,
                       surv_model=NULL,
                       registry_start_date=NULL,
                       death_column=NULL,
                       incident_column=NULL,
                       age_column='age',
                       age_dead=100,
                       status_column='status',
                       N_boot=1000,
                       population_size=NULL, proportion=1e5,
                       level=0.95,
                       dist=c('exponential', 'weibull', 'lognormal'),
                       precision=2) {

    # Needed for CRAN check
    alive_at_index <- NULL
    incident_date <- NULL

    if (is.null(incident_column)) {
        if (!is.null(inc_formula)) {
            incident_column <- all.vars(update(inc_formula, .~0))
        }
    }

    dist <- match.arg(dist)

    # Is it right for surv_formula to have precedence over surv_formula?
    if (!is.null(surv_formula)) {
        surv_LHS <- all.vars(update(surv_formula, .~0))
        status_column <- surv_LHS[length(surv_LHS)]
    }

    # Form formula for counted prevalence
    # extract entry column from incidence formula
    if (is.null(death_column)) {
        message("death_column not provided so prevalence cannot be counted over the registry. Estimates will be solely from simulation.")
        counted_formula <- NULL
    } else if (is.null(incident_column)) {
        message("incident_column not provided so prevalence cannot be counted over the registry. Estimates will be solely from simulation.")
        counted_formula <- NULL
    } else {
        counted_formula <- as.formula(paste(death_column, incident_column, sep='~'))
    }

    if (!is.null(incident_column)) {
        if (!incident_column %in% colnames(data)) {
            stop("Error: Cannot find incident column '", incident_column, "' in supplied data set.")
        }
        data[[incident_column]] <- lubridate::ymd(data[[incident_column]])
    }
    if (!is.null(death_column)) {
        if (!death_column %in% colnames(data)) {
            warning("Death column '", death_column, "' not found in supplied data set so estimates will be solely from simulation.")
            death_column <- NULL
        } else {
            data[[death_column]] <- lubridate::ymd(data[[death_column]])
        }
    }

    # This argument allows the user to specify when their registry started. I.e. it could have
    # started a month before received first incident case, in which case would want that taking into account when
    # estimating incidence rate and prevalence
    if (is.null(registry_start_date)) {
        if (is.null(incident_column)) {
            stop("Error: Unknown registry starting date. Please either provide 'registry_start_date' or the incident column in 'inc_formula'.")
        }
        registry_start_date <- min(data[[incident_column]])
    }

    index <- suppressWarnings(lubridate::ymd(index))
    if (is.na(index)) {
        stop("Error: Index date '", index, "' cannot be parsed as a date. Please enter it as a string in %Y%m%d or %Y-%m-%d format.")
    }
    registry_start_date <- lubridate::ymd(registry_start_date)
    sim_start_date <- index - lubridate::years(max(num_years_to_estimate))

    # NEED SIMULATION:
    #   - have N years > R registry years available
    #   - haven't provided date of death for registry data (not always available)
    if (!((sim_start_date >= registry_start_date) & (!is.null(death_column)))) {

        # Incidence models
        if (is.null(inc_model) & is.null(inc_formula)) {
            stop("Error: Please provide one of inc_model and inc_formula.")
        }
        if (!is.null(inc_model) & !(is.null(inc_formula))) {
            stop("Error: Please provide only one of inc_model and inc_formula.")
        }
        if (is.null(inc_model)) {
            inc_model <- fit_exponential_incidence(inc_formula, data)
        }

        # Survival models
        if (!is.null(surv_model) & !(is.null(surv_formula))) {
            warning("warning: both surv_model and surv_formula provided, survival model will be built using surv_model and surv_formula ignored.")
        }

        if (missing(surv_model) & missing (surv_formula)) {
            stop("Error: Please provide one of surv_model or surv_formula.")
        }

        if (!missing(surv_formula)) {
            surv_model <- build_survreg(surv_formula, data, dist)
        }

        prev_sim <- sim_prevalence(data, index, sim_start_date,
                                   inc_model, surv_model,
                                   age_column=age_column,
                                   age_dead=age_dead,
                                   N_boot=N_boot)

        # Create column indicating whether contributed to prevalence for each year of interest
        for (year in num_years_to_estimate) {
            # Determine starting incident date
            starting_incident_date <- index - lubridate::years(year)

            # We'll create a new column to hold a binary indicator of whether that observation contributes to prevalence
            col_name <- paste0("prev_", year, "yr")

            # Determine prevalence as incident date is in range and alive at index date
            prev_sim$results[, (col_name) := as.numeric((incident_date > starting_incident_date & incident_date < index) & alive_at_index)]
        }

    } else {
        prev_sim <- NULL
    }

    # Determine point estimates of prevalence by combining simulated and counted values
    names <- sapply(num_years_to_estimate, function(x) paste('y', x, sep=''))
    estimates <- lapply(setNames(num_years_to_estimate, names),
                        new_point_estimate,  # Function
                        prev_sim$results, index, data,
                        counted_formula,
                        registry_start_date,
                        status_column,
                        population_size, proportion, level, precision)

    full_surv_model <- if (!is.null(prev_sim)) prev_sim$full_surv_model else NULL
    full_inc_model <- if (!is.null(prev_sim)) prev_sim$full_inc_model else NULL
    surv_models <- if (!is.null(prev_sim)) prev_sim$surv_models else NULL
    inc_models <- if (!is.null(prev_sim)) prev_sim$inc_models else NULL

    if (!is.null(counted_formula)) {
        if (!status_column %in% colnames(data)) {
            stop("Error: cannot find status column '", status_column, "' in data frame.")
        }
        counted_prev <- counted_prevalence(counted_formula, index, data, registry_start_date, status_column)
    } else {
        counted_prev <- NULL
    }
    object <- list(estimates=estimates,
                   simulated=prev_sim$results,
                   counted=counted_prev,
                   full_surv_model=surv_model,
                   full_inc_model=full_inc_model,
                   surv_models=surv_models,
                   inc_models=inc_models,
                   index_date=index,
                   est_years=num_years_to_estimate,
                   counted_incidence_rate = nrow(data) / as.numeric(difftime(index,
                                                                             registry_start_date,
                                                                             units='days')),
                   registry_start=registry_start_date,
                   proportion=proportion,
                   status_col=status_column,
                   N_boot=N_boot)

    # Calculate covariate averages for survfit later on
    if (!is.null(prev_sim)) {
        covars <- extract_covars(surv_model)
        # Obtain if continuous or categorical
        is_factor <- sapply(covars, function(x) is.factor(data[[x]]) || is.character(data[[x]]))
        fact_cols <- covars[is_factor]
        cont_cols <- covars[!is_factor]
        cont_means <- sapply(cont_cols, function(x) mean(data[[x]]))
        cat_modes <- sapply(fact_cols, function(x) names(which.max(table(data[[x]]))))

        # Save into data frame
        means <- data.frame()
        for (var in cont_cols) {
            means[1, var] <- cont_means[var]
        }
        for (var in fact_cols) {
            means[1, var] <- cat_modes[var]
            means[[var]] <- factor(means[[var]], levels=levels(data[[var]]))
        }
        object$means <- means
    } else {
        object$means <- NULL
    }

    # Add max time if possible
    if (!is.null(incident_column) & !is.null(death_column)) {
        object$max_event_time <- max(as.numeric(difftime(data[[death_column]],
                                              data[[incident_column]],
                                              units='days')))
    }


    if (!is.null(prev_sim)) {
        object$pval <- test_prevalence_fit(object)
    }

    attr(object, 'class') <- 'prevalence'
    object

}

#' Count prevalence from registry data.
#'
#' Counts contribution to prevalence at a specific index from each year of a
#' registry. A person is included as contributing to disease prevalence if they
#' are incident within the specified time-span, and are either alive or censored
#' at the index date. The rationale for including censored cases in prevalence
#' estimation is that such cases have typically been lost to follow-up, and are
#' often more likely to have been alive at the index date than not.
#'
#' @inheritParams prevalence
#' @param formula A formula of the form <event date column> ~ <entry date column>.
#' @param start_date The initial date to start counting prevalence from as a \code{Date} object.
#'     Typically the index date - (Nyears * 365.25). Allows for non-whole year prevalence estimations.
#' @param status_col The name of the column holding a binary indicator variable
#'     of whether the individual experienced an event at their event time or was
#'     censored.
#'
#' @return The number of prevalent cases at the specified
#' index date as a single integer.
counted_prevalence <- function(formula, index, data, start_date, status_col) {
    death_col <- all.vars(update(formula, .~0))
    entry_col <- all.vars(update(formula, 0~.))

    incident <- data[[entry_col]] >= start_date & data[[entry_col]] < index

    # Use dead at index as it's a simpler boolean operation that just needs negating
    dead_at_index <- !(data[[death_col]] > index) & (data[[status_col]] == 1)

    sum(incident & !dead_at_index)
}

#' Estimate prevalence using Monte Carlo simulation.
#'
#' Estimates prevalent cases at a specific index date by use of Monte Carlo
#' simulation. Simulated cases are marked with age and sex to enable agreement
#' with population survival data where a cure model is used, and calculation of
#' the posterior distributions of each.
#' @inheritParams prevalence
#' @param starting_date The initial date to start simulating prevalence from as a \code{Date} object.
#'     Typically the index date - (Nyears * 365.25). Allows for non-whole year prevalence estimations.
#'
#' @return A list with the following attributes:
#'   \item{results}{A data.table containing the simulated incident populations from each
#'   simulation along with their covariates and survival status at the index.}
#'   \item{full_surv_model}{The survival model built on the full registry data set.}
#'   \item{full_inc_model}{The incidence model built on the full registry data set.}
#'   \item{surv_models}{A list containing survival models built on each bootstrap sample.}
#'   \item{inc_models}{A list containing incidence models built on each bootstrap sample.}
#' @importFrom survival survfit
sim_prevalence <- function(data, index, starting_date,
                           inc_model, surv_model,
                           age_column='age',
                           N_boot=1000,
                           age_dead=100)
                           {

    # Needed for CRAN check
    alive_at_index <- NULL
    time_to_index <- NULL

    data <- data[complete.cases(data), ]
    full_data <- data

    covars <- extract_covars(surv_model)
    number_incident_days <- as.numeric(difftime(index, starting_date, units='days'))

    run_sample <- function() {
        # bootstrap dataset
        data <- full_data[sample(seq(nrow(full_data)), replace=T), ]

        # fit incidence and survival models.
        bs_inc <- eval(inc_model$call)
        bs_surv <- eval(surv_model$call)

        # Draw the incident population using the fitted model and predict their death times
        incident_population <- draw_incident_population(bs_inc, full_data, number_incident_days, extract_covars(bs_surv))
        data.table::setDT(incident_population)

        # For each individual determine the time between incidence and the index
        incident_date <- as.Date(starting_date + incident_population[[1]])
        time_to_index <- as.numeric(difftime(index, incident_date, units='days'))

        # Estimate whether alive as Bernouilli trial with p = S(t)
        surv_prob <- predict_survival_probability(bs_surv, incident_population[, -1], time_to_index)
        incident_population[, 'incident_date' := incident_date]
        incident_population[, 'time_to_index' := time_to_index]
        incident_population[, 'alive_at_index' := rbinom(length(surv_prob), size=1, prob=surv_prob)]
        list(pop=incident_population,
             surv=bs_surv,
             inc=bs_inc)
    }

    # TODO Implement this and turn into user facing argument
    n_cores <- 1
    if (n_cores > 1) {
        message("Multi-core functionality not currently implemented, defaulting to single-core.")
    }
    all_results <- replicate(N_boot, run_sample(), simplify=FALSE)

    # Combine incident population into single table
    results <- data.table::rbindlist(lapply(all_results, function(x) x$pop), idcol='sim')

    # Force death at 100 if possible
    if (!is.null(age_column) & age_column %in% colnames(results)) {
        results[(get(age_column)*DAYS_IN_YEAR + time_to_index) > age_dead * DAYS_IN_YEAR, alive_at_index := 0]
    } else {
        message("No column found for age in ", age_column, ", so cannot assume death at 100 years of age. Be careful of 'infinite' survival times.")
    }

    # These intermediary columns aren't useful for the user and would just clutter up the output
    results[, c('time_to_index', 'time_to_entry') := NULL]

    list(results=results,
         full_surv_model=surv_model,
         full_inc_model=inc_model,
         surv_models=lapply(all_results, function(x) x$surv),
         inc_models=lapply(all_results, function(x) x$inc))
}

#' @export
print.prevalence <- function(x, ...) {
    cat(paste0("Estimated prevalence at ", x$index_date, ":\n"))
    lapply(names(x$estimates), function(item) {
        year <- strsplit(item, 'y')[[1]][2]
        abs_prev_est <- x$estimates[[item]][1]
        if (length(x$estimates[[item]]) > 1) {
            rel_prev <- x$estimates[[item]][2]
            rel_prev_est <- paste0("(", rel_prev, " per ", x$proportion, ")")
        } else {
            rel_prev_est <- NULL
        }
        cat(paste(year, "years:", abs_prev_est, rel_prev_est, '\n'))
    })
}

#' @export
summary.prevalence <- function(object, ...) {

    # Required to pass R CMD CHECK
    incident_date <- NULL
    sim <- NULL
    V1 <- NULL

    cat("Prevalence \n~~~~~~~~~~\n")
    print(object)
    cat("\n")

    cat("Registry Data\n~~~~~~~~~~~~~\n")
    cat("Index date:", as.character(object$index_date), "\n")
    cat("Start date:", as.character(object$registry_start), "\n")
    cat("Overall incidence rate:", round(object$counted_incidence_rate, 3), "\n")
    cat("Counted prevalent cases:", object$counted, "\n")

    if (!all(is.na(object$simulated))) {
        cat("\nSimulation\n~~~~~~~~~~\n")
        cat("Iterations:", object$N_boot, "\n")
        cat("Average incidence rate:",
            round(object$simulated[, length(incident_date), by=sim][, mean(V1)] / (max(object$est_years)*DAYS_IN_YEAR), 3),
            "\n")
        cat("P-value:", object$pval)
    }
}
