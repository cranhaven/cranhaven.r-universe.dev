new_point_estimate <- function(year, sim_results, index, registry_data, prev_formula, registry_start_date, status_col,
                               population_size=NULL, proportion=1e5,
                               level=0.95, precision=2) {
    if (year <= 0) {
        warning("Cannot estimate prevalence for a non-positive value of num_year_to_estimate.")
        return(list(absolute.prevalence=0))
    }

    # CRAN check
    incident_date <- NULL
    sim <- NULL

    # See if need simulation if have less registry data than required
    initial_date <- index - lubridate::years(year)
    need_simulation <- initial_date < registry_start_date

    # Only count prevalence if formula isn't null
    if (!is.null(prev_formula)) {
        count_prev <- counted_prevalence(prev_formula, index, registry_data, max(initial_date, registry_start_date), status_col)

        # See if appending prevalence to simulation data or it's entirely counted
        if (initial_date < registry_start_date) {
            stopifnot(!is.null(sim_results))

            col_name <- paste0("prev_", year, "yr")
            sim_contributions <- sim_results[incident_date < registry_start_date][, sum(get(col_name)), by=sim][[2]]  # Return results column
            the_estimate <- count_prev + mean(sim_contributions)

            # Closure to calculate combined standard error
            se_func <- build_se_func(counted_contribs=count_prev, sim_contribs=sim_contributions)

        } else {
            the_estimate <- count_prev
            # Closure to calculate standard error of counted data
            se_func <- build_se_func(counted_contribs=count_prev)
        }
    } else {
        # If don't have counted data then prevalence estimates are entirely simulated
        col_name <- paste0("prev_", year, "yr")
        sim_contributions <- sim_results[, sum(get(col_name)), by=sim][[2]]  # Return results column
        the_estimate <- mean(sim_contributions)

        # Closure to calculate standard error of simulated data
        se_func <- build_se_func(sim_contribs=sim_contributions)
    }

    result <- list(absolute.prevalence=the_estimate)

    if (!is.null(population_size)) {
        the_proportion <- (the_estimate / population_size) * proportion
        se <- se_func(population_size)

        z_level <- qnorm((1+level)/2)
        CI <- z_level * se * proportion

        # Setup labels for proportion list outputs
        est_lab <- paste0('per', proportion_label(proportion))
        upper_lab <- paste(est_lab, '.upper', sep='')
        lower_lab <- paste(est_lab, '.lower', sep='')
        result[[est_lab]] <- the_proportion
        result[[upper_lab]] <- the_proportion + CI
        result[[lower_lab]] <- the_proportion - CI
    }

    lapply(result, round, precision)
}

build_se_func <- function(counted_contribs=NULL, sim_contribs=NULL) {
    # Pure simulated
    if (is.null(counted_contribs)) {
        function(pop_size) {
            calculate_se_sim(pop_size, sim_contribs)
        }
    }  else if (is.null(sim_contribs)) {
        # Pure counted
        function(pop_size) {
            calculate_se_counted(pop_size, counted_contribs)
        }
    } else {
        # Combination
        function(pop_size) {
            calculate_se_combined(pop_size, counted_contribs, sim_contribs)
        }
    }
}

calculate_se_combined <- function(population_size, counted_contribs, sim_contribs) {
    calculate_se_sim(population_size, sim_contribs) +
        calculate_se_counted(population_size, counted_contribs)
}

calculate_se_sim <- function(population_size, sim_contribs) {
    sd(sim_contribs) / population_size
}
calculate_se_counted <- function(population_size, counted_contribs) {
    raw_proportion <- counted_contribs / population_size
    sqrt((raw_proportion * (1 - raw_proportion)) / population_size)
}
