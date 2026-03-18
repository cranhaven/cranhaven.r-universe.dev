fit_exponential_incidence <- function(inc_form, data) {
    entry_col <- all.vars(update(inc_form, .~0))
    if (is.null(entry_col) || length(entry_col) != 1) {
        stop("Error: Please provide only a single LHS term in the inc_formula, representing the column holding incident date.")
    }

    if (! entry_col %in% colnames(data)) {
        stop("Error: Column '", entry_col, "' not found in '", quote(data), "'.")
    }

    strata <- all.vars(update(inc_form, 0~.))
    # Obtain rate for combination of each of these factors and throw error if any instances < required
    if (length(strata) == 1) {

        if (! strata %in% colnames(data)) {
            stop("Error: Column '", strata, "' not found in '", quote(data), "'.")
        }

        if (!all(sapply(strata, function(x) is.factor(data[[x]])))) {
            stop("Error: Please format any incidence strata as factors.")
        }

        strata_counts <- table(data[, strata])
        if (any(strata_counts < MIN_INCIDENCE)) {
            stop("Error: Less than ", MIN_INCIDENCE, " incident cases. Please ensure data has sufficient incident cases.")
        }

        # Calculate time between first and last patient for each strata
        durations <- vapply(names(strata_counts), function(s) {
            sub <- data[[strata]] == s
            as.numeric(difftime(max(data[sub, entry_col]), min(data[sub, entry_col]), units='days'))
        }, numeric(1))

        rate <- data.frame(strata_counts / durations)
        names(rate)[1:(ncol(rate)-1)] <- strata
        strata_names <- strata
    } else if (length(strata) > 1) {
        stop("Default incidence model can currently only handle one stratification.")
    } else {
        rate <- nrow(data) / as.numeric(difftime(max(data[[entry_col]]), min(data[[entry_col]]), units='days'))
        strata_names <- NULL
    }

    # Formulate call with the formula evaluated in this environment with the data argument as a promise
    # that will be evaluated later on the bootstrapped data
    func_call <- match.call()
    func_call$inc_form <- eval(inc_form)

    obj <- list(rate=rate, call=func_call, strata=strata_names)
    class(obj) <- "expinc"
    obj
}

#' @export
draw_incident_population.expinc <- function(object, data, timeframe, covars) {

    if (timeframe < 0) {
        stop("Error: timeframe is negative with a value of ", timeframe, ".")
    }

    for (c in covars) {
        if (!c %in% colnames(data)) {
            stop("Error: Column '", c, "' not found in '", quote(data), "'.")
        }
    }

    if (is.null(object$strata)) {
        initial_num_inds <- INCIDENCE_MARGIN * object$rate * timeframe
        time_to_entry <- cumsum(rexp(initial_num_inds, object$rate))
        time_to_entry <- time_to_entry[time_to_entry < timeframe]
        new_data <- data.table::data.table(time_to_entry)
    } else {
        foo <- apply(object$rate, 1, function(row) {
            rate <- as.numeric(row['Freq'])
            initial_num_inds <- INCIDENCE_MARGIN * rate * timeframe
            time_to_entry <- cumsum(rexp(initial_num_inds, rate))
            time_to_entry <- time_to_entry[time_to_entry < timeframe]
            # Form data frame with these factor levels too
            new_data <- data.table::data.table(time_to_entry)
            new_data[, names(row[-length(row)]) := row[-length(row)]]
            new_data
        })
        new_data <- data.table::rbindlist(foo)
    }


    if (length(covars) > 0) {
        # draw covariate values from model that are in survival formula and not in data frame
        missing_covars <- setdiff(covars, colnames(new_data))
        higher_order <- missing_covars[grep("^I\\(.+\\)$", missing_covars)]
        covars_to_sample <- setdiff(missing_covars, higher_order)

        # Possible to have length covars > 0 and no missing variables, i.e. if the variable is used as
        # stratification in incidence model
        if (length(covars_to_sample) > 0) {

            # Draw new covariates from prior with appropriate stratification
            if (is.null(object$strata)) {
                new_covars <- setNames(lapply(covars_to_sample, function(x) sample(data[[x]], nrow(new_data), replace=T)), covars_to_sample)
                data.table::setDT(new_covars)
            } else {
                strata_vals <- object$rate[[object$strata]]
                freqs <- table(new_data[[object$strata]])
                covars_list <- setNames(lapply(strata_vals, function(sx) {
                    priors <- data[data[[object$strata]] == sx, ]
                    num <- freqs[as.character(sx)]
                    setNames(lapply(covars_to_sample, function(x) sample(priors[[x]], num, replace=T)), covars_to_sample)
                }), strata_vals)
                new_covars <- data.table::rbindlist(covars_list)
            }

            # Add in higher order terms
            # NB This method of evaluating higher order won't work if the covariate isn't in this new_covars data, however,
            # this is acceptable as the covariates missing from this data set will be those used in the incidence modelling and
            # so will be categorical and thus not not have higher order terms
            for (term in higher_order) {
                new_covars[, (term) := eval(parse(text=term))]
            }

            # Combine together to form a population that has an entry time and the required covariates for modelling survival
            new_data <- cbind(new_data, new_covars)
        }
    }

    new_data
}
