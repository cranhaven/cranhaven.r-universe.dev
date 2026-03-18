#' Form bootstrapped survival curves.
#'
#' Calculates bootstrapped survival probabilities from the Weibull models fitted
#' to the \code{prevalence} object.
#'
#' @param formula A \code{prevalence} object.
#' @param newdata A list or dataframe with the covariate values to calculate
#'   survival probabilities for. Defaults to using the mean values from the the
#'   original dataset that the model was fit to.
#' @param ... Other arguments to \code{survfit}.
#' @return An S3 object of class \code{survfit.prev} with the following
#'   attributes: \item{time}{A vector of time points at which survival
#'   probability has been calculated.} \item{surv}{A matrix of survival
#'   probabilities, where the rows represent a different bootstrapped Weibull
#'   model, and the columns are each timepoint.} \item{fullsurv}{A vector of
#'   survival probabilities for the predictors provided in newdata.}
#'
#' @import stats
#' @export
survfit.prevalence <- function(formula, newdata=NULL, ...) {
    if (is.null(newdata)) {
        newdata <- formula$means
    } else {
        # Check names have same names as those in original data
        if (!all(sort(names(newdata)) == sort(names(formula$means))))
            stop("Error: Please provide a list with the same column names as in the original dataset.")

        # Set levels of newdata to the levels in the original data (which have been previously saved in 'means')
        orig_levels <- lapply(formula$means, levels)
        cat_vars <- orig_levels[!sapply(orig_levels, is.null)]
        for (var in names(cat_vars)) {
            levels(newdata[[var]]) <- cat_vars[[var]]
        }

    }
    if (is.null(formula$max_event_time)) {
        stop("Error: Cannot fit survfit model as event times have not been saved in the prevalence ",
             "object. Please provide incident and event dates in the prevalence call.")
    }

    times <- seq(formula$max_event_time)
    ndata <- newdata[rep(seq(nrow(newdata)), length(times)), ]

    survprobs <- lapply(formula$surv_models, predict_survival_probability, ndata, times)
    survprobs_mat <- t(simplify2array(survprobs))

    full_survprobs <- predict_survival_probability(formula$full_surv_model, ndata, times)

    result <- list(time=times, surv=survprobs_mat, fullsurv=full_survprobs)
    attr(result, 'class') <- 'survfit.prev'
    result
}

#' @export
print.survfit.prev <- function(x, ...) {
    cat("Survival probability calculated at", length(x$time),
        "timepoints, across", dim(x$surv)[1], "bootstraps.")
}

#' Obtain N-year survival probability estimates.
#'
#' Summarises survival information at pre-specified years of interest on a
#' \code{survfit.prev} object.
#'
#' Survival probability is estimated as the mean of the bootstrapped survival
#' curves at a specific timepoint, with 2.5% and 97.5% quantiles providing 95%
#' confidence intervals. Survival probability can only be estimated at time
#' points less than the maximum survival time in the original dataset that the
#' \code{prevalence} object was fitted to.
#'
#' @param object A \code{survfit.prev} object.
#' @param years A vector of years for which to estimate survival probability
#'   from the bootstrapped survival curves.
#' @param ... Arguments passed to main \code{summary} function.
#' @return None, displays the survival probabilities to screen as a side-effect.
#' @examples
#' data(prevsim)
#'
#' \dontrun{
#' prev_obj <- prevalence(Surv(time, status) ~ age(age) + sex(sex) +
#'                        entry(entrydate) + event(eventdate),
#'                        data=prevsim, num_years_to_estimate = c(5, 10),
#'                        population_size=1e6, start = "2005-09-01",
#'                        num_reg_years = 8, cure = 5)
#'
#' survobj <- survfit(prev_obj, newdata=list(age=65, sex=0))
#'
#' summary(survobj)
#'
#' summary(survobj, years=c(1, 3, 5, 7))
#' }
#'
#' @export
summary.survfit.prev <- function(object, years=c(1, 3, 5), ...) {

    # Truncate years to the maximum number allowed
    max_years <- floor(dim(object$surv)[2] / DAYS_IN_YEAR)
    if (sum(years > max_years) > 0)
        message("Cannot estimate survival probabilities beyond the ",
                max_years, " years provided in the dataset.")
    years <- years[years <= max_years]
    days <- sapply(years, function(x) floor(x * DAYS_IN_YEAR))

    probs <- colMeans(as.matrix(object$surv[, days]))
    lower <- apply(object$surv[, days], 2, function(x) quantile(x, 0.025))
    upper <- apply(object$surv[, days], 2, function(x) quantile(x, 0.975))

    cat("Survival probability estimated using", dim(object$surv)[1],
        "bootstrap survival curves:\n")
    f <- mapply(function(x, y, l, u)
                cat(x, " year survival: ", y, " (", l, " - ", u, ") \n",
                    sep=''),
                years, round(probs, 3), round(lower, 3), round(upper, 3))
}

#' Plot bootstrapped survival curves.
#'
#' This method plots survival curves for a \code{survfit.prev} object.
#'
#' The survival curve for a model formed on all the data is displayed in orange,
#' while the 95% confidence interval for the bootstrapped models are displayed
#' as a grey ribbon.
#'
#' @param x A \code{survfit.prev} object.
#' @param ... Arguments passed to \code{plot}.
#' @return An S3 object of class \code{ggplot}.
#' @examples
#' data(prevsim)
#'
#' \dontrun{
#' prev_obj <- prevalence(Surv(time, status) ~ age(age) + sex(sex) +
#'                        entry(entrydate) + event(eventdate),
#'                        data=prevsim, num_years_to_estimate = c(5, 10),
#'                        population_size=1e6, start = "2005-09-01",
#'                        num_reg_years = 8, cure = 5)
#'
#' survobj <- survfit(prev_obj, newdata=list(age=65, sex=0))
#'
#' plot(survobj)
#' }
#'
#' @export
#' @importFrom magrittr "%>%"
plot.survfit.prev <- function(x, ...) {
    num_boot <- dim(x$surv)[1]
    num_days <- dim(x$surv)[2]
    if (num_days != length(x$time))
        stop("Error: Number of survival probabilities not consistent with time variable.")

    df <- data.frame(x$surv)
    colnames(df) <- seq(num_days)
    df[, 'bootstrap'] <- seq(num_boot)

    gathered <- tidyr::gather(df, 'time', 'survprob', -'bootstrap')

    smooth <- gathered %>%
        dplyr::group_by_('time') %>%
        dplyr::summarise_(mx=lazyeval::interp(~quantile(v, 0.975), v=as.name('survprob')),
                          mn=lazyeval::interp(~quantile(v, 0.025), v=as.name('survprob'))) %>%
        dplyr::mutate_(time=lazyeval::interp(~as.numeric(v), v=as.name('time'))) %>%
        dplyr::arrange_('time')

    p <- ggplot2::ggplot() +
            ggplot2::geom_ribbon(data=smooth,
                                 ggplot2::aes_string(x='time', ymin='mn', ymax='mx'), alpha=0.15) +
            ggplot2::geom_line(data=data.frame(time=as.numeric(seq(num_days)), survprob=x$fullsurv),
                               ggplot2::aes_string(x='time', y='survprob'), colour='orange', size=1) +
            ggplot2::theme_bw() +
            ggplot2::ylim(0, 1) +
            ggplot2::labs(x="Time", y="Survival probability")
    p
}
