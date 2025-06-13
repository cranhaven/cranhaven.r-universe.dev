#' Create simulations for long-term effects in models with temporal dependence
#'
#' @param obj a fitted model object.
#' @param obj_tvar character string specifying the name of the base time variable
#' in \code{obj}.
#' @param cf a data frame with the first row containing the counterfactual.
#' An optional second row could be supplied with values for the baseline
#' scenario. If not supplied then all values are set to zero for the baseline.
#' Columns should have names that match variables in \code{obj} and contain
#' fitted values to find quantities of interest for. Note, this should not
#' include your time variable as this is handled using \code{t_points}.
#' @param cf_duration a character string or numeric specifying the
#' counterfactual's duration. If \code{'permanent'} then the
#' counterfacutal lasts for the full time span in \code{t_points}. If
#' \code{'one-time'} then the counterfactual only lasts for one period.
#' If \code{cf_duration} is numeric then the number specifies the
#' number of time increments at which the counterfactual resets.
#' @param t_points a numeric vector with a minimum length of 2 and a maximum
#' lentgh of 3. The first and last values should be the time starting and ending
#' points for the simulatinos. The (optional) middle value can specify a point
#' between the first and last time points where a subsequent event occurs.
#' @param FUN a function for finding a quantity of interest from the linear
#' systematic component. See \code{\link{qi_builder}}. The default is a function
#' for finding the predicted probability from a logistic regression model.
#' @param nsim number of simulations to draw.
#' @param ci the proportion of the central interval of the simulations to
#' return. Must be in (0, 1] or equivalently (0, 100].
#'
#' @return A data frame with the medians and central intervals of the
#' simulated scenarios. Note that the column \code{scenario_name}
#' encodes scenarios where y = 0 as \code{baseline} and y = 1 as
#' \code{counterfactual}.
#'
#' @examples
#' data('negative')
#'
#' # BTSCS set the data
#' neg_set <- btscs(df = negative, event = 'y', t_var = 'tim',
#'                 cs_unit = 'group', pad_ts = FALSE)
#'
#' # Create temporal dependence variable
#' neg_set$t <- neg_set$spell + 1
#'
#' m1 <- glm(y ~ x + t + I(t^2) + I(t^3),
#'           family = binomial(link = 'logit'), data = neg_set)
#'
#' # Create fitted counterfactual
#' counterfactual <- data.frame(x = 0.5)
#'
#' # Permanent counterfactual, one event
#' sim1 <- plte_builder(obj = m1, obj_tvar = 't',
#'                      cf = counterfactual, t_points = c(13, 25))
#'
#' # Multiple events
#' sim2 <- plte_builder(obj = m1, obj_tvar = 't',
#'                      cf = counterfactual, t_points = c(13, 18, 25))
#'
#' # One-time counterfactual
#' sim3 <- plte_builder(obj = m1, obj_tvar = 't',
#'                      cf = counterfactual, t_points = c(13, 25),
#'                      cf_duration = 'one-time')
#'
#' # Temporary (4 period counterfactual)
#' sim4 <- plte_builder(obj = m1, obj_tvar = 't',
#'                      cf = counterfactual, t_points = c(13, 25),
#'                      cf_duration = 4)
#'
#' # Custom baseline scenario
#' # Note: the second row is the custom baseline
#' counterfactual_baseline <- data.frame(x = c(1, 0.5))
#'
#' sim5 <- plte_builder(obj = m1, obj_tvar = 't', cf_duration = 4,
#'                      cf = counterfactual_baseline, t_points = c(13, 25))
#'
#' # Time splines
#' library(splines)
#' m2 <- glm(y ~ x + bs(t, degree = 3), family = binomial(link = 'logit'),
#'           data = neg_set)
#'
#' sim6 <- plte_builder(obj = m2, obj_tvar = 't', cf_duration = 4,
#'                      cf = counterfactual, t_points = c(13, 25))
#'
#' @source
#' Williams, Laron K. 2016. "Long-Term Effects in Models with Temporal
#' Dependence". Political Analysis: 24(2): 243-262.
#'
#' @importFrom coreSim qi_builder
#'
#' @export

plte_builder <- function(obj, obj_tvar,
                         cf, cf_duration = 'permanent',
                         t_points, FUN = logistic_prob_FUN,
                         ci = 0.95, nsim = 1000)
{
    # Create scenarios to simulate ---------------------------------------------
    if (missing(obj_tvar))
        stop('obj_tvar must be specified.', call. = FALSE)
    if (!(obj_tvar %in% bs_stripper(names(obj$coefficients))))
        stop('Cannot find obj_tvar in the fitted model object.', call. = FALSE)
    if (obj_tvar %in% colnames(cf)) {
        message('It is not necessary to include the time variable in cf.',
                call. = FALSE)
        cf[, obj_tvar] <- NULL
    }

    if (length(t_points) < 2)
        stop('Must specify at least two t_range values.', call. = FALSE)
    if (length(t_points) > 3)
        stop('t_points can only include 2 or three time points.', call. = FALSE)

    t_start = t_points[1]
    t_end = t_points[length(t_points)]
    t_no_events <- c(t_start, t_start:t_end)
    t_change <- c(rep(t_start, 2), 0:(t_end - t_start - 1))

    if (length(t_points) == 3) {
        temp <- t_no_events
        for (u in seq_along(temp)) {
            if (t_points[2] <= temp[u]) temp[u] <- temp[u] - t_points[2]
            if (temp[u] < t_change[u]) t_change[u] <- temp[u]
        }
    }

    nrow_cf <- nrow(cf)
    if (!(nrow_cf %in% 1:2))
        stop('cf must have only one or two rows.', call. = FALSE)

    # Create baseline scenario if not supplied
    names_cf <- names(cf)
    if (nrow_cf == 1) {
        make_zero <- function(x) x * 0
        baseline <- data.frame(apply(cf, 1, make_zero))
    }
    else if (nrow_cf == 2) {
        baseline <- data.frame(cf[2, ])
        cf <- data.frame(cf[1, ])
        names(cf) <- names_cf
    }
    names(baseline) <- names_cf

    # Find counterfactuals over time
    if (length(cf_duration) != 1)
        stop('cf_duration can only have one value', call. = FALSE)

    npost_base <- (t_end - t_start) + 1

    if (cf_duration == tolower('permanent')) {
        post_base_cf <- df_repeat(cf, npost_base)
        cf <- rbind(baseline, post_base_cf)
    }
    else if (cf_duration == tolower('one-time')) {
        cf <- rbind(baseline, cf, df_repeat(baseline, npost_base - 1))
    }
    else if (is.numeric(cf_duration)) { # temporary
        cf <- rbind(df_repeat(baseline, 1), df_repeat(cf, cf_duration),
                    df_repeat(baseline, npost_base - cf_duration))
    }

    scenarios <- list(baseline = data.frame(cf, t_no_events),
                      counterfactual = data.frame(cf, t_change))

    # Find quantities of interest ----------------------------------------------
    sims <- data.frame()
    for (i in 1:length(scenarios)) {
        temp <- scenarios[[i]]
        names(temp) <- c(names(temp)[-ncol(temp)], obj_tvar)

        temp_sims <- data.frame()
        for (u in 1:nrow(temp)) {
            temp_1 <- temp[u, ]
            temp_1 <- qi_builder(obj = obj, newdata = temp_1, FUN = FUN,
                                    ci = ci, nsim = nsim, slim = TRUE,
                                    original_order = TRUE)
            temp_sims <- rbind(temp_sims, temp_1)
        }
        temp_sims$scenario_name <- names(scenarios)[i]
        temp_sims$scenario_time <- 1:nrow(temp_sims)
        sims <- rbind(sims, temp_sims)
    }

    ncs <- ncol(sims)
    sims <- cbind(sims[, c((ncs-1):ncs)], sims[, c(1:(ncs-2))])

    class(sims) <- c('data.frame', 'plte')
    attr(sims, 'obj_tvar') <- obj_tvar
    return(sims)

}
