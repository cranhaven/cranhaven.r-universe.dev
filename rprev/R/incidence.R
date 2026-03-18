#' Inspects disease incidence for its compatibility with a homogeneous Poisson process.
#'
#' Calculates incidence by year of the registry data, along with mean incidence
#' with confidence intervals. A smoothed cumulative incidence function is fit to
#' the data for inspecting deviations in the registry data from a homogeneous
#' Poisson process.
#'
#' Annual incidence rates are calculated for every year that is present in
#' \code{entry}, with years being delimited by the date specified in \code{year_start}
#' that include every incident case.
#' For example, under the default values, if the earliest incident date in \code{entry}
#' is 1981-04-28, and the latest is 2016-12-16, then annual incidence rates will be
#' calculated with the boundaries [1981-01-01, 1982-01-01), ..., [2016-01-01, 2017-01-01).
#'
#' If \code{year_start} was specified as '09-01' then the boundaries would be
#' [1980-09-01, 1981-09-01), ..., [2016-09-01, 2017-09-01).
#'
#' The \code{truncate_start} and \code{truncate_end} arguments remove incident
#' cases in the first and last years before and after the yearly boundaries
#' respectively.
#'
#' So if they were both \code{TRUE}, with \code{year_start} as '09-01' as before, then the
#' boundaries would be [1981-09-01, 1982-09-01), ..., [2015-09-01, 2016-09-01),
#' i.e. the incident cases in [1981-04-28, 1981-09-01) are discarded by \code{truncate_start}
#' and those in [2016-09-01, 2016-12-16] removed by \code{truncate_end}.
#'
#' This helps to ensure that annual incidence is measured on a time-scale appropriate
#' for your registry.
#'
#' @param entry Vector of diagnosis dates for each patient in the registry in
#'   the format YYYY-MM-DD.
#' @param year_start Date which to use to delimit years in the format MM-DD.
#'   See details for how this is used.
#' @param truncate_start See details.
#' @param truncate_end See details.
#' @param population_size The population of the area covered by the registry. If not provided
#'   then only absolute incidence can be calculated.
#' @param df The desired degrees of freedom of the smooth.
#' @param proportion The denominator of the incidence rate.
#' @param level The desired confidence interval width.
#' @param precision The number of decimal places required.
#' @return An S3 object of class \code{incidence} with the following attributes:
#'   \item{yearly_incidence}{Vector of absolute incidence values for each included
#'   year of the registry}
#'   \item{ordered_diagnoses}{Vector of times (days) between diagnosis date and
#'   the earliest date of inclusion in the registry, ordered shortest to
#'   longest.}
#'   \item{smooth}{Smooth fitted to the cumulative incidence data.}
#'   \item{index_dates}{Dates delimiting the years in which incidence is
#'   calculated.}
#'   \item{mean}{List containing absolute yearly incidence as well as relative
#'   rates.}
#'   \item{pvals}{p-values resulting to a test of over and under dispersion on the
#'   incidence data respectively. Used to test the suitability of the homogeneous
#'   Poission process assumption.}
#'   \item{dof}{Degrees of freedom of the smooth.}
#' @examples
#' data(prevsim)
#'
#' \dontrun{
#' test_homogeneity(prevsim$entrydate)
#' }
#'
#' @export
test_homogeneity <- function(entry, year_start='01-01',
                             truncate_start=FALSE,
                             truncate_end=FALSE,
                             population_size=NULL, df=4, proportion=1e5,
                             level=0.95, precision=2) {

    # Obtain registry start and end points
    entry <- as.Date(entry)
    min_date <- min(entry)
    min_year <- strftime(min_date, "%Y")
    max_date <- max(entry)
    max_year <- strftime(max_date, "%Y")

    potential_start <- as.Date(strptime(paste(min_year, year_start, sep='-'), format="%Y-%m-%d"))
    potential_end <- as.Date(strptime(paste(max_year, year_start, sep='-'), format="%Y-%m-%d"))

    if (truncate_start) {
        entry <- entry[entry >= potential_start]
        min_date <- min(entry)
    }
    if (truncate_end) {
        entry <- entry[entry <= potential_end]
        max_date <- max(entry)
    }

    # If earliest and latest incident cases lie within these years then use them
    start_date <- if (min_date >= potential_start) {
        potential_start
    } else {
        as.Date(strptime(paste(as.numeric(min_year)-1, year_start, sep='-'), format="%Y-%m-%d"))
    }

    end_date <- if (max_date <= potential_end) {
        potential_end
    } else {
        as.Date(strptime(paste(as.numeric(max_year)+1, year_start, sep='-'), format="%Y-%m-%d"))
    }

    # Determine yearly endpoints
    yearly_endpoints <- paste(seq(as.numeric(strftime(start_date, "%Y")),
                                  as.numeric(strftime(end_date, "%Y"))),
                              year_start, sep='-')

    # Slightly confused that the following are not all integers:
    diags <- sort(as.numeric(difftime(entry, min(entry), units='days')))
    smo <- smooth.spline(diags, seq_along(diags), df=df)

    # Calculate number that are incident each year
    yearly_inc <- vapply(seq(length(yearly_endpoints)-1),
                       function(i) sum(entry >= yearly_endpoints[i] & entry < yearly_endpoints[i+1]),
                       integer(1))

    object <- list(yearly_incidence=yearly_inc,
                   ordered_diagnoses=diags,
                   smooth = smo,
                   index_dates=yearly_endpoints,
                   mean=mean_incidence_rate(yearly_inc, population_size=population_size,
                                            proportion=proportion,
                                            precision=precision, level=level),
                   pvals=test_dispersion(yearly_inc),
                   dof=df)
    attr(object, 'class') <- 'incdiag'
    object
}


# Calculates the average incidence rate per one hundred thousand with
# confidence intervals for the given registry data.
#
# param raw_inc Vector of incidence values by year.
# return A list with the following values:
#
#   \item{absolute}{Overall incidence for the period of interest as a single
#   double} \item{per100K}{Incidence for the period of interest per one hundred
#   thousand} \item{per100K.lower}{Lower bounds of the specified confidence
#   level on the per one hundred thousand estimate} \item{per100K.upper}{Upper
#   bounds of the specified confidence level on the per one hundred thousand
#   estimate}
mean_incidence_rate <- function(raw_inc, population_size=NULL, precision = 2, level=0.95,
                                proportion=1e5){
    mean_rate <- mean(raw_inc)
    z_conf <- qnorm((1+level)/2)
    object <- list(absolute = mean_rate)

    if (!is.null(population_size)) {
        CI <- proportion * (z_conf * sqrt(mean_rate) / length(raw_inc)) / population_size
        est <- proportion * mean_rate / population_size

        lab <- paste0('per', proportion_label(proportion))

        object[[lab]] <- est
        object[[paste0(lab, '.lower')]] <- est - CI
        object[[paste0(lab, '.upper')]] <- est + CI
    }

    lapply(object, round, precision)
}


#' Visualise disease incidence.
#'
#' Plots a comparison between the smoothed daily incidence function and actual
#' incidence.
#'
#' This function generates a plot from the cumulative incidence object. The
#' incidence rate per year of the registry is shown in red. Mean incidence rate
#' is shown as a solid blue line, with the confidence interval shown in dashed
#' blue lines. The smooth fitted to the cumulative incidence data is shown in
#' green.
#' @param x An \code{incidence} object.
#' @param level The desired confidence interval width.
#' @param ... Arguments passed to \code{plot}.
#' @return An object of class \code{ggplot}.
#' @examples
#' data(prevsim)
#'
#' \dontrun{
#' inc <- test_homogeneity(prevsim$entrydate, population_size=1e6,
#'                         start = "2004-01-30", num_reg_years = 9)
#'
#' plot(inc)
#' }
#'
#' @export
plot.incdiag <- function(x, level=0.95, ...){
    raw_incidence <- x$yearly_incidence
    mean_rate <- mean(raw_incidence)
    day_mean_rate <- mean_rate / DAYS_IN_YEAR

    z_conf <- qnorm((1+level)/2)
    CI_lim <- z_conf * sqrt(mean_rate)/DAYS_IN_YEAR
    num_reg_years <- length(raw_incidence)

    inc_rate <- data.frame(inc=raw_incidence/DAYS_IN_YEAR, day=as.Date(x$index_dates[-length(x$index_dates)]) + 182, col='r')
    pred_rate <- predict(x$smooth, seq(num_reg_years*DAYS_IN_YEAR), deriv=1)
    smooth_rate <- data.frame(rate=pred_rate$y, day=as.Date(x$index_dates[1]) + pred_rate$x, col='g')
    mean_rate <- data.frame(mean=day_mean_rate, upper=day_mean_rate+CI_lim,
                            lower=day_mean_rate-CI_lim, col='b')
    ci_diff <- 0.5 * (mean_rate$upper - mean_rate$lower)

    p <- ggplot2::ggplot() +
            ggplot2::geom_point(data=inc_rate, ggplot2::aes_string(x='day', y='inc', colour='col')) +
            ggplot2::geom_line(data=inc_rate, ggplot2::aes_string(x='day', y='inc', colour='col'), size=1) +
            ggplot2::geom_line(data=smooth_rate, ggplot2::aes_string(x='day', y='rate', colour='col'),  size=1) +
            ggplot2::geom_hline(data=mean_rate, ggplot2::aes_string(yintercept='mean', colour='col')) +
            ggplot2::geom_hline(data=mean_rate, ggplot2::aes_string(yintercept='upper', colour='col'),
                                linetype='dashed') +
            ggplot2::geom_hline(data=mean_rate, ggplot2::aes_string(yintercept='lower', colour='col'),
                                linetype='dashed') +
            ggplot2::labs(x='Year', y='Daily incidence rate') +
            ggplot2::theme_bw() +
            ggplot2::theme(legend.position='bottom') +
            ggplot2::scale_colour_manual(name='Data',
                                         values=c('r'='red', 'g'='green', 'b'='#0080ff'),
                                         breaks=c('r', 'g', 'b'),
                                         labels=c('Actual incidence', 'Smoothed incidence', 'Mean actual incidence'))
    p
}

#' @export
print.incdiag <- function(x, ...) {
    cat("Cumulative incidence object with", length(x$yearly_incidence), "years of data.\n")
    cat("Smooth fitted using", x$dof, "degrees of freedom.\n")

}

#' @export
summary.incdiag <- function(object, ...) {
    cat("Number of years of registry data:", length(object$yearly_incidence), "\n")

    cat("\nIncidence\n~~~~~~~~~\n")
    cat("Known incidence by year:", object$yearly_incidence, "\n")
    cat("Diagnoses (time since registry began):\n")
    print(summary(object$ordered_diagnoses))
    cat("p-values for over/under dispersion:", object$pvals, "\n")

    cat("\nFitted smooth:\n~~~~~~~~~~~~~\n")
    print(object$smooth)
}
