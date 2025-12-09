#' Impute missing or implausible values
#'
#' This is a workhorse function used by \code{\link{impute_ndd}},
#' \code{\link{impute_qty}} and others.
#'
#' The argument \code{where} indicates which values are to be imputed.
#' It can be specified as either a vector or as a function. Thus you can
#' specify, for example, \code{\link{is.na}} to impute all missing values, or
#' you can pass in a vector, if it depends on something else rather than just
#' the current values of the variable to imputed.
#' This design may change in future. In particular, if we want to impute
#' implausible values and impute missing values separately, it's important that
#' these steps are independent.
#'
#' @import dplyr
#' @param data A data frame containing columns \code{prodcode}, \code{pracid}, \code{patid}
#' @param variable Unquoted name of the column in \code{dataset} to be imputed
#' @param method Method for imputing the values. See details.
#' @param where Logical vector, or function applied to \code{variable} returning such a vector, indicating which elements to impute. Defaults to \code{\link[base:NA]{is.na}}
#' @param group Level of structure for imputation. Defaults to whole study population.
#' @param ... Extra arguments, currently ignored
#' @param replace_with if the method 'replace' is selected, which value should be inserted?
#'
#' \itemize{
#' \item \code{ignore}. Do nothing, leaving input unchanged.
#' \item \code{mean}. Replace values with the mean by \code{group}
#' \item \code{median}. Replace values with the median by \code{group}
#' \item \code{mode}. Replace values with the most common value by \code{group}
#' \item \code{replace}. Replace values with \code{replace_with}, which defaults to \code{NA} (i.e. mark as missing)
#' \item \code{min}. Replace with minimum value.
#' \item \code{max}. Replace with maximum value.
#' \item \code{sum}. Replace with sum of values.
#' }
#'
#' @return A data frame of the same structure as \code{data}, with values imputed
#' @importFrom stats median
#' @export
impute <- function(data,
                   variable,
                   method = c('ignore', 'mean', 'median', 'mode', 'replace',
                              'min', 'max', 'sum'),
                   where = is.na,
                   group,
                   ...,
                   replace_with = NA_real_) {

  if (length(replace_with) != 1)
    stop('replace_with must be a scalar value')

  if (!is.numeric(replace_with))
    stop('replace_with should be numeric, but input was ', class(replace_with))

  where_fn <- if (is.function(where)) where else function(x) where
  method <- match.arg(method)

  impute_fun <- switch(method,
                       'ignore' = identity,
                       'replace' = function(x) replace(x, TRUE, replace_with),
                       'mean' = function(x) mean(x, na.rm = TRUE),
                       'median' = function(x) stats::median(x, na.rm = TRUE),
                       'mode' = get_mode,
                       'min' = function(x) min(x, na.rm = TRUE),
                       'max' = function(x) max(x, na.rm = TRUE),
                       'sum' = function(x) sum(x, na.rm = TRUE))

  group_vars <- if (method %in% c('ignore', 'replace')) NULL else
    c('prodcode', if (group[1] == 'population') NULL else
      match.arg(group, colnames(data), several.ok = TRUE))

  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::mutate("{{variable}}" := ifelse(no   = {{ variable }},
                                           test = rep_len(where_fn({{ variable }}), length({{ variable }})),
                                           yes  = impute_fun({{ variable }}))) %>%
    dplyr::ungroup()
}

#' Find implausible entries

#' Replace implausible or missing prescription quantities
#'
#' @inheritParams impute
#'
#' @examples
#' impute_qty(example_therapy, 'mean')
#'
#' @return A data frame of the same structure as \code{data}, with values imputed
#'
#' @importFrom rlang .data
#' @export
impute_qty <- function(data,
                       method,
                       where = is.na,
                       group = 'population',
                       ...) {
  impute(data, .data$qty, method, where, group, ...)
}

#' Replace implausible or missing numerical daily doses (NDD)
#'
#' @inheritParams impute
#'
#' @examples
#' impute_ndd(example_therapy, 'mean')
#'
#' @return A data frame of the same structure as \code{data}, with values imputed
#'
#' @importFrom rlang .data
#' @export
impute_ndd <- function(data,
                       method,
                       where = is.na,
                       group = 'population',
                       ...) {
  impute(data, .data$ndd, method, where, group, ...)
}


#' Replace missing or implausible prescription durations
#'
#' Instead of replacing missing stop dates, we impute the durations and then
#' infer the stop dates from there.
#'
#' We can fix clashing start dates by setting \code{group} to \code{start_date}
#' and \code{patid}, i.e. average over groups with more than one member;
#' any metric should return the original values if the group size is one.
#'
#' @inheritParams impute
#'
#' @examples
#' example_duration <- transform(example_therapy, duration = qty / ndd)
#' impute_duration(example_duration, method = 'mean', group = 'patid')
#'
#' @return A data frame of the same structure as \code{data}, with values imputed
#'
#' @importFrom rlang .data
#' @export
impute_duration <- function(data,
                            method,
                            where = is.na,
                            group = c('patid', 'start_date'),
                            ...) {
  impute(data, .data$duration, method, where, group, ...)
}

#' Clean implausibly-long prescription durations
#'
#' Given a prescription length limit, truncate any prescriptions that appear to
#' be longer than this, or mark them as missing.
#'
#' The method 'truncate' causes any duration longer than \code{max_months} to
#' be replaced with the value of \code{max_months} (albeit converted to days).
#' The method 'remove' causes such durations to be replaced with \code{NA}.
#' There is no explicit 'ignore' method, but if you want to 'do nothing', simply
#' set \code{max_months} to an arbitrarily high number.
#' By default, the maximum is infinite, so nothing should happen.
#' (Of course, you could also just \emph{not} run the function...)
#'
#' @note Currently the variable name is hard-coded as 'duration', but in
#' principle this could be parametrised for datasets where the column has a
#' different name.
#'
#' @param data A data frame containing a column called \code{duration}
#' @param max_months The maximum plausible prescription length in months
#' @param method Either 'truncate' or 'remove'. See details
#'
#' @examples
#' long_presc <- data.frame(duration = c(100, 300, 400, 800))
#' clean_duration(long_presc, 6)
#' clean_duration(long_presc, 12, 'remove')
#'
#' @return A data frame of the same structure as the input, possibly with some elements of the \code{duration} column changed
#'
#' @importFrom rlang .data :=
#' @export
clean_duration <- function(data,
                           max_months = Inf,
                           method = c('truncate', 'remove')) {

  stopifnot(length(max_months) == 1 & is.numeric(max_months))
  method <- match.arg(method)
  max_days <- round(max_months * 365 / 12)
  impute(data, .data$duration,
         method = 'replace',
         where = function(x) x > max_days,
         replace_with = switch(method, truncate = max_days, remove = NA_real_))
}
