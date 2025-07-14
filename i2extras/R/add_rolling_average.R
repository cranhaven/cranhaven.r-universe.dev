#' Add a rolling average
#'
# -------------------------------------------------------------------------
#' `add_rolling_average()` adds a rolling average to an `<incidence2>` object.
#' If multiple groupings or count variables are present then the average will be
#' calculated for each.
#'
# -------------------------------------------------------------------------
#' @param x `[incidence2]` object
#'
#' @param n `[integer]`
#'
#' How many date groupings to consider in each window?
#'
#' `double` vectors will be converted via `as.integer(n)`.
#'
#' @param complete_dates `[bool]`
#'
#' Should `incidence2::complete_dates()` be called on the data prior to adding
#' the rolling average.
#'
#' Defaults to TRUE.
#'
#' @param colname `[character]`
#'
#' The name of the column to contain the rolling average.
#'
#' @param ...
#'
#' Other arguments passed to `incidence2::complete_dates()`
#'
#' @inheritParams data.table::rollmean
#'
# -------------------------------------------------------------------------
#' @return
#'
#' The input object with an additional column for the rolling average.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#' \dontshow{withAutoprint(\{}
#'
#'   data(ebola_sim_clean, package = "outbreaks")
#'   dat <- ebola_sim_clean$linelist
#'   dat <- subset(dat, date_of_onset <= as.Date("2014-10-05"))
#'
#'   inci <- incidence2::incidence(
#'       dat,
#'       date_index = "date_of_onset",
#'       groups = "gender",
#'       interval = "isoweek"
#'   )
#'
#'   add_rolling_average(inci, n = 3L)
#'   inci2 <- incidence2::regroup(inci)
#'   add_rolling_average(inci2, n = 7L)
#' \dontshow{\})}
#' }
#'
# -------------------------------------------------------------------------
#' @export
add_rolling_average <- function(
        x,
        n = 3L,
        complete_dates = TRUE,
        align = c("right", "center"),
        colname = "rolling_average",
        ...
) {

    if (!inherits(x, "incidence2"))
        stopf("`%s` is not an <incidence2> object", deparse(substitute(x)))

    if (!is.numeric(n) || length(n) != 1L)
        stopf("`n` must be an integer of length 1.")

    .assert_bool(complete_dates)

    align <- match.arg(align)

    .assert_scalar_character(colname)
    if (colname %in% names(x))
        stopf("There is already a column named %s in `x`", dQuote(colname))

    row.names(x) <- NULL
    if (complete_dates)
        x <- complete_dates(x, ...)
    old <- attributes(x)
    n <- as.integer(n)
    date_var <- get_date_index_name(x)
    group_vars <- get_group_names(x)
    count_var <- get_count_variable_name(x)
    count_value <- get_count_value_name(x)
    out <- as.data.table(x)
    setorderv(out, date_var)
    out[, (colname) := frollmean(.SD, n = n, align = align, algo = "exact"), by = c(group_vars, count_var), .SDcols = count_value]
    setDF(out)
    old$names <- names(out)
    attributes(out) <- old
    out
}


