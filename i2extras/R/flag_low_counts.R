#' Flag low counts and set them to NAs
#'
#' Low counts may be genuine, but they can also reflect actually missing data or
#' strong under-reporting. This function aims to detect the latter by flagging
#' any count below a certain threshold, expressed as a fraction of the median
#' count. Setting low values to NAs can be useful to help fitting temporal
#' trends to the data, as zeros / low counts can throw off some models
#' (e.g. Negative Binomial GLMs).
#'
#' @author Tim Taylor and Thibaut Jombart
#'
#' @md
#'
#' @param x An [incidence2::incidence] object.
#'
#' @param counts A tidyselect compliant indication of the counts to be used.
#'
#' @param threshold A numeric multiplier of the median count to be used as
#'   threshold. Defaults to 0.001, in which case any count strictly lower than
#'   0.1% of the mean count is flagged as low count.
#'
#' @param set_missing A `logical` indicating if the low counts identified should
#'   be replaced with NAs (`TRUE`, default). If `FALSE`, new logical columns
#'   with the `flag_low` suffix will be added, indicating which entries are
#'   below the threshold.
#'
#' @return An [incidence2::incidence] object.
#'
#' @export
#'
#' @examples
#'
#' if (requireNamespace("outbreaks", quietly = TRUE) &&
#'     requireNamespace("incidence2", quietly = TRUE)) {
#'   data(covid19_england_nhscalls_2020, package = "outbreaks")
#'   dat <- covid19_england_nhscalls_2020
#'   i <- incidence(dat, "date", interval = "isoweek", counts = "count")
#'   plot(i)
#'   plot(flag_low_counts(i, threshold = 0.1))
#'   plot(flag_low_counts(i, threshold = 1), title = "removing counts below the median")
#' }
flag_low_counts <- function(x, counts = NULL, threshold = 0.001, set_missing = TRUE) {

    ## checks
    if (!inherits(x, "incidence2")) {
        stop(sprintf("`%s` is not an incidence object", deparse(substitute(x))))
    }

    ## snapshot original attributes
    original_attributes <- attributes(x)

    ## if no count is given use the current counts
    counts <- rlang::enquo(counts)
    if (!rlang::quo_is_null(counts)) {
        idx <- tidyselect::eval_select(counts, x, allow_rename = FALSE)
        counts <- names(x)[idx]
    } else {
        counts <- NULL
    }
    if (is.null(counts)) {
        counts <- get_count_value_name(x)
    }

    ## get group and date names
    group_names <- get_group_names(x)

    ## The output can take two forms depending on `set_missing`:
    ## * TRUE: counts are modified so that values below the threshold are set to NA
    ## * FALSE: counts are not modified, but new logical variables with a "flag_low"
    ## suffix are generated, with TRUE wherever values are below the threshold


    below_thres <- function(x) {
        x < round(threshold * mean(x, na.rm = TRUE))
    }

    out <- x
    if (!is.null(group_names)) {
        out <- grouped_df(out, group_names)
    }

    if (set_missing) {
        out <- mutate(
            out,
            across(
                {{counts}},
                function(x) if_else(
                    below_thres(x),
                    NA_counts_(x),
                    x
                )
            )
        )
    } else {
        new_var_names <- paste(counts, "flag_low", sep = "_")
        out <- mutate(
            out,
            across(
                {{counts}},
                below_thres,
                .names = new_var_names
            )
        )
    }

    ## restore attributes
    new_names <- names(out)
    attributes(out) <- original_attributes
    names(out) <- new_names

    out
}
