
#' Set and get options related to how [sift()] runs.
#'
#' - `sift_limit` (Integer; default `25`)
#'     - How many matches should [sift()] print? This saves you from locking up R
#'       by accidentally printing a summary that is thousands of columns long.
#' - `sift_guessmax` (Integer; default `1000`)
#'     - Running summary statistics on very large dataframes (hundreds of columns,
#'       millions of rows) can take a long time. This option controls the point at
#'       which [sift()] decides that a dataframe has too many rows to use as-is,
#'       and starts randomly sampling from it instead.
#'     - For any dataframe with `nrow() <= guessmax`, the entirety
#'       of each column will be used for summary stats like "Missing %" and "Peek
#'       at unique values". Above this row count, `n = guessmax` elements of each
#'       column will be randomly sampled without replacement to make these stats,
#'       and a warning glyph will be shown alongside those stats to show that they
#'       were estimated.
#'     - Factor variables are never sampled; their levels are used in full.
#' - `sift_peeklength` (Integer; default `3000`)
#'     - When [sift()] creates a dictionary, it generates a "peek" that previews
#'       the unique values of each column. You are only shown a small part of that
#'       peek in query results, but the full peek is used to search through the
#'       dictionary. This option controls how long (in characters) this full peek
#'       is allowed to be. The practical maximum is around 30 thousand characters.
#'       The default of 3000 characters is about as long as a 1-page Word document
#'       at default settings.
#'
#' @param key (String) The name of an option.
#' @param val (Optional) A new value for the option, if you want to change it.
#'
#' @return The option's value. If invoked with no arguments (`options_sift()`),
#'     prints the status of all options to the console and returns `NULL`.
#' @export
#'
#' @examples
#' \donttest{
#' options_sift("sift_limit")  # Returns the option's current value
#' options_sift("sift_limit", 100)  # Change the value to something else.
#' options_sift("sift_limit", 25)  # Change it back.
#'
#' # Options set in this function are set in R's options() interface
#' options("sift_limit")
#' getOption("sift_limit")
#' }
#'
#' @md
options_sift <- function(key = c("sift_limit", "sift_guessmax", "sift_peeklength"), val = NULL) {
    default_setting <- list(
        sift_limit      = 25,
        sift_guessmax   = 1000,
        sift_peeklength = 2000
    )

    if (identical(key, eval(formals(options_sift)$key))) {
        # options_sift() was called with no arguments. Print something helpful.
        curr_options <- paste(cli::col_green(names(default_setting)), default_setting, sep = " = ")
        names(curr_options) <- rep(" ", length(curr_options))

        cli::cli_text("Sift's options are currently:")
        cli::cat_line()
        cli::cli_bullets(curr_options)
        cli::cat_line()
        cli::cli_text("See ", cli::col_yellow("?options_sift"), " for what these control.")

        # I choose not to return curr_options here because I do not want to
        # create anoter way of getting options. All option accesses in sift
        # should happen through options_sift("name of option").
        return(invisible(NULL))
    }

    if ((key %in% names(default_setting)) == FALSE) {
        cli::cli_abort(c(
            "x" = msg_sift("not option", 1, key),
            "i" = msg_sift("not option", 2, fold_or(names(default_setting)))
            ))
    }

    if (is.null(getOption(key))) {
        # Not set, give it the default setting.
        options(default_setting[key])
    }

    if (!is.null(val)) {
        # Set, but may need updating.

        # I am passing options in lists because it's how you get dynamically-named options.
        # Doing options(key = val) just makes a new option with name 'key'.

        opt <- list()
        opt[key] <- val

        options(opt[key])
    }

    return(getOption(key))
}
