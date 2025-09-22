summary <- R6::R6Class("summary",
  public = list(
    initialize = function(filter, failed_ions, passed_ions) {
      stopifnot(any(class(filter) == "character"))

      if (is.null(failed_ions)) {
        private$filter <- filter
        private$failed_ions <- c()
        private$passed_ions <- passed_ions
      } else {
        stopifnot(any(class(failed_ions) == c("numeric", "character")))
        stopifnot(any(class(passed_ions) == c("numeric", "character")))

        private$filter <- filter
        private$failed_ions <- failed_ions
        private$passed_ions <- passed_ions
      }
    },
    summarize = function(x) {
      l <- length(private$failed_ions)
      f <- private$filter
      r <- length(private$passed_ions)

      cli::cli_alert_success(c("{l} ions failed the {f} filter, ",
                               "{r} ions remain."))
    },
    get_failed_ions = function() {
      return(private$failed_ions)
    },
    get_passed_ions = function() {
      return(private$passed_ions)
    },
    get_filter = function() {
      return(private$filter)
    }
  ),
  private = list(
    filter = NA,
    failed_ions = NA,
    passed_ions = NA
  )
)
