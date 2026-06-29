# Main function -----------------------------------------------------------

  #' Run the Hibbing 2023 summary scheme
  #'
  #' @inheritParams ee_summary
  #' @param verbose logical. Print updates to console?
  #'
  #' @return A data frame whose contents are prepared according to the
  #'   indicated scheme
  #' @export
  #'
  #' @examples
  #'
  #' f <- system.file("extdata/TAS1H30182785_2019-09-17.gt3x", package = "read.gt3x")
  #'
  #' ## Read acceleration data
  #'   accel <- ee_file(f)
  #'
  #' ## Generate some bogus count data
  #'   cts <- data.frame(
  #'     Timestamp = accel$Timestamp,
  #'     Axis1 = 0, Axis2 = 0, Axis3 = 0,
  #'     valid_status = "Non-Wear"
  #'   )
  #'
  #' ## Returns NA with a warning about no complete days in the file
  #'   suppressWarnings(ee_summary(
  #'     merge(accel, cts)
  #'   ))
  #'
  #' @name hibbing23-summary
  ee_summary_hibbing23 <- function(d, verbose = FALSE) {

    stopifnot(

      inherits(d, "data.frame"),

      exists("Timestamp", d),
      inherits(d$Timestamp, "POSIXt"),
      epoch_length(d) == 60,

      exists("valid_status", d),
      is.character(d$valid_status),
      all(unique(d$valid_status) %in% c("Awake-Wear", "Non-Wear", "Sleep"))

    )

    PAutilities::full_days(d) %>%
    prep_hibbing23(verbose) %>%
    to_day_hibbing23(verbose) %>%
    dplyr::relocate(Date, total_minutes)

  }


# Helper functions --------------------------------------------------------

  prep_hibbing23 <- function(d, verbose = FALSE) {

    # Setup

      if (verbose) cat("\n...Preparing to summarize")

      fix_names <-
        names(d) %>%
        intersect(.sum_names)

      fix_row <- d$valid_status %in% c("Sleep", "Non-Wear")

    # Work on sum variables

      d[ ,fix_names] %<>% sapply(
        function(x, fix_row) ifelse(fix_row, 0, x),
        fix_row = fix_row
      )

      d$is_Sleep <- ifelse(
        d$valid_status == "Sleep", 1, 0
      )

      d$is_NonWear <- ifelse(
        d$valid_status == "Non-Wear", 1, 0
      )

      fix_names %>%
      setdiff(c("is_Sleep", "is_NonWear")) %>%
      d[ ,.] %>%
      .[d$valid_status %in% c("Sleep", "Non-Wear"), ] %>%
      rowSums(.) %>%
      {. == 0} %>%
      all(.) %>%
      stopifnot(.)

    # Finish up

      d

  }

  to_day_hibbing23 <- function(d, verbose = FALSE) {

    vars <-
      names(d) %>%
      intersect(.sum_names)

    d %T>%
    {if (verbose) cat("\n...Collapsing to day-level summaries")} %>%
    dplyr::mutate(Timestamp = as.Date(Timestamp)) %>%
    {merge(
      run_summary_hibbing23(., vars, "total_", sum),
      run_summary_hibbing23(., vars[1], "length_", length)
    )} %>%
    stats::setNames(
      .,
      gsub(paste0("^length_", vars[1], "$"), "total_minutes", names(.))
    )

  }


# dplyr coding ------------------------------------------------------------

  run_summary_hibbing23 <- function(d, vars, tag = "", fun) {

      c("Timestamp", vars) %>%
      unique(.) %>%
      d[ ,.] %>%
      dplyr::group_by(dplyr::pick(dplyr::all_of("Timestamp"))) %>%
      dplyr::summarise(dplyr::across(
        .fns = fun,
        .names = "{tag}{.col}"
      ), .groups = "drop") %>%
      dplyr::rename(Date = "Timestamp") %>%
      data.frame(stringsAsFactors = FALSE)

  }
