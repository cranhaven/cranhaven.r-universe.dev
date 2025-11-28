assert_is_single_h_desc <- function(x) {
  if (as.integer(R.Version()[["major"]]) < 4L) {
    stopifnot(is_single_hdesc(x))
  } else {
    stopifnot(
      `x must be a single Hmisc::describe() object` =
        is_single_hdesc(x)
    )
  }
  invisible(TRUE)
}


assert_is_h_desc <- function(x) {
  if (as.integer(R.Version()[["major"]]) < 4L) {
    stopifnot(is_hdesc(x))
  } else {
    stopifnot(
      `x must be an Hmisc::describe() object (or one of its elements)` =
        is_hdesc(x)
    )
  }
  invisible(TRUE)
}


is_val_freq_list <- function(x) {
  length(x) &&
    is.list(x) &&
    all(names(x) %in% c("value", "frequency"))
}


is_proper_matrix <- function(tab) {
  is.matrix(tab) && nrow(tab) >= 2L && ncol(tab) >= 2L
}

empty_h_test <- function() {
  ui_warn("tab is not a proper matrix. No test is done")

  list(
    # values (mandatory)
    P = NA,
    stat = NA,
    df = NA,

    # names (mandatory)
    testname = "notestname",
    statname = "nostatname",
    namefun = "nonamefun",

    # special labels (optional)
    note = "tab is not a proper matrix. No test is done"
  )
}


has_multiple_proper_groups <- function(n_grouup, n_subject) {

}

fake_h_group_test <- function() {
  ui_warn("Only one group with data, no paired test is done")
  # `return()` exits from the function here!
  list(
    # values (mandatory)
    P = stats::setNames(1L, "P"),
    stat = stats::setNames(Inf, "XXX"),
    df = stats::setNames(0L, "df"),

    # names (mandatory)
    testname = "notestname",
    statname = "nostatname",
    namefun = "nonamefun",

    # special labels (optional)
    note = "Only one group with data, no paired test is done."
  )
}
