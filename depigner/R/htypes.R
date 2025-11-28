#' Checks for describe objects
#'
#' These two function are useful to test if an object is of class
#' [Hmisc][Hmisc::describe].
#'
#' @details In `Hmisc` both "single" `describe` objects and lists
#' of them are of class `describe`. In particular, even if
#' `Hmisc::describe()` results in a single variable description, it is
#'  directly the "single" `describe` object and not a list of them with
#'  only a single `describe` object included!
#'
#' `is_hdesc()` test for general inheritance.
#'
#' @param x an object to test if it is of class `describe`.
#'
#' @return (lgl) is `x` (a single element or a general) `describe`
#'   object?
#'
#' @export
#'
#' @seealso [describe][Hmisc::describe]
#' @seealso [is_hcat], [is_hcon], [htype], [htypes]
#'
#' @examples
#' \donttest{
#'   library(Hmisc)
#'   desc <- describe(mtcars)
#'
#'   is_hdesc(desc) # TRUE
#'   is_hdesc(desc[[1L]]) # TRUE
#' }
is_hdesc <- function(x) {
  class(x) == "describe"
}

#' @rdname is_hdesc
#' @details `is_single_hdesc()` test for single instance of a
#'   `describe` object.
#'
#' @export
#' @examples
#' \donttest{
#'   is_single_hdesc(desc) # FALSE
#'   is_single_hdesc(desc[[1L]]) # TRUE
#' }
is_single_hdesc <- function(x) {
  (class(x) == "describe") && (class(x[[1L]]) != "describe")
}






#' Type's checks accordingly to [Hmisc] package
#'
#' These functions decide and report if a single variable represented by
#' a single instance of an `Hmisc`'s [describe][Hmisc::describe] object
#' will considered a categorical variable or a continuous one.
#'
#' @details  A "single" object of `Hmisc`'s [describe][Hmisc::describe]
#'   class represents a variable. When you plot and object of class
#'   [describe][Hmisc::describe] the plot function decide if it is a
#'   continuous variable or a categorical one to plot it in the
#'   correspond plot. It is also possible that the variable is not
#'   considered in none of that categories, in which case it will not be
#'   plotted at all.
#'
#'   These functions have been produced/deduced from reading the
#'   source code of `Hmisc`'s [plot][Hmisc::describe]. In particular,
#'   from the definition of the (two distinct) functions `f` defined
#'   within it (one for categorical variables and the other for
#'   continuous variables). Both lead to a possible execution of
#'   `warning("no categorical variable found")` or `warning("no
#'   continuous variable found")`. I tried to keep the same
#'   names/code/logic that I found there.
#'
#' @param x an instance of the class [describe][Hmisc::describe], in the
#'   cases of "singular" functions (ie `is_*()` or `htype()`) it must
#'   be a single-variable [describe][Hmisc::describe] object.
#' @param n_unique (int, 10L) the minimum number of distinct values a
#'   numeric variable must have before plot.describe uses it in a
#'   continuous variable plot.
#'
#' @return (chr) `htype` returns one of "cat" (if `x` will be considered
#'   categorical), "con" (if `x` will be considered continuous), "none"
#'   (if `x` will be not considered categorical nor continuous, and
#'   hence it will be not plotted), or "both" (with a warning, if the
#'   variable will be considered both categorical and continuous. This
#'   would possibly never happen).
#'
#' @seealso [describe][Hmisc::describe],
#' @seealso [is_hdesc], [is_single_hdesc]
#' @seealso Gist with test and usage examples: https://bit.ly/htype-gist
#'
#' @export
#' @examples
#' \donttest{
#'   library(Hmisc)
#'   desc <- describe(mtcars)
#'
#'   htype(desc[["vs"]]) # "cat"
#'   htype(desc[["mpg"]]) # "con"
#'   htype(desc[["cyl"]]) # "none"
#' }
htype <- function(x, n_unique = 10L) {
  is_con <- is_hcon(x, n_unique = n_unique)
  is_cat <- is_hcat(x)

  htype <- c("cat", "con")[c(is_cat, is_con)]

  if (length(htype) == 0L) {
    return("none")
  }
  if (length(htype) == 1L) {
    return(htype)
  }
  if (length(htype) == 2L) {
    return({
      warning(
        "Strange behaviour: both cat and con! (this would never happen)"
      )
      "both"
    })
  }
}


#' @describeIn htype Report types for multi-variables objects
#' @return (chr) character vector of the types identified by [htype] for
#'   every variable represented in (each element of) `x`.
#' @export
htypes <- function(x, n_unique = 10L) {
  UseMethod("htypes", x)
}

#' @rdname htype
#' @method htypes describe
#' @export
#' @examples
#' \donttest{
#'   htypes(desc) # c(
#'   #   mpg = "con", cyl = "none", disp = "con",
#'   #   hp = "con", drat = "con", wt = "con", qsec = "con",
#'   #   vs = "cat", am = "cat", gear = "none",
#'   #   carb = "none"
#'   # )
#' }
htypes.describe <- function(x, n_unique = 10L) {
  assert_is_h_desc(x)

  if (is_single_hdesc(x)) {
    return(htype(x, n_unique = n_unique))
  }
  vapply(x, htype, FUN.VALUE = character(1L))
}

#' @rdname htype
#' @method htypes default
#' @export
#' @examples
#' \donttest{
#'   htypes(mtcars) # htypes(desc)
#'   htypes(letters) # "none"
#' }
htypes.default <- function(x, n_unique = 10L) {
  htypes(Hmisc::describe(x))
}

#' @describeIn htype Check if a single-instance of a
#'   [describe][Hmisc::describe] object is categorical.
#' @return (lgl) `is_hcat` returns TRUE if x will be considered
#'   categorical.
#'
#' @export
#' @examples
#' \donttest{
#'   is_hcat(desc[["vs"]]) # TRUE
#'   is_hcat(desc[["mpg"]]) # FALSE
#' }
is_hcat <- function(x) {
  assert_is_single_h_desc(x)

  s <- x[["counts"]]
  v <- x[["values"]]

  ok_counts <- ("Sum" %in% names(s)) && (as.numeric(s[["Sum"]]) > 0L)
  ok_values <- is_val_freq_list(v) &&
    length(v[["frequency"]]) &&
    is.character(v[["value"]]) &&
    (length(v[["value"]]) <= 20L)

  ok_counts || ok_values
}


#' @describeIn htype Check if a single-instance of a
#'   [describe][Hmisc::describe] object is continuous.
#'
#' @return (lgl) `is_hcon` returns TRUE if x will be considered
#'   continuous.
#' @export
#'
#' @examples
#' \donttest{
#'   is_hcon(desc[["vs"]]) # FALSE
#'   is_hcon(desc[["mpg"]]) # TRUE
#' }
is_hcon <- function(x, n_unique = 10L) {
  assert_is_single_h_desc(x)

  s <- x[["counts"]]
  v <- x[["values"]]

  is_val_freq_list(v) &&
    ("distinct" %in% names(s)) &&
    (as.numeric(s[["distinct"]]) >= n_unique) &&
    (is.numeric(v[["value"]]) || Hmisc::testDateTime(v[["value"]], "either"))
}
