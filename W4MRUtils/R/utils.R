

#' stopf - to stop and format message
#' @description
#' stopf calls sprintf of its parameters to build the error message
#' and stops with the given message
#' @inheritDotParams base::sprintf
#' @return NULL
#' @examples
#'
#' tryCatch({
#'   file <- "/tmp/test"
#'   stopf("Error in %s file.", file)
#' }, error = function(error) {
#'   print(error)
#' })
#'
#' @author L.Pavot
#' @export
stopf <- function(...) {
  stop(sprintf(...))
}


#' stopaste - to paste string to a message and stop
#' @description
#' stopaste calls paste of its parameters to build the error message
#' and stops with the given message
#' @inheritDotParams base::paste
#' @return NULL
#' @examples
#'
#' tryCatch({
#'   file <- "/tmp/test"
#'   stopaste("Error in file: ", file)
#' }, error = function(error) {
#'   print(error)
#' })
#'
#' @author L.Pavot
#' @export
stopaste <- function(...) {
  stop(paste(...))
}

#' stopaste0 - to paste string to a message and stop
#' @description
#' stopaste calls paste0 of its parameters to build the error message
#' and stops with the given message
#' @inheritDotParams base::paste0
#' @return NULL
#' @examples
#'
#' tryCatch({
#'   file <- "/tmp/test"
#'   stopaste0("Error in file: ", file)
#' }, error = function(error) {
#'   print(error)
#' })
#'
#' @author L.Pavot
#' @export
stopaste0 <- function(...) {
  stop(paste0(...))
}

#' printf - to format a string and print it
#' @description
#' printf calls sprintf of its parameters to build the error message
#' and prints with the given message
#' @inheritDotParams base::sprintf
#' @return NULL
#' @examples
#'
#' file <- "/tmp/test"
#' printf("Error in file: ", file)
#'
#' @author L.Pavot
#' @export
printf <- function(...) {
  print(sprintf(...))
}

#' printfp - to paste, format and print a string
#' @description
#' printfp calls paste and sprintf of its parameters to build the error message
#' and prints with the given message
#' @inheritDotParams base::paste
#' @param x a list of format string to concatenate before using sprintf on it.
#' @return NULL
#' @examples
#'
#' file <- "/tmp/test"
#' printfp(
#'   list(
#'     "Very log error message that needs to be cut on multiple lines,",
#'     "and paste back together, but there are formatings like",
#'     "%%s for example, that provides a placeholder for parameters.",
#'     "Here %%s value is %s."
#'   ), file
#' )
#'
#' @author L.Pavot
#' @export
printfp <- function(x, ...) {
  print(sprintf(do.call(paste, x), ...))
}

#' printp - to format a string and print it
#' @description
#' printp calls sprintf of its parameters to build the error message
#' and prints with the given message
#' @inheritDotParams base::sprintf
#' @return NULL
#' @examples
#'
#' file <- "/tmp/test"
#' printp("Error in file: ", file)
#'
#' @author L.Pavot
#' @export
printp <- function(...) {
  print(paste(...))
}

#' collapse - to paste strings with collapse = ""
#' @description
#' collapse does exactly when paste does, but default collapse = ""
#' @param sep set the separator. Deafult is ""
#' @param ... passed to [base::paste0()]
#' @return NULL
#' @examples
#'
#' collapse(list("a message ", "in multiple", "parts"))
#'
#' @author L.Pavot
#' @export
collapse <- function(..., sep = "") {
  paste0(..., collapse = sep, sep = "")
}

#' collapse_lines - to paste strings with collapse = "\\n"
#' @description
#' collapse_lines() does exactly when paste does, but default collapse = "\\n"
#' @param sep set the separator. Deafult is "\\n"
#' @param ... passed to [base::paste0()]
#' @return NULL
#' @examples
#'
#' collapse_lines(list("a message ", "in multiple", "parts"))
#'
#' @author L.Pavot
#' @export
collapse_lines <- function(..., sep = "\n") {
  paste0(..., collapse = sep, sep = "")
}

#' check_param_type_n_length - to check parameters
#'
#' @description
#' Use this function to validate parameters.
#' You're never assured that provided parameters from users are the right
#' type, or length. This may be the case with your own code as well,
#' if you have undetected bugs in your code.
#'
#' This function helps prevent unpredictable behaviour coming from
#' bad parameters.
#'
#' It checks the size of vectors, and the type of values.
#' If the parameter is not the good type or length, the program stops
#' with an explanatory error.
#'
#' @param value The parameter to test.
#' @param expected_type The \code{chararcter} vector of the
#'  kind: \code{"character"}, \code{"integer"}, \code{"logical"}, ...
#' @param expected_size The expected \code{size} of the vector.
#'  Usualy, \code{1}.
#' @param nth This parameter is used in the error message generation.
#'  Provide a character vector like \code{"first"}, \code{"second"},
#'  \code{"1st"}, \code{"2nd"}, ... this must be the number of the
#'  parameter if the function.
#' @param func_name By default, the function name is guessed from the
#'  stack. But if you want to change it, or if it is not the right
#'  function name in error messages, set the right one here.
#' @param param_name Like \code{func_name}, by default the param name is
#'  guessed. But if you want to change it, or if it is not the right
#'  parameter name in error messages, set the right one here.
#' @param or_more When we check the parameter's length, if
#'  \code{or_more} is \code{TRUE} and the value is bigger than
#'  \code{expected_size}, then, the length check does not occur
#' @param or_null When we check the parameter's type, if \code{or_null}
#'  is \code{TRUE} and the value is \code{NULL}, then, the type check
#'  does not occur
#' @param nframe The number of function calls between this function
#'  and the function where the value to test is a parameter.
#'  for example, if a user calls function A, which calls
#'  check_param_* directly, then nframe must be 1 because it
#'  is a direct call.
#'  But, if the user has called function A, and function A calls
#'  function B, and check_param_*, is called in function B,
#'  then, for check_param_* to understant it is a parameter
#'  comming from function A (and not from function B), we have to tell
#'  check_param_* that nframe is 2.
#'  If the function name is not the right name, it may be because of that.
#'  So don't fear testing different values for nframes.
#' @return NULL
#'
#' @examples
#'
#' ## here is a simple utility function we will use in this example.
#' ## It is not important
#' show_last_error <- function(error) {
#'   dump.frames()
#'   message(base::attr(last.dump, "error.message"))
#' }
#'
#' ## The example really starts here
#' ## we have a simple function like this:
#' custom_message <- function(text) {
#'   message(sprintf("Message: %s", text))
#' }
#'
#' ## this function needs to have a character vector as first
#' ## parameter.
#' ## So, to validate the parameter, we could write:
#' custom_message <- function(text) {
#'   check_parameter_type(text, "character")
#'   message(base::sprintf("Message: %s", text))
#' }
#' tryCatch(custom_message(42), error = show_last_error)
#'
#'
#' ## this function needs to have a vector of length 1.
#' ## So, to validate the parameter, we could write:
#' custom_message <- function(text) {
#'   check_parameter_type(text, "character")
#'   check_parameter_length(text, 1)
#'   message(base::sprintf("Message: %s", text))
#' }
#' tryCatch(custom_message(c("uwu", "owo")), error = show_last_error)
#'
#'
#' ## Or, to be more concise:
#' custom_message <- function(text) {
#'   check_param_type_n_length(text, "character", 1)
#'   message(base::sprintf("Message: %s", text))
#' }
#' tryCatch(custom_message(c("uwu", "owo")), error = show_last_error)
#' tryCatch(custom_message(42), error = show_last_error)
#'
#'
#' ## Let's say the text can be 1 or more elements, and can be null.
#' custom_message <- function(text) {
#'   check_param_type_n_length(
#'     text,
#'     expected_type = "character",
#'     or_null = TRUE,
#'     expected_size = 1,
#'     or_more = TRUE
#'   )
#'   message(paste0(base::sprintf("Message: %s", text), collapse = "\n"))
#' }
#' tryCatch(custom_message(c(42, 43)), error = show_last_error)
#' tryCatch(custom_message(NULL), error = show_last_error)
#' ## no error, because or_null is TRUE
#' tryCatch(custom_message(character(0)), error = show_last_error)
#' tryCatch(custom_message(c("uwu", ":3")), error = show_last_error)
#' ## no error, because or_more is TRUE
#'
#' ## With a function that has a lot of parameters, it may be usefull to
#' ## provide the parameter's number. And, because it becomes very long
#' ## to test all those parameters, we will use shortcuts functions
#' write_msg <- function(
#'   text,
#'   font = "owo",
#'   font_size = 16,
#'   italic = FALSE,
#'   bold = FALSE
#' ) {
#'   check_one_character(text, nth = "1st")
#'   check_one_character(font, nth = "2nd")
#'   check_one_numeric(font_size, nth = "3rd")
#'   check_one_logical(italic, nth = "before last")
#'   check_one_logical(bold, nth = "last")
#'   message(paste0(base::sprintf("Message: %s", text), collapse = "\n"))
#' }
#' tryCatch(write_msg(text = 42, "font", 16), error = show_last_error)
#' tryCatch(write_msg("uwu", font = 1, 16), error = show_last_error)
#' tryCatch(write_msg("uwu", font_size = "16"), error = show_last_error)
#' tryCatch(write_msg("uwu", italic = "FALSE"), error = show_last_error)
#' tryCatch(write_msg("uwu", bold = "FALSE"), error = show_last_error)
#'
#' @seealso [check_parameter_type],[check_parameter_length]
#' @seealso [check_one_integer],[check_one_logical],[check_one_numeric]
#' @seealso [check_one_complex],[check_one_character]
#'
#' @author L.Pavot
#' @export
check_param_type_n_length <- function(
  value,
  expected_type,
  expected_size = 1,
  nth = NULL,
  func_name = NULL,
  param_name = NULL,
  or_more = FALSE,
  or_null = FALSE,
  nframe = 1
) {
  if (or_null && is.null(value)) {
    return(invisible(NULL))
  }
  if (is.null(param_name)) {
    param_name <- deparse(substitute(value))
  }
  if (is.null(func_name)) {
    last_frame <- sys.calls()[[max(sys.nframe() - nframe, 1)]][[1]]
    func_name <- deparse(last_frame)
  }
  check_parameter_type(
    value,
    expected_type,
    nth = nth,
    func_name = func_name,
    param_name = param_name,
    or_null = or_null
  )
  check_parameter_length(
    value,
    expected_size,
    nth = nth,
    func_name = func_name,
    param_name = param_name,
    or_more = or_more
  )
  return(invisible(NULL))
}

#' check_parameter_type - validate parameter's type
#'
#' @inherit check_param_type_n_length
#'
#' @author L.Pavot
#' @export
check_parameter_type <- function(
  value,
  expected_type,
  nth = NULL,
  func_name = NULL,
  param_name = NULL,
  or_null = FALSE,
  nframe = 1
) {
  if (is.null(param_name)) {
    param_name <- deparse(substitute(value))
  }
  if (is.null(func_name)) {
    last_frame <- sys.calls()[[max(sys.nframe() - nframe, 1)]][[1]]
    func_name <- deparse(last_frame)
  }
  if (! is.null(nth) && is.character(nth) && length(nth) == 1) {
    nth <- sprintf(" (%s)", nth)
  } else {
    nth <- ""
  }
  if (!is(value, expected_type) && !(or_null && is.null(value))) {
    stopf(
      "The '%s'%s parameter for %s must be a %s%s, not a %s.",
      param_name,
      nth,
      func_name,
      expected_type,
      ifelse(or_null, " or NULL", ""),
      paste(class(value), collapse = "/")
    )
  }
  return(invisible(NULL))
}

#' check_parameter_length - validate parameter's length
#'
#' @inherit check_param_type_n_length
#'
#' @author L.Pavot
#' @export
check_parameter_length <- function(
  value,
  expected_size,
  nth = NULL,
  func_name = NULL,
  param_name = NULL,
  or_more = FALSE,
  nframe = 1
) {
  if (is.null(param_name)) {
    param_name <- deparse(substitute(value))
  }
  if (is.null(func_name)) {
    last_frame <- sys.calls()[[max(sys.nframe() - nframe, 1)]][[1]]
    func_name <- deparse(last_frame)
  }
  if (! is.null(nth) && is.character(nth) && length(nth) == 1) {
    nth <- sprintf(" (%s)", nth)
  } else {
    nth <- ""
  }
  if (
    length(value) != expected_size
    && !(or_more && length(value) >= expected_size)
  ) {
    stopf(
      "The '%s'%s parameter for %s must be %s element long%s, not %s.",
      param_name, nth, func_name, expected_size,
      ifelse(or_more, " or more", ""),
      length(value)
    )
  }
  return(invisible(NULL))
}


## All of those function have the same behavior, but with different
## parameters. Moreover, all of those function needs to do
## a `deparse(substitute(value))`, before any other call.
## Duplicating functions would be a hassle, and a mess to maintain.
## So, I have generated all the functions needed with a for-loop, which
## prevented me from writing multiple times the same code.
## The env creation and assignment is necessary to force the .type__
## value to the current one.
## The documentation still needs to be duplicated.

#' @name check_one_character
#' @title check_one_character
#' @inherit check_param_type_n_length
#' @export
NULL
#' @name check_one_integer
#' @title check_one_integer
#' @inherit check_param_type_n_length
#' @export
NULL
#' @name check_one_logical
#' @title check_one_logical
#' @inherit check_param_type_n_length
#' @export
NULL
#' @name check_one_numeric
#' @title check_one_numeric
#' @inherit check_param_type_n_length
#' @export
NULL
#' @name check_one_complex
#' @title check_one_complex
#' @inherit check_param_type_n_length
#' @export
NULL
for (.type__ in c(
  "character",
  "integer",
  "logical",
  "numeric",
  "complex"
)) {
  env <- new.env()
  env$.type__ <- .type__
  .x__ <- function(value, ...) {
    args <- list(
      value = value,
      expected_type = .type__,
      expected_size = 1,
      ...
    )
    if (is.null(args$nframe)) {
      ## one for each:
      ##  - check_one_*
      ##  - check_param_type_n_length
      ## and one more for the do.call, just few lines later
      ## So, 3 frames
      args$nframe <- 3
    }
    if (is.null(args$param_name)) {
      args$param_name <- deparse(substitute(value))
    }
    return(do.call(check_param_type_n_length, args))
  }
  environment(.x__) <- env
  assign(sprintf("check_one_%s", .type__), .x__)
}

rm(.x__)
