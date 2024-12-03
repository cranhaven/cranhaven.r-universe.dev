

#' optparse_flag - define a command parameter as a trigger
#' @description
#' To be used with \code{optparse_parameters}. This function tells
#' the provided parameter is a trigger (logical - TRUE/FALSE).
#' When the trigger parameter is not provided in the command line,
#' the value is FALSE. Otherwise, it is TRUE.
#' @param help - The help string to display when --help is triggered
#' @param short - The shortcut fir this parameter. For example
#'   for a --output param, we could use
#'   \code{optparse_flag(short = "-o", ...)} to set the "-o" shortcut.
#' @param default - The default value this parameter will hold.
#' @return a list to give to \code{optparse_parameters} to build the
#'   whole command line parsing tool.
#' @seealso [optparse_parameters()]
#' @examples
#'
#' str(optparse_parameters(
#'   a_parameter = optparse_flag(),
#'   args = list("--a-parameter")
#' ))
#'
#' @author L.Pavot
#' @export
optparse_flag <- function(
  help = "No documentation yet.",
  short = NULL,
  default = FALSE
) {
  return(list(
    opt_str = short,
    action = "store_true",
    help = help,
    default = default
  ))
}

#' optparse_numeric - define a command parameter as an numeric
#' @description
#' To be used with \code{optparse_parameters}. This function tells
#' the provided parameter is to be parsed as an numeric.
#' @inheritParams optparse_flag
#' @inherit optparse_flag seealso
#' @examples
#'
#' str(optparse_parameters(
#'   a_parameter = optparse_numeric(),
#'   args = list("--a-parameter", "42.72")
#' ))
#'
#' @author L.Pavot
#' @export
optparse_numeric <- function(
  help = "No documentation yet.",
  short = NULL,
  default = 0
) {
  return(list(
    opt_str = short,
    type = "numeric",
    action = "store",
    help = help,
    default = default,
    metavar = "numeric"
  ))
}

#' optparse_integer - define a command parameter as an integer
#' @description
#' To be used with \code{optparse_parameters}. This function tells
#' the provided parameter is to be parsed as an integer.
#' @inheritParams optparse_flag
#' @inherit optparse_flag seealso
#' @examples
#'
#' str(optparse_parameters(
#'   a_parameter = optparse_integer(),
#'   args = list("--a-parameter", "42")
#' ))
#'
#' @author L.Pavot
#' @export
optparse_integer <- function(
  help = "No documentation yet.",
  short = NULL,
  default = 0
) {
  return(list(
    opt_str = short,
    type = "integer",
    action = "store",
    help = help,
    default = default,
    metavar = "number"
  ))
}

#' optparse_character - define a command parameter as string
#' @description
#' To be used with \code{optparse_parameters}. This function tells
#' the provided parameter is to be parsed as a single string.
#' @inheritParams optparse_flag
#' @inherit optparse_flag seealso
#' @examples
#'
#' str(optparse_parameters(
#'   a_parameter = optparse_character(),
#'   args = list("--a-parameter", "42")
#' ))
#'
#' @author L.Pavot
#' @export
optparse_character <- function(
  help = "No documentation yet.",
  short = NULL,
  default = 0
) {
  return(list(
    opt_str = short,
    type = "character",
    action = "store",
    help = help,
    default = default,
    metavar = "character"
  ))
}

#' optparse_list - define a command parameter as a list of objects
#' @description
#' To be used with \code{optparse_parameters}. This function tells
#' the provided parameter is to be parsed as a list of objects.
#' The \code{of} parameter tells what type are elements of the list.
#' Each element must be separated by a separator. This separator must
#' be the value given in the \code{sep} parameter
#' @inheritParams optparse_flag
#' @inherit optparse_flag seealso
#' @param of - This type of elements of this list
#' @param sep - This character to split on, to get the list
#' @param truevalues - A \code{character} vector of different string
#'   values to translate it as \code{TRUE} value.
#' @examples
#'
#' str(optparse_parameters(
#'   a_parameter = optparse_list(of="numeric"),
#'   b_parameter = optparse_list(of="integer"),
#'   c_parameter = optparse_list(of="logical"),
#'   args = list(
#'     "--a-parameter", "42.7,72.5",
#'     "--b-parameter", "42.7,72.5",
#'     "--c-parameter", "TRUE,FALSE,FALSE,TRUE"
#'   )
#' ))
#'
#' @author L.Pavot
#' @export
optparse_list <- function(
  help = "No documentation yet.",
  short = NULL,
  default = "",
  of = "character",
  sep = ",",
  truevalues = c("TRUE", "true", "1", "t", "T")
) {
  return(list(
    opt_str = short,
    type = "character",
    action = "callback",
    help = help,
    default = default,
    metavar = of,
    callback = function(option_parser, param, value, ...) {
      if (of == "character") {
        transfo <- as.character
      } else if (of == "numeric") {
        transfo <- as.numeric
      } else if (of == "integer") {
        transfo <- as.integer
      } else if (of == "logical") {
        transfo <- function(x) {
          x %in% truevalues
        }
      } else {
        stopf("Unknown type: %s. Cannot transform without a convertor", of)
      }
      return(lapply(strsplit(value[[1]], sep, fixed = TRUE)[[1]], transfo))
    }
  ))
}
#' optparse_parameters - parse easily the command line parameters
#'
#' @description
#' This function is made to be used with the functions optparse_flag,
#' optparse_numeric, optparse_integer, optparse_character and/or
#' optparse_list
#'
#' \code{optparse_parameters} parses arguments based on its parameters.
#'
#' You just have to call \code{optparse_parameters} with named arguments.
#' Each parameter is the result of either optparse_flag, optparse_numeric,
#' optparse_integer, optparse_character or optparse_list
#'
#' @param fix_hyphens logical - whether to turn underscores into hyphens or not
#' @param fix_dots logical - whether to turn points into hyphens or not
#' @param add_trailing_hyphens logical - whether to add trailing hyphens
#'   if missing
#' @param args \code{list} - The parameters from the \code{commandArgs} function
#' @param no_optparse logical - INTERNAL Tells whether to use optparse library or not
#' @param ... parameters definition. Must be the result of either those functions:
#'   - optparse_flag
#'   - optparse_numeric
#'   - optparse_integer
#'   - optparse_character
#'   - optparse_list
#' @examples
#'
#' args <- optparse_parameters(
#'   a_integer = optparse_integer(),
#'   a_float = optparse_numeric(),
#'   a_boolean = optparse_flag(),
#'   a_character = optparse_character(),
#'   a_list = optparse_list(of = "numeric"),
#'   a_char_list = optparse_list(of = "character"),
#'   a_int_list = optparse_list(of = "integer"),
#'   args = list(
#'     "--a-integer",
#'     "42",
#'     "--a-float",
#'     "3.14",
#'     "--a-boolean",
#'     "FALSE",
#'     "--a-character",
#'     "FALSE",
#'     "--a-list",
#'     "1.5,2,3",
#'     "--a-char-list",
#'     "1.5,2,3",
#'     "--a-int-list",
#'     "1.5,2,3"
#'   )
#' )
#'
#' str(args)
#'
#' @author L.Pavot
#' @export
optparse_parameters <- function(
  fix_hyphens = TRUE,
  fix_dots = TRUE,
  add_trailing_hyphens = TRUE,
  args = NULL,
  no_optparse = FALSE,
  ...
) {
  if (
    !suppressWarnings(requireNamespace("optparse", quietly = TRUE))
    || no_optparse
  ) {
    stopaste(
      "To uses `optparse_parameters`, you need to install the",
      "\"optparse\" package or to add it to your tool's dependencies"
    )
  }
  optparse <- loadNamespace("optparse")
  parser <- optparse$OptionParser()
  param_definition <- list(...)
  for (long in names(param_definition)) {
    original <- long
    definition <- param_definition[[long]]
    if (fix_hyphens) {
      long <- gsub("_", "-", long, fixed = TRUE)
    }
    if (fix_dots) {
      long <- gsub(".", "-", long, fixed = TRUE)
    }
    if (add_trailing_hyphens) {
      long <- paste0("--", long)
    }
    definition$object <- parser
    definition$opt_str <- c(definition$opt_str, long)
    definition$dest <- original
    definition$help <- definition$help
    definition$callback <- definition$callback
    parser <- do.call(optparse$add_option, definition)
  }
  if (!is.null(args)) {
    result <- optparse$parse_args(parser, args = args)
  } else {
    result <- optparse$parse_args(parser)
  }
  return(result)
}
