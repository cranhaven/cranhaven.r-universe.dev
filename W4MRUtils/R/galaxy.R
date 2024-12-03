#'
#' @importFrom utils capture.output
#' @importFrom utils str
#' @importFrom utils packageVersion
#' @importFrom utils sessionInfo
#' @importFrom rlang enexpr
#'
NULL


## taken from lib/galaxy/util/__init__.py:mapped_chars

mapped_chars__ <- list(
  `>` = "__gt__",
  `<` = "__lt__",
  `'` = "__sq__",
  '\"' = "__dq__",
  `[` = "__ob__",
  `]` = "__cb__",
  `{` = "__oc__",
  `}` = "__cc__",
  `@` = "__at__",
  `\n` = "__cn__",
  `\r` = "__cr__",
  `\t` = "__tc__",
  `#` = "__pd__"
)

mapped_chars_regex__ <- paste0(mapped_chars__, collapse = "|")


#' run_galaxy_function - automate running functions in galaxy
#' @description
#' This function executes the provided function as a galaxy processing
#' This provided function is expected to take two parameters:
#'   - args, a list of command line parameters
#'   - logger, the logger created for the tool
#'
#' @param tool_name - The name of the tool
#' @param func - The function to be run, after galaxy header is displayed
#' @param ... - Parameters propagated to [run_galaxy_processing]
#' @seealso [run_galaxy_processing]
#'
#' @author L.Pavot
#' @export
run_galaxy_function <- function(tool_name, func, ...) {
  check_param_type_n_length(tool_name, "character")
  check_param_type_n_length(func, "function")
  ## to prevent R CMD BUILD from telling logger does not exist
  logger <- NULL
  env <- new.env()
  env$func <- func
  return(run_galaxy_processing(
    tool_name,
    func(args, logger), ## nolint
    env = env,
    ...
  ))
}


#' run_galaxy_processing - automate running code in galaxy
#'
#' @description
#' run_galaxy_processing takes the tool's name, and the code to execute.
#' It detects galaxy-specific environement variable, and show headers
#' and footer if we are in a galaxy env.
#'
#' It will automatically convert command line parameters using
#' W4MRUtils::parse_args if args is not provided.
#'
#' Then, it unmangles galaxy parameters (galaxy params / values can be
#' mangled if they contains special characters)
#'
#' It creates a logger, and provide access to the `logger` and `args`
#' variables from withing the code to execute.
#'
#' Also, before executing the code, if `source_files` is set to some paths,
#' these paths will be source'd, so the code has access to functions
#' defined in these scripts.
#'
#' @param tool_name - Mandatory. The name of the tool to run.
#' @param code - Mandatory. The code to run the tool
#' @param tool_version - The version number of the tool to run.
#' @param unmangle_parameters - Whether or not to revert mangling from galaxy
#'   useful if galaxy produces strange command parameters.
#'   Not necessary, but produces more explicit outputs.
#' @param args - The result of [commandArgs] function, or from
#'   the [optparse_parameters] function.
#'   Can be NULL. In this case, uses [commandArgs] to get the args.
#' @param logger - You can provide a logger to use. If not provided,
#'    a logger will be created with the tool's name.
#' @param source_files - You may provide some paths to source before executing
#'    the provided code.
#' @param env - You may provide a environment object to execute the code
#'   within.
#' @param do_traceback - logical - tells whether to produce a traceback in
#'   case of error.
#' @examples
#'
#' write_r_file_with_content <- function(content) {
#'   "
#'   This function creates a temp R file. It writes the provided
#'   content in the R file. Then it returns the path of the script.
#'   "
#'   path <- tempfile(fileext = ".R")
#'   file.create(path)
#'   writeLines(content, con = path)
#'   return(path)
#' }
#' ## let's fake a galaxy env
#' Sys.setenv(GALAXY_SLOTS = 1)
#' ## let's says the tool has been launched with this command line
#' log_file <- tempfile()
#' file.create(log_file)
#' raw_args <- list(
#'   "--args",
#'   "--input", "in.csv",
#'   "--output", "out.csv",
#'   "--logs", log_file,
#'   "--one-float", "3.14",
#'   "--one-integer", "456",
#'   "--one-logical", "FALSE",
#'   "--some-floats", "1.5,2.4,3.3",
#'   "--some-characters", "test,truc,bidule",
#'   "--debug", "TRUE",
#'   "--verbose", "FALSE"
#' )
#'
#' ##
#' # example 1
#' ##
#'
#' my_r_script <- write_r_file_with_content('
#'   my_processing <- function(args, logger) {
#'     logger$info("The tool is running")
#'     logger$infof("Input file: %s.", args$input)
#'     logger$info("The tool ended.")
#'   }
#' ')
#'
#' W4MRUtils::run_galaxy_processing(
#'   "Test tool 1",
#'   my_processing(args, logger),
#'   source_file = my_r_script,
#'   args = W4MRUtils::parse_args(args = raw_args)
#' )
#'
#'
#' ##
#' # example 2
#'
#' ## let's say we have a R script with this content:
#' path <- write_r_file_with_content('
#'   setup_logger <- function(args, logger) {
#'     if (!is.null(args$verbose)) {
#'       logger$set_verbose(args$verbose)
#'     }
#'     if (!is.null(args$debug)) {
#'       logger$set_debug(args$debug)
#'     }
#'     if (!is.null(args$logs)) {
#'       logger$add_out_paths(args$logs)
#'     }
#'   }
#'   stop_logger <- function(logger) {
#'     logger$close_files()
#'   }
#'   processing <- function(args, logger) {
#'     setup_logger(args, logger)
#'     logger$info("The tool is working...")
#'     logger$infof("Input: %s.", args$input)
#'     logger$info("The tool stoping.")
#'     stop_logger(logger)
#'     return(NULL)
#'   }')
#'
#'
#' ## wrapper script:
#'
#' args <- W4MRUtils::optparse_parameters(
#'   input = W4MRUtils::optparse_character(),
#'   output = W4MRUtils::optparse_character(),
#'   logs = W4MRUtils::optparse_character(),
#'   one_float = W4MRUtils::optparse_numeric(),
#'   one_integer = W4MRUtils::optparse_integer(),
#'   one_logical = W4MRUtils::optparse_flag(),
#'   some_floats = W4MRUtils::optparse_list(of = "numeric"),
#'   some_characters = W4MRUtils::optparse_list(of = "character"),
#'   debug = W4MRUtils::optparse_flag(),
#'   verbose = W4MRUtils::optparse_flag(),
#'   args = raw_args[raw_args != "--args"]
#' )
#'
#' W4MRUtils::run_galaxy_processing("A Test tool", args = args, {
#'   ## processing is from the other R script
#'   processing(args, logger)
#' }, source_files = path)
#'
#'
#' @author L.Pavot
#' @export
run_galaxy_processing <- function(
  tool_name,
  code,
  tool_version = "unknown",
  unmangle_parameters = TRUE,
  args = NULL,
  logger = NULL,
  source_files = c(),
  env = NULL,
  do_traceback = FALSE
) {
  if (is.null(logger)) {
    logger <- get_logger(tool_name)
  }
  if (is.null(env)) {
    env <- new.env()
  }
  if (is.null(args)) {
    args <- suppressWarnings(parse_args(args = args)) # nolint:object_usage_linter
  }
  check_param_type_n_length(tool_name, "character")
  check_param_type_n_length(tool_version, "character")
  check_param_type_n_length(unmangle_parameters, "logical")
  check_parameter_type(args, "list")
  check_param_type_n_length(logger, "W4MLogger")
  check_parameter_type(source_files, "character", or_null = TRUE)
  check_parameter_type(env, "environment")
  if (in_galaxy_env()) {
    if (unmangle_parameters) {
      args <- unmangle_galaxy_param(args)
    }
    show_galaxy_header(
      tool_name,
      tool_version = tool_version,
      args = args,
      logger = logger
    )
  }
  env$args <- args
  env$logger <- logger
  in_error <- FALSE
  for (file in source_files) {
    logger$infof("Sourcing %s...", file)
    source_local(file, env = env)
  }
  time_before <- proc.time()
  result <- tryCatch({
      eval(rlang::enexpr(code), env)
    },
    error = function(error) {
      in_error <<- TRUE
      if (do_traceback) {
        traceback(2)
      }
      logger$errorf("The tool %s has crashed.", tool_name)
      logger$errorf("Error: %s", error)
      return(error)
    }
  )
  ellapsed_time <- proc.time() - time_before
  if (in_galaxy_env()) {
    show_galaxy_footer(
      tool_name,
      tool_version = tool_version,
      logger = logger,
      ellapsed = ellapsed_time
    )
  }
  if (in_error) {
    stopf(
      "The tool %s - version %s ended in error.",
      tool_name,
      tool_version
    )
  }
  return(invisible(result))
}

#' show_galaxy_header - shows the header for glaxy tools
#' @description
#' This function prints the header to display in galaxy's tools logs
#'
#' @param tool_name a \code{character(1)} holding the
#'   running tool's name.
#' @param tool_version a \code{character(1)} holding the
#'   running tool's version.
#' @param args a \code{list(param="value")} - if provided, their are the
#'   parameters shown in galaxy header and/or footer.
#' @param logger a \code{get_logger("name")} instance - if provided, the
#'   galaxy footer if output from the logger.
#' @param show_start_time - a logical telling whether to display
#'   the start time or not.
#' @param show_sys - a logical telling whether to display
#'   the system variables or not.
#' @param show_parameters - a logical telling whether to display
#'   the parameters or not.
#' @return NULL
#'
#' @seealso [run_galaxy_processing]
#'
#' @examples
#'
#' show_galaxy_header("Tool Name", "1.2.0")
#' show_galaxy_header(
#'   tool_name = "Tool Name",
#'   tool_version = "1.2.0",
#'   logger = get_logger("Some Tool"),
#'   show_start_time = FALSE,
#'   show_sys = FALSE,
#'   show_parameters = FALSE
#' )
#'
#' @author L.Pavot
#' @export
show_galaxy_header <- function(
  tool_name,
  tool_version,
  args = NULL,
  logger = NULL,
  show_start_time = TRUE,
  show_sys = TRUE,
  show_parameters = TRUE
) {
  sep <- collapse(rep("-", 78))
  gx_header <- ""
  if (show_start_time) {
    gx_header <- sprintf(
      "%sJob starting time:\n%s\n%s",
      gx_header,
      format(Sys.time(), "%a %d %b %Y %X"),
      sep
    )
  }
  if (show_sys) {
    gx_header <- sprintf(
      "%s\n%s\n%s",
      gx_header,
      collapse_lines(capture.output(print(get_r_env()))),
      sep
    )
  }
  if (show_parameters) {
    if (is.null(args)) {
      args <- commandArgs()
    }
    gx_header <- sprintf(
      "%s\nParameters used in %s - version %s:\n\n%s\n%s",
      gx_header,
      tool_name,
      tool_version,
      collapse_lines(capture.output(str(as.list(args)))),
      sep
    )
  }
  if (is.null(logger)) {
    cat(gx_header)
  } else {
    logger$default(gx_header)
  }
}

#' show_galaxy_footer - shows the footer for glaxy tools
#' @description
#' This function prints the footer to display in galaxy's tools logs
#'
#' @param tool_name a \code{character(1)} holding the
#'   running tool's name.
#' @param tool_version a \code{character(1)} holding the
#'   running tool's version.
#' @param logger a \code{get_logger("name")} instance - if provided, the
#'   galaxy footer if output from the logger.
#' @param show_packages logical - Tells whether to display loaded packages
#'   and attached packages.
#' @param ellapsed NULL or a \code{character(1)} with execution duration.
#' @return NULL
#'
#' @seealso [run_galaxy_processing]
#'
#' @examples
#'
#' show_galaxy_footer("Tool Name", "1.2.0")
#'
#' show_galaxy_footer(
#'   tool_name = "Tool Name",
#'   tool_version = "1.2.0",
#'   logger = get_logger("Some Tool"),
#'   show_packages = FALSE,
#'   ellapsed = "14.5 seconds"
#' )
#'
#' @author L.Pavot
#' @export
show_galaxy_footer <- function(
  tool_name,
  tool_version,
  logger = NULL,
  show_packages = TRUE,
  ellapsed = NULL
) {
  add <- function(text = "") {
    gx_footer <<- sprintf("%s\n%s", gx_footer, text)
  }
  gx_footer <- sprintf(
    "End of '%s' Galaxy module call: %s",
    tool_name,
    as.character(Sys.time())
  )
  add(collapse(rep("-", 78)))
  add()
  add(sprintf("Time ellapsed: %s", ellapsed))
  add()
  sessioninfo <- sessionInfo()
  add(sessioninfo$R.version$version.string)
  if (show_packages) {
    add()
    add("Main packages:")
    for (pkg in names(sessioninfo$otherPkgs)) {
      add(paste(pkg, packageVersion(pkg)))
    }
    add()
    add("Other loaded packages:")
    for (pkg in names(sessioninfo$loadedOnly)) {
      add(paste(pkg, packageVersion(pkg)))
    }
  }
  if (is.null(logger)) {
    cat(gx_footer)
  } else {
    logger$default(gx_footer)
  }
  return(invisible(NULL))
}

#' get_r_env - provides env vars begining with R_*
#' @description
#' Returns a list of env vars if the start with R_*.
#'
#' @return a \code{list} of environment variables which
#'   begin by `R_`.
#'
#' @seealso [run_galaxy_processing]
#'
#' @author L.Pavot
#' @export
get_r_env <- function() {
  env_vars <- Sys.getenv()
  env_vars[grepl(x = names(env_vars), pattern = "^R_.*$")]
}

#' show_sys - prints env variables related to R
#' @description
#' prints env variables related to R
#'
#' @return NULL
#'
#' @examples
#'
#' show_sys()
#'
#' @author L.Pavot
#' @export
show_sys <- function() {
  print(get_r_env())
}

#' in_galaxy_env - check if the script has been run by galaxy
#' @description
#' `in_galaxy_env` returns \code{TRUE} if it detects some
#' galaxy-specific environment variables. \code{FALSE} otherwise.
#' @return A logical - whether the script has been run by galaxy or not.
#'
#' @export
in_galaxy_env <- function() {
  sysvars <- Sys.getenv()
  sysvarnames <- names(sysvars)
  return(any(
    c(
      "_GALAXY_JOB_HOME_DIR",
      "_GALAXY_JOB_TMP_DIR",
      "GALAXY_MEMORY_MB",
      "GALAXY_MEMORY_MB_PER_SLOT",
      "GALAXY_SLOTS"
    ) %in% sysvarnames
  ))
}

#' unmangle_galaxy_param - revert effects of galaxy manglings.
#' @description
#' When running a tool from galaxy, the command line may be
#' altered because some forbidden chars have been translated by galaxy.
#'
#' This function takes `args` are invert the galaxy's mangling process.
#'
#' @param args named \code{list} - contains params_name=value.
#' @return a named \code{list} - with unmangled parameter name and
#'   values.
#'
#' @seealso [run_galaxy_processing], [unmangle_galaxy_string]
#'
#' @author L.Pavot
#' @export
unmangle_galaxy_param <- function(args) {
  check_parameter_type(args, "list")
  for (param in names(args)) {
    value <- args[[param]]
    if (is.character(value)) {
      value <- unmangle_galaxy_string(value)
    }
    unmangled_param <- unmangle_galaxy_string(param)
    if (unmangled_param != param) {
      args[[param]] <- NULL
    }
    args[[unmangled_param]] <- value
  }
  return(args)
}


#' unmangle_galaxy_string - revert effects of galaxy mangling
#' @description
#' Revert effect of string mangling from galaxy on the given string.
#'
#' @param string - the character vector to fix mangling.
#' @return string - the character vector, fixed.
#'
#' @seealso [run_galaxy_processing], [unmangle_galaxy_param]
#'
#' @author L.Pavot
#' @export
unmangle_galaxy_string <- function(string) {
  check_param_type_n_length(string, "character")
  if (! any(grepl(
    mapped_chars_regex__,
    string,
    ignore.case = TRUE
  ))) {
    return(string)
  }
  for (char in names(mapped_chars__)) {
    string <- gsub(
      mapped_chars__[[char]],
      char,
      string,
      fixed = TRUE
    )
  }
  return(string)
}
