#' @title Get System Info
#'
#' @description
#' Wrapper around Sys.info() that provides the values
#' in a named list format.
#'
#' @family Context
#' @return \code{Sys.info()} as a named list
get_system_info <- function() {
  as.list(Sys.info())
}

#' @title R Version
#'
#' @description
#' Wrapper around \code{R.Version()} to produce a nicely
#' formatted string for use use in sys_context.
#' @family Context
#' @return R environment version is (major).(minor) format
get_r_version <- function() {
  c("r_ver" = paste0(R.Version()[c("major", "minor")], collapse = "."))
}

#' @title System Context
#'
#' @description
#' Wrapper around Sys.info() and \code{get_r_version} that provides
#' a consolidated list of variables used for logging contexts.
#'
#' @section Metrics:
#'
#' System Context
#'
#' \itemize{
#'  \item{"sysname"} : {The operating system name.}
#'  \item{"release"} : {The OS release.}
#'  \item{"version"} : {The OS version.}
#'  \item{"nodename"} : {A name by which the machine is known on the network (if any).}
#'  \item{"machine"} : {A concise description of the hardware, often the CPU type.}
#'  \item{"login"} : {The user's login name, or "unknown" if it cannot be ascertained.}
#'  \item{"user"} : {The name of the real user ID, or "unknown" if it cannot be ascertained.}
#'  \item{"r-ver"} : {R Version in (major).(minor) format.}
#' }
#'
#' @return system context for evaluating \code{fmt_metric} objects.
#' @family Context
#' @export
sys_context <- function() {

  sys_info <- get_system_info()
  r_ver <- get_r_version()

  sys_context <- c(sys_info, r_ver)

  structure(sys_context, class = c("sys_context", "context"))
}

#' @title Extract Function Name
#'
#' @description
#' Extracts the name of the function from a deparse call.
#'
#' @param func function name
#' @family Internal
#' @returns function name without arguments
#' @importFrom stringr str_extract
extract_func_name <- function(func) {
  stringr::str_extract(func, pattern = "[^(]+")
}

#' @title Format Function Call
#'
#' @description
#' Formats a function call into a deparsed string.
#'
#' @param expr function call
#' @param cutoff max width cutoff
#' @family Context
#' @returns string representation of a func call.
format_fn_call <- function(expr,
                           cutoff = 100L) {
  deparse1(expr, collapse = " ",
           width.cutoff = cutoff,
           backtick = TRUE)
}

#' @title Formatted Call Stack
#'
#' @description
#' Placeholder for the formatted call stack in a log layout.
#'
#' @param keep_args T/F to indicate if you want to keep
#' call arguments or not.
#' @param call_subset offset index into system calls
#' @param filter_internal filter out internal calls?
#'
#' @family Context
#' @returns formatted call stack
#' @importFrom stringr str_detect fixed
get_call_stack <- function(keep_args = FALSE,
                           call_subset = c(-1, -1),
                           filter_internal = TRUE) {

  trace_back <- rlang::trace_back()
  trace <- sapply(trace_back, list, simplify = "branch")

  call_stack <- sapply(trace$call,
                       function(fn) {
                         func <- format_fn_call((fn))
                         ifelse(keep_args,
                                func, extract_func_name(func))
                       })

  if (filter_internal) {
    call_stack <- clean_internal_calls(call_stack)
  }

  if (!identical(call_subset, c(-1, -1))) {
    start <- max(call_subset[1], 1)
    end <- max(call_subset[2], length(call_stack))

    call_stack <- call_stack[start:end]
  }

  names(call_stack) <- paste0("call_", seq(length(call_stack)))

  structure(call_stack, class = c("call_stack", "stack"))
}

#' @title Is Logger Call
#'
#' @description
#' Determines if a call came from the logger, so we
#' can exclude it from the call stack.
#'
#' @param call function call
#' @family Internal
#' @returns string representation of a func call.
#' @importFrom stringr str_detect fixed
is_logger_call <- function(call) {
  stringr::str_detect(call, pattern = stringr::fixed("Logger$"))
}

#' @title Clean System Calls
#'
#' @description
#' Cleans up any internal system calls from
#' inside the package from the call stack.
#'
#' @param call_stack call stack
#'
#' @family Internal
#' @returns string representation of a func call.
#' @importFrom stringr str_starts fixed
clean_internal_calls <- function(call_stack) {

  internal_calls <- sapply(call_stack, function(call) {
    stringr::str_starts(call, pattern = stringr::fixed("dyn.log::"))
  }, simplify = TRUE)

  if (length(internal_calls) > 0) {
    call_stack <- call_stack[!internal_calls]
  }

  call_stack
}

#' @title Execution Context
#'
#' @description
#' Wrapper around Sys.info() and \code{get_r_version} that provides
#' a consolidated list of variables used for logging contexts.
#'
#' @param keep_args bool to specify keep function all arguments
#' @param max_calls maximum number of calls to keep from the stack
#' @param call_subset offset index into system calls
#' @param filter_internal filter out internal calls?
#'
#' @return system context for evaluating \code{fmt_metric} objects.
#' @family Context
#' @export
exec_context <- function(keep_args = FALSE,
                         max_calls = 5,
                         call_subset = c(-1, -1),
                         filter_internal = TRUE) {

  full_stack <- get_call_stack(keep_args = keep_args,
                               call_subset = call_subset,
                               filter_internal = filter_internal)

  lcs <- sapply(full_stack, is_logger_call, simplify = TRUE)
  lc_idx <- as.integer(which(lcs, arr.ind = TRUE))

  if (!identical(lc_idx, integer(0))) {
    full_stack <- full_stack[1:(lc_idx - 1)]
  }

  call_stack <- full_stack
  ncalls <- length(call_stack)

  exec_context <- list(
    call_stack = call_stack,
    calling_fn = ifelse(ncalls == 0, "none", call_stack[ncalls]),
    ncalls = ncalls
  )

  structure(exec_context, class = c("exec_context", "context"))
}

#' @title Calling Class Scope
#'
#' @description
#' Gets the exposed public field scope of a R6 class. Used for
#' evaluating cls field execution scopes.
#'
#' @param cls R6 class to export.
#'
#' @return system context for evaluating \code{fmt_metric} objects.
#' @family Context
#' @export
class_scope <- function(cls) {

  values <- list()

  cls_bindings <- c(as.list(cls),
                    as.list(cls$.__enclos_env__$private))

  invisible(lapply(names(cls_bindings), function(var) {
    value <- cls_bindings[[var]]

    if (!(any(class(value) %in% c("environment", "function"))))
      values[[var]] <<- value
  }))

  structure(values, class = c("cls_context", "context"))
}
