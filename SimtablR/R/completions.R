# Interactive REPL and IDE Autocompletion
#
# Opt-in completion engine for RStudio providing context-sensitive column name
# and terse flag auto-discovery inside SimtablR interactive verbs.

#########
# COMPLETION CALL PARSER AND CONTEXT DETECTION
# Syntax tree inspection, partial token recovery, and data argument extraction.

#' Return character vector of function names supporting autocomplete
#' @keywords internal
#' @noRd
.simtab_completion_surfaces <- function() {
  c("tb", "table1", "regtab", "diag_test", "roc", "survtab")
}

#' Parse buffer line to extract target function name and data argument
#' @keywords internal
#' @noRd
.simtab_parse_completion_call <- function(line) {
  if (!is.character(line) || length(line) != 1 || !nzchar(line)) {
    return(NULL)
  }

  # Close the (possibly partial) expression enough to parse it: strip a
  # trailing partial token (the thing being typed), then balance parens.
  # Strategy: repeatedly try parsing progressively-truncated / brace-closed
  # forms rather than a hand-rolled tokenizer, reusing R's own parser.
  closed <- .simtab_close_expression(line)
  if (is.null(closed)) {
    return(NULL)
  }

  parsed <- tryCatch(parse(text = closed, keep.source = TRUE), error = function(e) NULL)
  if (is.null(parsed) || length(parsed) == 0) {
    return(NULL)
  }

  expr <- parsed[[length(parsed)]]

  call_info <- .simtab_innermost_surface_call(expr)
  if (is.null(call_info)) {
    return(NULL)
  }

  # Require that the cursor sits after a "(" or "," belonging to that call's
  # dots (i.e. we are in a variable/flag position, not still typing the data
  # argument's own token). Approximated by requiring at least one comma (or
  # the opening paren immediately followed by nothing but the partial token)
  # between the data argument and end-of-line for the *original* (unclosed)
  # line.
  if (!.simtab_cursor_in_dots_position(line, call_info$fn_name)) {
    return(NULL)
  }

  list(
    fn_name = call_info$fn_name,
    data_expr = call_info$data_expr
  )
}

#' Close incomplete R expression string to enable syntactic parsing
#' @keywords internal
#' @noRd
.simtab_close_expression <- function(line) {
  # Drop the trailing partial identifier/token (what the user is typing).
  stripped <- sub("[A-Za-z0-9._]*$", "", line)

  opens <- gregexpr("[(]", stripped)[[1]]
  closes <- gregexpr("[)]", stripped)[[1]]
  n_open <- if (identical(opens[1], -1L)) 0L else length(opens)
  n_close <- if (identical(closes[1], -1L)) 0L else length(closes)
  balance <- n_open - n_close

  if (balance < 0) {
    return(NULL)
  }

  # Close with a dummy arg name (NULL) then the missing parens, so
  # `tb(epitabl, ` becomes a parseable call `tb(epitabl, NULL)`.
  closed <- paste0(stripped, "NULL", strrep(")", balance))

  # Sanity check it actually parses; if not, this line isn't recoverable.
  ok <- tryCatch({
    parse(text = closed)
    TRUE
  }, error = function(e) FALSE)

  if (!ok) NULL else closed
}

#' Traverse syntax tree to find innermost recognized SimtablR call
#' @keywords internal
#' @noRd
.simtab_innermost_surface_call <- function(expr) {
  surfaces <- .simtab_completion_surfaces()

  found <- NULL

  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }

    head <- e[[1]]
    fn_name <- if (is.symbol(head)) {
      as.character(head)
    } else if (is.call(head) && identical(head[[1]], as.symbol("::"))) {
      as.character(head[[3]])
    } else {
      NA_character_
    }

    # Recurse into arguments first so the *innermost* matching call wins.
    if (length(e) > 1) {
      for (i in 2:length(e)) {
        walk(e[[i]])
      }
    }

    if (!is.na(fn_name) && fn_name %in% surfaces) {
      data_arg <- .simtab_extract_data_arg(e)
      found <<- list(fn_name = fn_name, data_expr = data_arg)
    }

    invisible(NULL)
  }

  walk(expr)
  found
}

#' Extract deparsed data expression from candidate completion call
#' @keywords internal
#' @noRd
.simtab_extract_data_arg <- function(call_expr) {
  args <- as.list(call_expr)[-1]
  arg_names <- names(args)
  if (is.null(arg_names)) arg_names <- rep("", length(args))

  if ("data" %in% arg_names) {
    val <- args[[which(arg_names == "data")[1]]]
  } else {
    positional <- which(arg_names == "")
    if (length(positional) == 0) {
      return(NA_character_)
    }
    val <- args[[positional[1]]]
  }

  deparse1_safe <- function(x) {
    tryCatch(paste(deparse(x), collapse = " "), error = function(e) NA_character_)
  }
  deparse1_safe(val)
}

#' Test whether REPL cursor is located within variable or flag dot arguments
#' @keywords internal
#' @noRd
.simtab_cursor_in_dots_position <- function(line, fn_name) {
  # Find the LAST occurrence of "fn_name(" (accounting for optional pkg::)
  # in the original line, then require a "," to appear after it (i.e. we are
  # past the first/data argument already).
  pattern <- paste0("(^|[^A-Za-z0-9._])", fn_name, "\\s*\\(")
  m <- gregexpr(pattern, line, perl = TRUE)[[1]]
  if (identical(m[1], -1L)) {
    return(FALSE)
  }
  last_start <- m[length(m)]
  match_len <- attr(m, "match.length")[length(m)]
  after_paren <- substring(line, last_start + match_len)
  grepl(",", after_paren, fixed = TRUE)
}

#' Extract column names and valid terse flags matching partial token
#' @keywords internal
#' @noRd
.simtab_completion_candidates <- function(data_expr, fn_name, token, envir = globalenv()) {
  if (is.na(data_expr) || !nzchar(data_expr)) {
    return(character(0))
  }

  data_obj <- tryCatch(
    eval(parse(text = data_expr)[[1]], envir = envir),
    error = function(e) NULL
  )

  if (!is.data.frame(data_obj)) {
    return(character(0))
  }

  candidates <- names(data_obj)

  if (fn_name %in% c("tb", "table1")) {
    candidates <- c(
      candidates,
      .canonical_flag_tokens(),
      names(.simtab_flag_aliases()),
      names(.simtab_flag_synonyms())
    )
  }

  if (nzchar(token)) {
    candidates <- candidates[startsWith(candidates, token)]
  }

  candidates
}

#########
# RSTUDIO COMPLETION HOOK AND HOST DISPATCH
# Custom completion callback registration, host detection, and fallback chaining.

.simtab_completion_state <- new.env(parent = emptyenv())
.simtab_completion_state$mode <- NULL
.simtab_completion_state$prior_completer <- NULL
.simtab_completion_state$installed <- FALSE

#' Determine completion dispatch mode for current host environment
#' @keywords internal
#' @noRd
.simtab_completion_mode <- function() {
  if (!is.null(.simtab_completion_state$mode)) {
    return(.simtab_completion_state$mode)
  }

  # RStudio's own RPC dispatcher wraps the custom completer call in
  # tryCatch(..., error = identity) (verified against RStudio source,
  # SessionRCompletions.R) and falls through to its native
  # engine on error. That is what makes "throw" a safe way to decline a token:
  # the host completes it natively.
  #
  # No other host offers that contract. Handing a token back on Rterm or radian
  # would mean re-entering R's own completion machinery, which is reachable only
  # through an unexported `utils` function -- not something a CRAN package may
  # call. Rather than either shipping that call or silently returning no
  # completions at all, the completer is offered on RStudio only.
  mode <- if (.simtab_completion_host_supported()) "throw" else "unsupported"

  .simtab_completion_state$mode <- mode
  mode
}

#' Test whether host IDE supports transparent completion fallback
#' @keywords internal
#' @noRd
.simtab_completion_host_supported <- function() {
  identical(Sys.getenv("RSTUDIO"), "1")
}

#' REPL custom completion callback handler for RStudio
#' @keywords internal
#' @noRd
.simtab_completer <- function(env) {
  hand_back <- function() {
    prior <- .simtab_completion_state$prior_completer
    if (is.function(prior)) {
      return(prior(env))
    }
    # Deliberately a bare stop(), and deliberately NOT a simtab_error_*: this is
    # control flow, not a user-facing failure. RStudio's completion dispatcher
    # catches any error from a custom completer and falls through to its native
    # engine, so throwing is how this completer says "not mine". Classing it
    # would advertise a SimtablR error for an event the user never sees.
    stop("simtab_completer: not a SimtablR call; hand back to native completion.")
  }

  line <- tryCatch(env[["linebuffer"]], error = function(e) NULL)
  token <- tryCatch(env[["token"]], error = function(e) "")
  if (is.null(line)) {
    return(hand_back())
  }

  parsed <- tryCatch(
    .simtab_parse_completion_call(line),
    error = function(e) NULL
  )

  if (is.null(parsed)) {
    return(hand_back())
  }

  candidates <- tryCatch(
    .simtab_completion_candidates(parsed$data_expr, parsed$fn_name, token, globalenv()),
    error = function(e) character(0)
  )

  if (length(candidates) == 0) {
    return(hand_back())
  }

  env[["comps"]] <- candidates
  invisible(NULL)
}

#' Opt-in RStudio autocomplete for SimtablR calls
#'
#' Installs (or removes) a session-wide completer that offers column-name
#' and terse-flag completions inside calls to [tb()], [table1()], [regtab()],
#' [diag_test()], [roc()], and [survtab()] — e.g. `tb(df, var<TAB>)`.
#'
#' This works by setting `rc.options(custom.completer = )`, the only
#' completion-extension hook RStudio's editor honors for package authors.
#' Because that option is **session-global**, enabling it affects completion
#' behavior everywhere in the session, not just inside SimtablR calls: on any
#' line that isn't a recognized SimtablR call, this completer declines the
#' token, and RStudio falls back to its own completion engine (or to a
#' previously installed custom completer, which is called first).
#'
#' @section Supported hosts:
#' RStudio only. Declining a token relies on the host restoring native
#' completion, which RStudio does and a plain console does not; on any other
#' front end (Rterm, radian, batch), enabling the completer would leave
#' non-SimtablR tokens with no completions at all. `simtab_completions()`
#' therefore declines to install outside RStudio and tells you so, rather than
#' degrading completion for the rest of the session.
#'
#' Never installed automatically — SimtablR's `.onLoad` does not call this.
#' Call it yourself, e.g. from `.Rprofile`:
#' `if (interactive()) SimtablR::simtab_completions()`.
#'
#' @param enable Logical. `TRUE` (default) installs the completer; `FALSE`
#'   removes it and restores whatever `custom.completer` held before.
#' @param quiet Logical. Suppress the confirmation message. Default `FALSE`.
#' @return Invisibly, a list with `enabled` (logical), `mode` (`"throw"` when
#'   installed, `"unsupported"` when the host cannot support it), and `host`
#'   (`"rstudio"` or `"other"`).
#' @examples
#' simtab_completions(enable = FALSE, quiet = TRUE)
#' @export
simtab_completions <- function(enable = TRUE, quiet = FALSE) {
  host <- if (.simtab_completion_host_supported()) "rstudio" else "other"

  if (isTRUE(enable)) {
    if (!.simtab_completion_host_supported()) {
      if (!quiet) {
        cli::cli_inform(c(
          "SimtablR autocomplete is available in RStudio only.",
          "i" = "Other front ends do not restore native completion when a custom
                 completer declines a token, so installing it here would remove
                 completions for everything that is not a SimtablR call.",
          "v" = "Use SimtablR normally; completion is unaffected."
        ))
      }
      return(invisible(list(enabled = FALSE, mode = "unsupported", host = host)))
    }

    if (!isTRUE(.simtab_completion_state$installed)) {
      .simtab_completion_state$prior_completer <- utils::rc.getOption("custom.completer")
    }
    mode <- .simtab_completion_mode()
    utils::rc.options(custom.completer = .simtab_completer)
    .simtab_completion_state$installed <- TRUE

    if (!quiet) {
      cli::cli_inform(
        "SimtablR autocomplete enabled. Disable with {.code simtab_completions(FALSE)}."
      )
    }

    return(invisible(list(enabled = TRUE, mode = mode, host = host)))
  }

  prior <- .simtab_completion_state$prior_completer
  utils::rc.options(custom.completer = prior)
  .simtab_completion_state$installed <- FALSE
  .simtab_completion_state$prior_completer <- NULL

  if (!quiet) {
    cli::cli_inform("SimtablR autocomplete disabled.")
  }

  invisible(list(enabled = FALSE, mode = NA_character_, host = NA_character_))
}
