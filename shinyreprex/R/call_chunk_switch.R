#' @description
#' When reproducing a `switch` call, only the branch matching the current value of
#' the switch expression should be included in the output, rather than all branches.
#'
#' Fall-through alternatives (empty values, e.g. `"a" =,`) are resolved by walking
#' forward to the next non-empty branch. If no named alternative matches, the unnamed
#' default (last unnamed argument) is used. When there is no match and no default,
#' an empty `Repro` object is returned.
#'
#' @include repro_call_chunk.R
#' @noRd
S7::method(repro_call_chunk, class_call_switch) <- function(x, repro_code = Repro(), env = rlang::caller_env()) {
  switch_args <- rlang::call_args(x)
  check <- eval(switch_args[[1L]], envir = env)
  alternatives <- switch_args[-1L]
  alt_names <- names(alternatives)

  matched_idx <- match(check, alt_names)

  # Walk forward through fall-through (empty/missing) alternatives
  if (!is.na(matched_idx)) {
    while (matched_idx <= length(alternatives) && rlang::is_missing(alternatives[[matched_idx]])) {
      matched_idx <- matched_idx + 1L
    }
  }

  # Fall back to unnamed default if no named match
  if (is.na(matched_idx) || matched_idx > length(alternatives)) {
    default_positions <- which(alt_names == "")
    matched_idx <- if (length(default_positions) > 0L) tail(default_positions, 1L) else NA_integer_
  }

  # No matching branch and no default
  if (is.na(matched_idx)) return(repro_code)

  branch <- alternatives[[matched_idx]]
  branch_exprs <- if (rlang::is_call(branch, "{")) as.list(branch)[-1L] else list(branch)

  check_calls <- purrr::map(branch_exprs, repro_chunk, env = env)
  repro_code@packages <- purrr::map(check_calls, "packages") |> unlist()
  repro_code@prerequisites <- purrr::map(check_calls, "prerequisites") |>
    purrr::discard(identical, list()) |>
    unlist(recursive = FALSE)

  repro_code@packages <- get_pkg_name(x)
  repro_code@code <- purrr::map(check_calls, "code") |> unlist(recursive = FALSE)
  repro_code
}
