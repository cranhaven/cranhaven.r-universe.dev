#' Specialized "long to wide" reshaping
#'
#' @description
#' On the surface, \code{conjecture()} appears similar to \code{tidyr::pivot_wider()}, but uses different logic tailored to a specific type of dataset:
#'
#' \itemize{
#'   \item{column corresponding to \code{names_from} contains only 2 levels}
#'   \item{there is no determinate combination of elements to fill 2 columns per row}.
#' }
#'
#' See vignette("conjecture") for more details.
#'
#' @details
#' \code{conjecture()} uses the following routine to match elements:
#'
#' \enumerate{
#'   \item{Values in \code{sort_by} are separated into two vectors: anterior and posterior.}
#'   \item{Each anterior element is matched with the closest posterior element measured by \code{sort_by}.}
#' }
#'
#'
#' @param data A data frame to reshape.
#' @param sort_by Column name, as symbol. Plays a similar role as \code{values_from} in \code{pivot_wider()}, but also serves as sorting dimension for underlying conjecture algorithm.
#' @param names_from Column name, as symbol. Used to differentiate anterior/posterior observations. Column must only contain 2 levels (missing values not allowed).
#' @param names_first level in variable specified by \code{names_from} indicating anterior observation.
#'
#' @return
#' An object of the same type as \code{data}.
#'
#' @export
#'
#' @examples
#' # See vignette("conjecture") for more examples
#'
#' conjecture(comms, timestamp, type, "send")
conjecture <- function(data, sort_by, names_from, names_first) {
  UseMethod("conjecture")
}

#' @export
conjecture.data.frame <- function(data, sort_by, names_from, names_first) {
  # name validation
  nms <- validate_name_col(data, ensym(names_from), names_first)
  if(any(nms %in% names(data))) abort(glue::glue("One of '{paste0(nms, collapse = ', ')}' matches column name in 'data'"))

  # sorting column validation
  x <- validate_sort_col(data, ensym(sort_by))

  # consolidate columns
  id <- create_group_id(data, enexpr(sort_by), enexpr(names_from))
  id_nm <- check_name("id", names(data), ".x")
  env_poke(current_env(), id_nm, id)

  nm <- pull(data, {{names_from}})
  df <- tibble(id, x, nm) %>%
    tidyr::complete(id, nm)

  df_A <- filter(df, nm == nms[1]) %>%
    arrange(id, desc(x))
  df_B <- filter(df, nm == nms[2]) %>%
    arrange(id, x)

  A_index <- which(!duplicated(df_A$id)) - 1L
  A_index[length(A_index) + 1] <- length(df_A$x)

  B_index <- which(!duplicated(df_B$id)) - 1L
  B_index[length(B_index) + 1] <- length(df_B$x)

  inner_join(ungroup(data) %>%
               mutate("{id_nm}" := !!sym(id_nm)) %>%
               select(-c(enexpr(sort_by), enexpr(names_from))) %>%
               distinct(),
             df_A %>%
               transmute("{id_nm}" := id,
                         "{nms[1]}" := x,
                         "{nms[2]}" := df_B$x[interlace(A_index, as.double(df_A$x), B_index, as.double(df_B$x))]) %>%
               tidyr::drop_na(sym(nms[1])),
             by = id_nm) %>%
    select(-sym(id_nm)) %>%
    arrange(!!sym(nms[1]))
}

validate_name_col <- function(data, names_from, names_first) {
  x <- pull(data, !!names_from)

  if (!names_first %in% x) abort(glue::glue("Value '{names_first}' not found in {names_from}."))
  if (anyNA(x)) abort(glue::glue("Column '{names_from}' must not contain any missing values."))
  if (n_distinct(x) != 2) abort(glue::glue("Column '{names_from}' must contain exactly 2 levels."))

  return(c(names_first, setdiff(unique(x), names_first)))
}

validate_sort_col <- function(data, sort_by) {
  tryCatch(
    warning = function(cnd) {
      abort("Column '{sort_by}' must be coercible to <double>")
    },
    as.double(pull(data, !!sort_by))
  )
  pull(data, !!sort_by)
}


create_group_id <- function(data, sort_by, names_from) {
  data %>%
    group_by(!!!syms(names(tidyselect::eval_select(expr(-c(!!sort_by, !!names_from)), data)))) %>%
    group_indices()
}
