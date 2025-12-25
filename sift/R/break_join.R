#' Join tables based on overlapping intervals.
#'
#' @description
#' User-friendly interface that synthesizes power of \code{dplyr::left_join} and \code{findInterval}.
#'
#' @param x A data frame.
#' @param y Data frame containing desired reference information.
#' @param brk Name of column in \code{x} and \code{y} to join by via interval overlapping. Must be coercible to numeric.
#' @param by Joining variables, if needed. See \link[dplyr]{mutate-joins}.
#' @param ... additional arguments automatically directed to \code{findInterval} and \code{dplyr::left_join}. No partial matching.
#'
#' @return
#' An object of the same type as \code{x}.
#'
#' \itemize{
#'   \item All \code{x} rows will be returned.
#'   \item All columns between \code{x} and \code{y} are returned.
#'   \item Rows in \code{y} are matched with \code{x} based on overlapping values of \code{brk} (e.g. \code{findInterval(x$brk, y$brk, ...)}).
#' }
#' @export
#' @import rlang
#'
#' @examples
#' # joining USA + UK leaders with population time-series
#' break_join(us_uk_pop, us_uk_leaders, brk = c("date" = "start"))
#'
#' # simple dataset
#' set.seed(1)
#' a <- data.frame(p = c(rep("A", 10), rep("B", 10)), q = runif(20, 0, 10))
#' b <- data.frame(p = c("A", "A", "B", "B"), q = c(3, 5, 6, 9), r = c("a1", "a2", "b1", "b2"))
#'
#' break_join(a, b, brk = "q") # p identified as common variable automatically
#' break_join(a, b, brk = "q", by = "p") # same result
#' break_join(a, b, brk = "q", all.inside = TRUE) # note missing values have been filled
#'
#' # joining toll prices with vehicle time-series
#'
#' library(mopac)
#' library(dplyr, warn.conflicts = FALSE)
#' library(hms)
#'
#' express %>%
#'   mutate(time_hms = as_hms(time)) %>%
#'   break_join(rates, brk = c("time_hms" = "time"))
break_join <- function(x, y, brk = character(), by = NULL, ...) {
  UseMethod("break_join")
}

#' @export
break_join.data.frame <- function(x, y, brk = character(), by = NULL, ...) {

  # check x & y have columns
  has_columns(x)
  has_columns(y)

  # dot handling
  dots <- list2(...)
  dot_fns <- assign_fn(names(dots), findInterval, left_join)

  # brk handling
  if (length(brk) != 1 | !is.character(brk)) stop("input brk must be length 1 chr", call. = FALSE)
  brk <- mirror_name(brk)
  eval(expr(arg_match0(!!names(brk), names(x))))
  eval(expr(arg_match0(!!unname(brk), names(y))))

  # by handling
  if (is.null(by)) by <- assign_by(x, y, brk)
  if (is.null(names(by))) by <- mirror_name(by)

  # test join operation (take advantage of underlying dplyr arg checks)
  skeleton <- left_join(x[0,], y[0,setdiff(names(y),brk)], by = by, !!!dots[dot_fns == "left_join"])

  if (nrow(x) == 0) return(skeleton)

  .bj1 <- check_name(".bj1", c(names(x), names(y)), ".x")
  .bj2 <- check_name(".bj2", c(names(x), names(y)), ".x")

  # prep y
  y <- y %>%
    group_by(!!!syms(unname(by))) %>%
    arrange(!!sym(unname(brk))) %>%
    tidyr::nest("{.bj2}" := -group_cols())

  # prep x, join with y, overlap intervals
  arrange(x, !!sym(names(brk))) %>%
    tidyr::nest("{.bj1}" := -names(by)) %>%
    left_join(y, by = by, !!!dots[dot_fns == "left_join"]) %>%
    mutate("{.bj1}" := purrr::map2(!!sym(.bj1), !!sym(.bj2), .f = align_intervals, brk, by, skeleton, !!!dots[dot_fns == "findInterval"]),
           .keep = "unused") %>%
    tidyr::unnest(!!sym(.bj1)) %>%
    dplyr_reconstruct(x)
}

assign_fn <- function(args, ...) {

  fns <- enexprs(...)
  fn_args <- purrr::map(fns, ~ {
    fn_fmls_names(eval(..1))
  })


  fn_names <- purrr::map_chr(fns, ~ {
    f1 <- if(is.call(..1)) ..1[[3]] else ..1
    as_string(f1)
  })
  fn_names_flt <- paste0(fns, collapse = ", ")


  purrr::map_chr(args, ~ {
    a <- ..1
    b <- purrr::map_chr(fn_args, ~ {
      tryCatch(
        error = function(cnd) {
          NA_character_
        },
        arg_match0(a, .x)
      )
    })

    if(all(is.na(b))) {
      eval(expr(stop("Arg '", a, "' not found in ", fn_names_flt, call. = FALSE)))
    } else if(length(b[is.na(b)]) > 1) {
      eval(expr(stop("Cannot assign arg '", a, "' since it appears multiple times between ", fn_names_flt, call. = FALSE)))
    }

    fn_names[which(!is.na(b))]
  })
}

has_columns <- function(x) {
  x1 <- as_string(ensym(x))

  if(ncol(x) == 0) abort(paste0("input ", x1, " has no columns"))
  invisible(TRUE)
}

mirror_name <- function(x) {
  if (is.null(names(x))) return(set_names(x, x))
  return(x)
}

align_intervals <- function(.x, .y, brk, by, skeleton, ...) {
  if(is.null(.y)) return(.x %>% left_join(skeleton[0,setdiff(names(skeleton), by)], by = names(.x)))

  .brk <- findInterval(.x[[names(brk)]], .y[[brk]], ...)
  .brk <- if_else(.brk == 0, NA_integer_, .brk)
  .brk <- .y[[brk]][.brk]

  brk_nm <- check_name(".brk", c(names(.x), names(.y)), ".x")
  env_poke(current_env(), brk_nm, .brk)

  left_join(
    ungroup(.x) %>%
      mutate("{brk_nm}" := !!sym(brk_nm)),
    .y %>%
      rename("{brk_nm}" := !!sym(unname(brk))),
    by = brk_nm
  ) %>%
    select(-!!sym(brk_nm))
}
