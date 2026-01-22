#' The rescaling module
#'
#' The rescale module changes the scale of the variable(s) using one of the
#' available rescaling functions: \code{rescale_zscore()},
#' \code{rescale_minmax()}, and \code{rescale_center}.
#'
#' @param data an index table object, see [tidyindex::init()]
#' @param ... used in \code{rescaling}, a rescaling object of class
#' \code{rescale}, currently one of the \code{rescale_zscore()},
#' \code{rescale_minmax()}, and \code{rescale_center()},
#' @param var the variable(s) to rescale, accept tidyselect syntax
#' @param na.rm used in \code{rescale_*()}, logical, whether to remove NAs
#' @param min,max used in \code{rescale_minmax()}, the minimum and maximum value
#' @param censor used in \code{rescale_minmax()}, logical;
#' whether to censor points outside min and max, if provided
#' @return an index table object
#' @rdname rescale
#' @export
#' @examples
#' dt <- hdi |> init()
#' dt |> rescaling(life_exp = rescale_zscore(life_exp))
#' dt |> rescaling(life_exp2 = rescale_minmax(life_exp, min = 20, max = 85))
rescaling <- function(data, ...){
  dot <- rlang::list2(...)
  dot_mn <- names(dot) |> sym()
  dot <- dot[[1]]

  check_idx_tbl(data)
  check_rescale_obj(dot)

  var <- attr(dot, "var")
  args <- attr(dot, "args")
  args_quo_idx <- purrr::map_lgl(args, rlang::is_quosure)
  args_quo <- args[args_quo_idx]
  args_not_quo <- args[!args_quo_idx]

  args_quo_sym_idx <- purrr::map_lgl(
    args_quo, ~rlang::quo_get_expr(.x) |> rlang::is_symbol())
  args_quo_sym <- args_quo[args_quo_sym_idx]
  args_quo_not_sym <- args_quo[!args_quo_sym_idx]

  args_quo_sym <- map(args_quo_sym, rlang::quo_get_expr)
  args_quo_sym <- map(args_quo_sym, function(x) {
    data$paras |>
      dplyr::filter(variables == rlang::quo_get_expr(var)) |>
      dplyr::pull(x)
    }
  )

  args <- c(args_not_quo, args_quo_not_sym, args_quo_sym)

  data$data <- data$data |> mutate(!!dot_mn := do.call(
    attr(dot, "fn"),
    c(args, var = attr(dot, "var"))
    ))

  data$steps <- data$steps |>
    rbind(dplyr::tibble(
      id = nrow(data$steps) + 1,
      module = "rescaling",
      op = list(dot),
      name = as.character(dot_mn)))
  return(data)

}


#' @export
#' @rdname rescale
rescale_zscore <- function(var, na.rm = TRUE){

  fn <- function(var, na.rm = TRUE){
    (var - mean(var, na.rm = na.rm))/ sd(var, na.rm = na.rm)
  }

  new_rescale("rescale_zscore", var = enquo(var), fn = fn, na.rm = na.rm)

}

#' @export
#' @rdname rescale
rescale_minmax <- function(var, min = NULL, max  = NULL, na.rm = TRUE, censor = TRUE){

  min <- enquo(min)
  max <- enquo(max)

  fn <- function(var, min = NULL, max  = NULL, na.rm = TRUE, censor = TRUE){
    if (is.null(min)) min <- min(var, na.rm = na.rm)
    if (is.null(max)) max <- max(var, na.rm = na.rm)

    res <- (var - min)/diff(c(min, max))
    if (censor) res[res > 1] <- 1; res[res < 0] <- 0
    res
  }

  new_rescale("rescale_minmax", var = enquo(var), fn = fn,
              max = max, min = min, na.rm = na.rm, censor = censor)

}

#' @export
#' @rdname rescale
rescale_center <- function(var, na.rm = TRUE){
  fn <- function(var, na.rm = TRUE) {var - mean(var, na.rm = na.rm)}
  new_rescale("rescale_center", var = enquo(var), fn = fn, na.rm = na.rm)
}

new_rescale <- function(type, var, fn, ...){
  dots <- rlang::list2(...)
  name <- type
  attr(name, "var") <- var
  attr(name, "fn") <- fn
  attr(name, "args") <- dots
  class(name) <- "rescale"
  name
}
