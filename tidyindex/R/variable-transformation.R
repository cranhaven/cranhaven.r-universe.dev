#' The variable transformation module
#'
#' The variable transformation module is used to transform a single variable
#' in the index table object. The transformation is specified by a variable
#' transformation object of class \code{var_trans}, created by
#'  \code{trans_*} functions. Currently, the following transformation functions
#'  are supported: \code{trans_log10}, \code{trans_quadratic},
#'  \code{trans_square_root}, and \code{trans_cubic_root}.
#'
#'
#' @param data an index table object
#' @param ... an variable transformation recipe of class \code{var_trans},
#' created by \code{trans_*} function, the transformation recipe to be
#' evaluated
#' @param var used in \code{trans_*} functions, the variable to be transformed
#'
#' @return an index table object
#' @rdname variable-transformation
#' @export
#' @examples
#' hdi |> init() |> variable_trans(gni_pc = trans_log10(gni_pc))
variable_trans <- function(data, ...){
  dot <- rlang::list2(...)
  dot_mn <- names(dot) |> sym()
  dot <- dot[[1]]

  check_idx_tbl(data)
  check_var_trans_obj(dot)

  id <- get_id(data)
  if (length(id) == 1) {
    dt <- data$data |>  dplyr::group_by(!!!dplyr::syms(id))
  }  else{
    dt <- data$data
  }
   data$data <- dt |> mutate(!!dot_mn := do.call(
    attr(dot, "fn"),
    c(attr(dot, "args"), attr(dot, "var"))
  )) |> dplyr::ungroup()


  data$steps <- data$steps |>
    rbind(dplyr::tibble(
      id = nrow(data$steps) + 1,
      module = "variable_transformation",
      op = list(dot),
      name = as.character(dot_mn)))
  return(data)
}


#' @rdname variable-transformation
#' @export
trans_log10 <- function(var){
  fn <- function(x) log10(x)
  new_trans("trans_log", var = enquo(var), fn = fn)
}

#' @rdname variable-transformation
#' @export
trans_quadratic <- function(var){
  fn <- function(x) x^2
  new_trans("trans_quadratic", var = enquo(var), fn = fn)
}

#' @rdname variable-transformation
#' @export
trans_square_root <- function(var){
  fn <- function(x) sqrt(x)
  new_trans("trans_square_root", var = enquo(var), fn = fn)
}

#' @rdname variable-transformation
#' @export
trans_cubic_root <- function(var){
  fn <- function(x) x^(1/3)
  new_trans("trans_cubic_root", var = enquo(var), fn = fn)
}

new_trans <- function(type, var, fn, ...){
  dots <- rlang::list2(...)
  name <- type
  attr(name, "var") <- var
  attr(name, "fn") <- fn
  attr(name, "args") <- dots
  class(name) <- "var_trans"
  name
}
