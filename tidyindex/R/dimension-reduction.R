#' The dimension reduction module
#'
#' The module combines multiple variables into a new variable. The new variable
#' can be a linear combination of the original variables,
#' \code{aggregate_linear()}, or a geometric mean of the original variables,
#' \code{aggregate_geometry()}, or created from an user formula input,
#' \code{aggregate_manual()}.
#'
#' @param data used in \code{dimension_reduction()}, an \code{idx_tbl} object,
#' see [tidyindex::init()]
#' @param ... used in \code{dimension_reduction()}, a dimension reduction
#' object of \code{dim_red} class, currently one of \code{aggregate_linear()},
#' \code{aggregate_geometrical()}, or \code{aggregate_manual()}.
#' @param formula the formula to evaluate
#' @param weight used in \code{aggregate_linear()}, the column of the
#' linear weights from the \code{roles} element in an index table object.
#' See [tidyindex::add_paras()]
#'
#' @return an index table object
#' @rdname dimension-reduction
#' @export
#' @examples
#' dt <- gggi |>
#'   dplyr::select(country, sex_ratio_at_birth:healthy_life_expectancy) |>
#'   init()
#'
#' dt |>
#'   dimension_reduction(health = aggregate_manual(
#'     ~sex_ratio_at_birth * 0.693 + healthy_life_expectancy * 0.307))
#' dt |>
#'   add_paras(gggi_weights, by = variable) |>
#'   dimension_reduction(health = aggregate_linear(
#'     ~sex_ratio_at_birth:healthy_life_expectancy, weight = var_weight))
#' dt |>
#'   dimension_reduction(health = aggregate_geometrical(
#'     ~sex_ratio_at_birth:healthy_life_expectancy)
#'   )
#'
dimension_reduction <- function(data, ...){

  dot_name <- names(rlang::dots_list(...))
  # only do one action for now
  dot <- rlang::dots_list(...)[[1]]
  all_attrs <- names(attributes(dot))

  check_idx_tbl(data)
  check_dim_red_obj(dot)


  if ("var" %in% all_attrs){
    v <- attr(dot, "var")
    vars <- tidyselect::eval_select(rlang::parse_expr(v), data$data)
    vars_nm <- names(vars)
  }

  if ("weight" %in% all_attrs){
    w <- attr(dot, "weight")
    weight <- tidyselect::eval_select(rlang::parse_expr(w), data$paras)
    weight_nm <- names(weight)
  }

  if ("formula" %in% all_attrs){
    dot_fml <- attr(dot, "formula") |> rlang::parse_expr()
  }

  if (dot == "aggregate_linear"){
    weight <- data$paras |> filter(variables %in% vars_nm) |> pull(weight_nm)
    pieces <-  paste0(vars_nm, "*", weight, collapse = "+")
    dot_fml <- paste("~", pieces) |> stats::as.formula() |> rlang::f_rhs()
    data$data <- data$data |>
      mutate(!!dot_name := rlang::eval_tidy(dot_fml, data = data$data))
    exprs <- NA
    vars <- list(vars_nm)
    params <-  list(weight = weight)
  }

  if (dot == "aggregate_geometrical"){
    dot_fml <- build_geometrical_expr(vars_nm)
    data$data <- data$data |>
      mutate(!!dot_name := rlang::eval_tidy(dot_fml, data = data$data))
    exprs <- NA
    vars <- list(vars_nm)
    params <- NA
  }

  if (dot ==  "aggregate_manual"){
    data$data <- data$data |>
      mutate(!!dot_name := rlang::eval_tidy(dot_fml, data = data$data))
    exprs <- deparse(dot_fml)
    vars <- NA
    params <- NA
  }

  data$steps <- data$steps |> rbind(dplyr::tibble(
    id = nrow(data$steps) + 1,
    module = "dimension_reduction",
    op = list(dot),
    name = as.character(dot_name)))



  return(data)
}

#' @rdname dimension-reduction
#' @export
aggregate_linear <- function(formula, weight){
  vars <- rlang::f_text(formula)
  weight <- enquo(weight) |> rlang::quo_text()
  new_dimension_reduction("aggregate_linear", vars = vars,  weight = weight, formula = NULL)
}

#' @rdname dimension-reduction
#' @export
aggregate_geometrical <- function(formula){
  vars <- rlang::f_text(formula)
  new_dimension_reduction("aggregate_geometrical", vars = vars,  weight = NULL, formula = NULL)
}

#' @rdname dimension-reduction
#' @export
aggregate_manual <- function(formula){
  formula <- rlang::f_text(formula)
  new_dimension_reduction("aggregate_manual", formula = formula, vars = NULL, weight = NULL)
}

new_dimension_reduction <- function(type, formula, vars, weight){
  name <- type
  attr(name, "var") <- vars
  attr(name, "weight") <- weight
  attr(name, "formula") <- formula
  class(name) <- "dim_red"
  name
}

build_geometrical_expr <- function(vars){
  glue::glue("~(", paste0(vars,  collapse = "*"), ")^(1/{length(vars)})") |>
    stats::as.formula() |>
    rlang::f_rhs()
}
