#' Testing alternatives
#'
#' The two functions allows you to substitute a value/expression in the pipeline
#' with other options. These functions will evaluate the modified pipeline step,
#' as well as its prior and subsequent steps to create different versions of the
#' index.
#'
#' @param data an \code{idx_tbl} object
#' @param .var the name of the variable, which the step is tested for
#' alternatives
#' @param .param the name of the parameter to swap
#' @param .values,.exprs a list of values or expressions
#'
#' @return  an index table
#' @rdname swap
#' @export
#' @examples
#' library(generics)
#' hdi_paras <- hdi_scales |>
#' dplyr::add_row(dimension = "Education", name = "Education",
#'                var = "sch", min = 0, max = 0) |>
#'   dplyr::mutate(weight = c(1/3, 0, 0, 1/3, 1/3),
#'                 weight2 = c(0.1, 0, 0, 0.8, 0.1),
#'                 weight3 = c(0.8, 0, 0, 0.1, 0.1),
#'                 weight4 = c(0.1, 0, 0, 0.1, 0.8))
#'
#' dt <- hdi |>
#'   init(id = country) |>
#'   add_paras(hdi_paras, by = var) |>
#'   rescaling(life_exp = rescale_minmax(life_exp, min = min, max = max)) |>
#'   rescaling(exp_sch = rescale_minmax(exp_sch, min = min, max = max)) |>
#'   rescaling(avg_sch = rescale_minmax(avg_sch, min = min, max = max)) |>
#'   rescaling(gni_pc = rescale_minmax(gni_pc, min = min, max = max)) |>
#'   dimension_reduction(sch = aggregate_manual(~(exp_sch + avg_sch)/2)) |>
#'   dimension_reduction(index = aggregate_linear(~c(life_exp, sch, gni_pc),
#'                       weight = weight))
#'
#'
#' dt2 <- dt |>
#'   swap_values(.var = "index", .param = weight,
#'               .value = list(weight2, weight3, weight4))
#' augment(dt2)
#'
#' dt3 <- dt |>
#'   swap_exprs(.var = index, .exprs = list(
#'              aggregate_geometrical(~c(life_exp, sch, gni_pc))))
#' augment(dt3)
swap_values <- function(data, .var, .param, .values){
  param <- rlang::ensym(.param) |> rlang::as_string()

  steps <- data$steps
  row_swap <- steps |> dplyr::filter(name == !!enquo(.var))

  ops_before <- steps |> filter(id < row_swap$id)
  data_raw <- attr(data, "data") |> init() |> add_paras(data$paras)
  res <- run_ops(data_raw, ops_before)

  param_values <- as.list(enexpr(.values))[-1] |> map(rlang::as_string)
  param_values <- c(attr(row_swap$op[[1]], param), param_values)
  param <- enquo(.param) |> rlang::quo_text()
  res2 <- map(param_values, function(x){
    attr(row_swap$op[[1]], param) <- x
    run_op_single(res, row_swap)}
    )

  ops_after <- data$steps |> filter(id > row_swap$id)
  res2 <- purrr::map(res2, ~run_ops(.x, ops_after))

  res <- tibble(.params = unlist(param_values)) |> dplyr::mutate(values = res2)
  class(res) <- c("idx_res", class(res))
  res
}

#' @rdname swap
#' @export
swap_exprs <- function(data, .var, .exprs){
  var <- enquo(.var) |> rlang::quo_name()
  exprs <- as.list(.exprs)
  row_swap <- data$steps |> dplyr::filter(name == !!var)

  ops_before <- data$steps |> filter(id < row_swap$id)
  data_raw <- attr(data, "data") |> init() |> add_paras(data$paras)
  res <- run_ops(data_raw, ops_before)

  rows <- list(row_swap, row_swap |> dplyr::mutate(op = .exprs))
  res2 <- map(rows, ~run_op_single(res, .x))

  ops_after <- data$steps |> dplyr::filter(id > row_swap$id)
  res2 <- purrr::map(res2, ~run_ops(.x, ops_after))

  res <- tibble(.exprs = seq_len(length(.exprs) + 1)) |>
    dplyr::mutate(values = res2)
  class(res) <- c("idx_res", class(res))
  res
}

run_ops <- function(data, ops){

  i <- 1
  while(i <= nrow(ops)){
    data <- run_op_single(data = data, op = ops[i,])
    i <- i + 1
  }

  data
}

run_op_single <- function(data, op){
  args <- list(data = data, var = op$op[[1]])
  args <- rlang::set_names(args, c("data", op$name))
  res <- do.call(op$module, args)
  return(res)
}

globalVariables(c("module", "var","modify", "id", "res_str", ".", "obj",
                  ".raw_data", "out"))
