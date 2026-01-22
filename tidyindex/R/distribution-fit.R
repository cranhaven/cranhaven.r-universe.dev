#' The distribution fit module
#'
#' This module fits a distribution to the variable of interest. Currently
#' implemented distributions are: gamma, \code{dist_gamma()},
#' generalized logistic, \code{dist_glo()}, generalized extreme value,
#' \code{dist_gev()}, and Pearson Type III, \code{dist_pe3()}
#'
#' @param data an index table object
#' @param ... a distribution fit object, currently implemented are
#' \code{dist_gamma()}, \code{dist_glo()}, \code{dist_gev()}, and
#' \code{dist_pe3()}
#' @param var used in \code{dist_*()} functions, the variable to fit
#' @param method used in \code{dist_*()} functions, the fitting method,
#' currently support "lmoms" for L-moment fit
#' @param .n_boot the number of bootstrap replicate, default to 1
#' @param .boot_seed the seed to generate bootstrap replicate, default to 123
#' @return an index table object
#' @rdname dist-fit
#' @export
#' @examples
#' library(dplyr)
#' library(lmomco)
#' tenterfield |>
#'   mutate(month = lubridate::month(ym)) |>
#'   init(id = id, time = ym, group = month) |>
#'   temporal_aggregate(.agg = temporal_rolling_window(prcp, scale = 12)) |>
#'   distribution_fit(.fit = dist_gamma(.agg, method = "lmoms"))
distribution_fit <- function(data, ...){
  dot <- rlang::list2(...)
  dot_mn <- names(dot) |> sym()
  dot <- dot[[1]]

  check_idx_tbl(data)
  check_dist_fit_obj(dot)
  group_var <- get_group_var(data)
  id <- get_id(data)
  time <- get_temporal_index(data)

  distfit_vars_root <- rlang::quo_get_expr(attr(dot, "var"))
  distfit_vars <- grep(distfit_vars_root, data$steps$name, value = TRUE)
  dot_mn <- paste0(dot_mn, sub(distfit_vars_root, "", distfit_vars))


  if (attr(dot, "n_boot") != 1){
    dt <- bootstrap_aggregation(
      data$data, quo(!!distfit_vars), time,
      attr(dot, "n_boot"), attr(dot, "boot_seed"))
  } else{
    dt <- data$data |>
      dplyr::ungroup() |>
      dplyr::nest_by(id, !!sym(group_var))
  }

  res <- purrr::map2(distfit_vars, dot_mn, ~compute_distfit(dt, .x, .y, dot))

  if (attr(dot, "n_boot") != 1){
    dt <- dt |> dplyr::select(-data) |> dplyr::rename(data = raw_data)
    data$paras <- data$paras |>
      dplyr::add_row(variables = ".boot", roles = "boot_id")
  }


  data$data <- dt |> tidyr::unnest(data) |> dplyr::ungroup() |>
    dplyr::bind_cols(purrr::reduce(res, dplyr::bind_cols)) |>
    dplyr::arrange(id, !!sym(time))


  data$steps <- data$steps |>
    rbind(dplyr::tibble(
      id = nrow(data$steps) + 1,
      module = "distribution_fit",
      op = list(dot),
      name = as.character(dot_mn)))
  return(data)
}

bootstrap_aggregation <- function(data, col, date, n_boot, boot_seed){
  set.seed(boot_seed)
  col_str <- rlang::quo_get_expr(col)
  purrr::map_dfr(1:n_boot, ~data %>% mutate(.boot = .x)) |>
    group_by(id, .period = lubridate::month(!!sym(date)))  |>
    tidyr::nest(raw_data = -c(id, .period, .boot)) |>
    rowwise() |>
    mutate(
      data = list(
        tibble(!!sym(col_str) := sample(raw_data[[col_str]], nrow(raw_data), replace = TRUE))
        )) |>
    ungroup() |>
    rowwise()
}

compute_distfit <- function(data, var, name, dot){
  if ("raw_data" %in% colnames(data)){
    res <- data |>
      mutate(!!name[[1]] := list(do.call(
        attr(dot, "fn"), list(var_para = data[[var[[1]]]],
                              var_fit = raw_data[[var[[1]]]])))
      )
  } else{
    res <- data |>
      mutate(!!name[[1]] := list(do.call(
        attr(dot, "fn"), list(var_para = data[[var[[1]]]],
                              var_fit = data[[var[[1]]]])))
      )
  }

  res <- res |>
    dplyr::ungroup() |>
    tidyr::unnest(!!name[[1]]) |>
    ungroup() |>
    mutate(data = pmap(list(data, .fitted = fit), cbind)) |>
    tidyr::unnest(data) |>
    dplyr::select(-fit)

  new_nm <- c(name, glue::glue("{name[[1]]}_obj"))
  res |>
    dplyr::select(.fitted, para) |>
    dplyr::rename(!!new_nm[[1]] := ".fitted", !!new_nm[[2]] := "para")

}



#' @export
#' @rdname dist-fit
dist_gamma <- function(var, method = "lmoms", .n_boot = 1, .boot_seed = 123){

  check_lmomco_installed()
  fn <- switch(method,
    lmoms = function(var_para, var_fit) {
      para <- do.call("pargam", list(do.call("lmoms", list(var_para))))
      var_fit2 <- var_fit[!is.na(var_fit)]
      fit <- do.call("cdfgam", list(x = var_fit2, para = para))
      n_padding <- length(var_fit) - length(fit)
      if (n_padding > 0) {
        fit <- c(rep(NA, n_padding), fit)

      }
      tibble(para = list(para), fit = list(fit))
    },
    mle = function(var, dist) fn_mle,
    mom = function(var, dist) fn_mom
  )
  new_dist_fit("distfit_gamma", var = enquo(var), dist = "gamma",
               fn = fn, .n_boot = .n_boot, .boot_seed = .boot_seed)
}

#' @export
#' @rdname dist-fit
dist_glo <- function(var, method = "lmoms", .n_boot = 1, .boot_seed = 123){

  check_lmomco_installed()
  fn <- switch(method,
    lmoms = function(var_para, var_fit) {
      para <- do.call("parglo", list(do.call("lmoms", list(var_para))))
      fit <- do.call("cdfglo", list(x = var_fit, para = para))
      tibble(para = list(para), fit = list(fit))
    },
    mle = function(var, dist) fn_mle,
    mom = function(var, dist) fn_mom
  )
  new_dist_fit("distfit_glo", var = enquo(var), dist = "glo",
               fn = fn, .n_boot = .n_boot, .boot_seed = .boot_seed)
}

#' @export
#' @rdname dist-fit
dist_gev <- function(var, method = "lmoms", .n_boot = 1, .boot_seed = 123){

  check_lmomco_installed()
  fn <- switch(method,
    lmoms = function(var_para, var_fit) {
      para <- do.call("pargev", list(do.call("lmoms", list(var_para))))
      fit <- do.call("cdfgev", list(x = var_fit, para = para))
      tibble(para = list(para), fit = list(fit))
    },
    mle = function(var, dist) fn_mle,
    mom = function(var, dist) fn_mom
  )
  new_dist_fit("distfit_gev", var = enquo(var), dist = "gev",
               fn = fn, .n_boot = .n_boot, .boot_seed = .boot_seed)
}

#' @export
#' @rdname dist-fit
dist_pe3 <- function(var, method = "lmoms", .n_boot = 1, .boot_seed = 123){

  check_lmomco_installed()
  fn <- switch(method,
    lmoms = function(var_para, var_fit) {
      para <- do.call("parpe3", list(do.call("lmoms", list(var_para))))
      fit <- do.call("cdfpe3", list(x = var_fit, para = para))
      tibble(para = list(para), fit = list(fit))
    },
    mle = function(var, dist) fn_mle,
    mom = function(var, dist) fn_mom)
  new_dist_fit("distfit_pe3", var = enquo(var), dist = "pe3",
               fn = fn, .n_boot = .n_boot, .boot_seed = .boot_seed)
}

new_dist_fit <- function(type, var, dist, fn, .n_boot, .boot_seed, ...){
  dots <- rlang::list2(...)
  name <- type
  attr(name, "var") <- var
  attr(name, "fn") <- fn
  attr(name, "n_boot") <- .n_boot
  attr(name, "boot_seed") <- .boot_seed
  attr(name, "dist") <- dist
  class(name) <- "dist_fit"
  name
}

globalVariables(c("fn_mle", "fn_mom", "variables", "fit", ".period", ".boot",
                  "raw_data", ".fitted", "para"))
