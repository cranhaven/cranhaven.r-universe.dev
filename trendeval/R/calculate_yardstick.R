calculate_yardstick <- function(x, truth, estimate, metric, na.rm, as_tibble) {
  # for some reason match.fun was not working in some situations, likely due
  # to the funky messing around with environments.
  #fun <- match.fun(metric)
  fun <- switch(
    metric,
    mae_vec = mae_vec,
    rmse_vec = rmse_vec,
    rsq_vec = rsq_vec,
    stop("invalid function call in calculate_yardstick")
  )

  f <- make_catcher(fun)
  metric <- sub("_vec", "", metric, perl = TRUE)
  truth <- x[[truth]]
  estimate <- x[[estimate]]
  res <- f(truth = truth, estimate = estimate, na.rm = na.rm)
  if (as_tibble) {
    result <- res$result
    if (is.null(result)) result <- NA_real_
    out <- tibble(
      metric = metric,
      result = result,
      warnings = list(res$warnings),
      errors = list(res$errors)
    )
  } else {
    out <- append(list(metric = metric), res)
  }
  out
}

# -------------------------------------------------------------------------

calculate_yardstick_trending_fit <- function(x, new_data, na.rm, as_tibble, metric) {
  if (missing(new_data)) new_data <- get_fitted_data(x)
  pred <- predict(x, new_data, add_pi = FALSE)
  result <- get_result(pred)
  truth <- try_na(try(get_response(result), silent = TRUE))
  estimate <- try_na(try(get_estimate(result), silent = TRUE))
  calculate_yardstick(
    x = result,
    truth = truth,
    estimate = estimate,
    metric = metric,
    na.rm = na.rm,
    as_tibble = as_tibble
  )
}

# -------------------------------------------------------------------------

calculate_yardstick_trending_fit_tbl <- function(x, new_data, na.rm, metric, ...) {
  metric <- metric
  pred <- predict(x, new_data, add_pi = FALSE)
  result <- get_result(pred)
  truth <- try_na(try(get_response(x), silent = TRUE))
  res <- .mapply(
    FUN = calculate_yardstick,
    dots = list(
      x = result,
      truth = truth
    ),
    MoreArgs = list(
      estimate = "estimate",
      metric = metric,
      na.rm = na.rm,
      as_tibble = TRUE
    )
  )
  out <- do.call(rbind, res)
  nm_var <- attr(x, "model_name")
  nms <- if (is.null(nm_var)) paste0("model_", 1:nrow(x)) else x[[nm_var]]
  tibble(model_name = nms, out)
}

# -------------------------------------------------------------------------

calculate_yardstick_trending_predict <- function(x, na.rm, as_tibble, metric) {
  result <- get_result(x)
  truth <- try_na(try(get_response(x), silent = TRUE))
  estimate <- try_na(try(get_estimate(x), silent = TRUE))
  calculate_yardstick(
    x = result,
    truth = truth,
    estimate = estimate,
    metric = metric,
    na.rm = na.rm,
    as_tibble = as_tibble
  )
}

# -------------------------------------------------------------------------

calculate_yardstick_trending_predict_tbl <- function(x, na.rm, metric, ...) {
  result <- get_result(x)
  truth <- try_na(try(get_response(x), silent = TRUE))
  estimate <- try_na(try(get_estimate(x), silent = TRUE))
  res <- .mapply(
    FUN = calculate_yardstick,
    dots = list(
      x = result,
      truth = truth,
      estimate = estimate
    ),
    MoreArgs = list(
      metric = metric,
      na.rm = na.rm,
      as_tibble = TRUE
    )
  )
  do.call(rbind, res)
}


# -------------------------------------------------------------------------

calculate_yardstick_trending_model <- function(x, data, na.rm, as_tibble, metric) {
  fitt <- fit(x, data)
  calculate_yardstick_trending_fit_tbl(
    fitt,
    new_data = data,
    na.rm = na.rm,
    as_tibble = as_tibble,
    metric = metric)
}

