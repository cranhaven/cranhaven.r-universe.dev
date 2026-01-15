# nocov start

make_survival_reg_survival_ln_mixture <- function() {
  parsnip::set_model_engine(
    model = "survival_reg",
    mode = "censored regression",
    eng = "survival_ln_mixture"
  )

  parsnip::set_dependency(
    "survival_reg",
    eng = "survival_ln_mixture", pkg = "lnmixsurv"
  )

  parsnip::set_fit(
    model = "survival_reg",
    eng = "survival_ln_mixture",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "lnmixsurv", fun = "survival_ln_mixture"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "survival_reg",
    eng = "survival_ln_mixture",
    mode = "censored regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "survival_reg",
    eng = "survival_ln_mixture",
    mode = "censored regression",
    type = "hazard",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          type = "hazard",
          eval_time = quote(eval_time)
        )
    )
  )

  parsnip::set_pred(
    model = "survival_reg",
    eng = "survival_ln_mixture",
    mode = "censored regression",
    type = "survival",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          type = "survival",
          eval_time = quote(eval_time),
          interval = quote(interval),
          level = quote(level)
        )
    )
  )
}

make_survival_reg_survival_ln_mixture_em <- function() {
  parsnip::set_model_engine(
    model = "survival_reg",
    mode = "censored regression",
    eng = "survival_ln_mixture_em"
  )

  parsnip::set_dependency(
    "survival_reg",
    eng = "survival_ln_mixture_em", pkg = "lnmixsurv"
  )

  parsnip::set_fit(
    model = "survival_reg",
    eng = "survival_ln_mixture_em",
    mode = "censored regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "lnmixsurv", fun = "survival_ln_mixture_em"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "survival_reg",
    eng = "survival_ln_mixture_em",
    mode = "censored regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "survival_reg",
    eng = "survival_ln_mixture_em",
    mode = "censored regression",
    type = "hazard",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          type = "hazard",
          eval_time = quote(eval_time)
        )
    )
  )

  parsnip::set_pred(
    model = "survival_reg",
    eng = "survival_ln_mixture_em",
    mode = "censored regression",
    type = "survival",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          type = "survival",
          eval_time = quote(eval_time)
        )
    )
  )
}

# nocov end
