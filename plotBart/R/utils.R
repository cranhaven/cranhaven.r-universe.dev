
`%notin%` <- Negate(`%in%`)

# coerce_to_logical_(c(1, 0, 'T', FALSE, '0', '1'))
coerce_to_logical_ <- function(x){
  x[x %in% c(1, '1')] <- TRUE
  x[x %in% c(0, '0')] <- FALSE
  x <- as.logical(x)
  if (!is.logical(x) | sum(is.na(x)) > 0) stop("treatment_col must be logical with no NAs")
  return(x)
}

# validate the model is a bartc model
validate_model_ <- function(.model){
  if (!inherits(.model, "bartcFit")) stop(".model must be of class 'bartcFit'")
}

is_numeric_vector_ <- function(x){
  if (!inherits(x, 'numeric')) stop('moderator must be numeric vector')
}

is_discrete_ <- function(x){
  # must be more than one level and all levels can't be unique
  is_discrete <- length(unique(x)) > 1 && length(unique(x)) < length(x)
  if (!isTRUE(is_discrete)) stop('moderator must be discrete')
}

# adjust [moderator] to match estimand
adjust_for_estimand_ <- function(.model, x){
  validate_model_(.model)

  out <- switch(
    .model$estimand,
    ate = x,
    att = x[.model$trt == 1],
    atc = x[.model$trt != 1]
  )

  return(out)
}

# used within plot_moderator_c_pd()
fit_pd_ <- function(x, z1, z0, index, .model){
  z1[, index] <- x
  z0[, index] <- x
  preds.1 <- predict(.model, newdata = z1)
  preds.0 <- predict(.model, newdata = z0)
  preds <- preds.1 - preds.0

  cate <- apply(preds, 1, mean)
  return(cate)
}

pclamp_ <- function(x, x_min, x_max) pmin(x_max, pmax(x, x_min))

# to satisfy CMD CHECK when using pipe variables
if(getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      'x',
      'xend',
      'y',
      'yend',
      'y_new',
      'yend_new',
      'name',
      'value',
      'support_rule',
      'index',
      'threshold',
      'point',
      '.min',
      '.max',
      'label',
      'Z',
      'Z_treat',
      '..count..',
      '..density..',
      'iteration',
      'Chain',
      'icate.o',
      'ci_2.5',
      'ci_97.5',
      'ci_10',
      'ci_90'
    )
  )
}

