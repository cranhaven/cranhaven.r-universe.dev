#' Re-exported functions for interacting with Julia model objects
#'
#' @name jlme-model-reexports
#'
#' @param x Julia model object
#' @param ... Additional arguments passed to the Julia function
#'
#' @return An appropriate R or Julia object
#'
#' @examplesIf check_julia_ok()
#' \donttest{
#' jlme_setup(restart = TRUE)
#'
#' x <- jlmer(r2 ~ Anger + (1 | id), lme4::VerbAgg, family = "binomial")
#'
#' # `propertynames()` lists properties accessible via `$`
#' propertynames(x)
#'
#' # `issingular()` reports whether model has singular fit
#' issingular(x)
#'
#' # `likelihoodratiotest()` conducts a likelihood-ratio test between nested models
#' likelihoodratiotest(
#'   x,
#'   jlmer(r2 ~ 1 + (1 | id), lme4::VerbAgg, family = "binomial")
#' )
#'
#' stop_julia()
#' }
NULL

#' @rdname jlme-model-reexports
#' @export
propertynames <- function(x) {
  stopifnot(is_jl(x))
  nm <- jl_get(JuliaConnectoR::juliaCall("propertynames", x))
  sort(as.character(nm))
}

#' @rdname jlme-model-reexports
#' @export
issingular <- function(x) {
  stopifnot(is_jl(x, "MixedModel"))
  JuliaConnectoR::juliaCall("MixedModels.issingular", x)
}

#' @rdname jlme-model-reexports
#' @export
likelihoodratiotest <- function(x, ...) {
  stopifnot(is_jl(x))
  model1 <- x
  models_rest <- list(...)
  all_models <- c(list(model1), models_rest)
  fn <- JuliaConnectoR::juliaFun("MixedModels.likelihoodratiotest")
  do.call(fn, all_models)
}
