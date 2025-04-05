#' Profile the likelihood surface of Julia mixed effects models
#'
#' @param x A Julia MixedModel of class `jlme`
#' @param ... Not implemented
#'
#' @return MixedModels.profile() output as object of class `jlmeprof`
#' @export
#'
#' @examples
#' \donttest{
#' jlme_setup(restart = TRUE)
#'
#' jmod <- jlmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#' tidy(jmod)
#'
#' prof <- profilelikelihood(jmod)
#' prof
#'
#' tidy(prof)
#'
#' stop_julia()
#' }
profilelikelihood <- function(x, ...) {

  stopifnot(is_jlmer(x))

  fn <- JuliaConnectoR::juliaFun("MixedModels.profile")
  prof <- fn(x)

  class(prof) <- c("jlmeprof", class(prof))
  attr(prof, "jmod") <- x
  prof

}

#' @rdname jlme_tidiers
#' @method tidy jlmeprof
#' @export
tidy.jlmeprof <- function(x, effects = c("var_model", "ran_pars", "fixed"),
                          ...) {

  stopifnot(
    "`x` must be output of `profilelikelihood()`" = inherits(x, "jlmeprof")
  )
  effects <- match.arg(effects)

  res <- JuliaConnectoR::juliaCall("MixedModels.confint", x)
  res_list <- lapply(jl_get(res)$data, `[[`, "values")
  res_list$par <- NULL

  tidied <- tidy(attr(x, "jmod"))
  tidied <- tidied[!grepl(x = tidied$term, "cor__"), ]
  par_keep <- c(
    which(tidied$effect == "fixed"),
    which(tidied$group == "Residual"),
    utils::head(grep(x = tidied$term, "sd__"), -1)
  )
  tidied_keep <- tidied[par_keep, c("effect", "group", "term")]

  res_tbl <- cbind(tidied_keep, as.data.frame(res_list))
  row_residual <- which(res_tbl$group == "Residual")
  res_tbl <- maybe_as_tibble(
    rbind(res_tbl[-row_residual,], res_tbl[row_residual,])
  )

  # Repair (fixed upstream in MixedModels#785)
  lower_upper <- res_tbl[, c("lower", "upper")]
  res_tbl$conf.low <- do.call(pmin, lower_upper)
  res_tbl$conf.high <- do.call(pmax, lower_upper)
  res_tbl[, c("lower", "upper")] <- NULL

  resolve_effects(res_tbl, effects)


}
