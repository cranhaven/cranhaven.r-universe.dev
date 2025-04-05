#' Parametric bootstrap for Julia mixed effects models
#'
#' @param x A Julia MixedModel of class `jlme`
#' @param nsim Number of simulations
#' @param seed Seed for the random number generator (Random.MersenneTwister)
#' @param ... Not implemented
#' @param optsum_overrides Values to override in the OptSummary.
#'
#' @return MixedModels.parametricboostrap() output as object of class `jlmeboot`
#' @export
#'
#' @examples
#' \donttest{
#' jlme_setup(restart = TRUE)
#'
#' jmod <- jlmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#' tidy(jmod)
#'
#' samp <- parametricbootstrap(jmod, nsim = 100L, seed = 42L)
#' samp
#'
#' tidy(samp)
#'
#' stop_julia()
#' }
parametricbootstrap <- function(x, nsim, seed, ...,
                                optsum_overrides = list(ftol_rel = 1e-8)) {

  stopifnot(is_jlmer(x))
  jl_require("Random")
  if (!jl("@isdefined _is_logging", .R = TRUE)) {
    # Hack to show progress when called via R
    jl("import MixedModels._is_logging")
    jl("_is_logging(io::Base.PipeEndpoint) = false")
  }

  rng <- jl("Random.MersenneTwister(%i)", as.integer(seed))
  nsim <- jl("%i", as.integer(nsim))

  fn <- JuliaConnectoR::juliaFun("MixedModels.parametricbootstrap")
  samp <- fn(
    rng, nsim, x,
    optsum_overrides = list2ntuple(optsum_overrides)
  )

  class(samp) <- c("jlmeboot", class(samp))
  attr(samp, "jmod") <- x
  samp

}

#' @rdname jlme_tidiers
#' @method tidy jlmeboot
#' @export
tidy.jlmeboot <- function(x, effects = c("var_model", "ran_pars", "fixed"),
                          ...) {

  stopifnot(
    "`x` must be output of `parametricbootstrap()`" = inherits(x, "jlmeboot")
  )
  effects <- match.arg(effects)

  res_list <- jl_get(JuliaConnectoR::juliaCall("MixedModels.shortestcovint", x))
  res <- do.call(rbind.data.frame, res_list)

  beta <- "\u03b2"
  sigma <- "\u03c3"
  rho <- "\u03c1"

  res$group[res$group == "residual"] <- "Residual"
  res$names[res$group == "Residual"] <- "Observation"
  res$effect <- ifelse(res$type == beta, "fixed", "ran_pars")
  res$term <- res$names
  res$term[res$type == sigma] <- paste0("sd__", res$term[res$type == sigma])
  res$term[res$type == rho] <- paste0("cor__", res$term[res$type == rho])
  res$term[res$type == rho] <- gsub(x = res$term[res$type == rho], ", ", ".")
  res$term <- gsub(" & ", ":", res$term)
  res$term <- gsub(": ", "", res$term)
  names(res)[names(res) %in% c("lower", "upper")] <- c("conf.low", "conf.high")

  tidied <- tidy(attr(x, "jmod"))
  keys <- c("effect", "group", "term")

  combined <- maybe_as_tibble(merge(
    tidied[, c(keys, "estimate")],
    res[, c(keys, "conf.low", "conf.high")]
  ))
  combined <- combined[match(tidied$term, combined$term),]

  resolve_effects(combined, effects)

}
