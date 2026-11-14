#' Linear mixed model with correlated random effects structure
#'
#' \code{Almer} fits a univariate linear mixed model incorporating a correlated
#' random effects structure. Can be used to fit phylogenetic mixed models and
#' animal models. The function is based on the \code{\link[lme4]{lme4}} package and is
#' very similar to \code{\link[lme4]{lmer}}, apart from the A argument.
#'
#' @param A an optional named list of sparse matrices. The names must correspond
#'   to the names of the random effects in the formula argument. All levels of
#'   the random effect should appear as row and column names for the matrices.
#' @param formula as in \code{\link[lme4]{lmer}}.
#' @param data as in \code{\link[lme4]{lmer}}.
#' @param REML as in \code{\link[lme4]{lmer}}.
#' @param control as in \code{\link[lme4]{lmer}}.
#' @param start as in \code{\link[lme4]{lmer}}.
#' @param verbose as in \code{\link[lme4]{lmer}}.
#' @param weights as in \code{\link[lme4]{lmer}}.
#' @param na.action as in \code{\link[lme4]{lmer}}.
#' @param offset as in \code{\link[lme4]{lmer}}.
#' @param contrasts as in \code{\link[lme4]{lmer}}.
#' @param devFunOnly as in \code{\link[lme4]{lmer}}.
#' @param ... as in \code{\link[lme4]{lmer}}.
#' @return \code{Almer} an object of class \code{\link[lme4]{merMod}}.
#' @author Geir H. Bolstad
#' @examples
#' # See the vignette 'Phylogenetic mixed model'.
#' @importFrom lme4 lFormula lmerControl mkLmerDevfun optimizeLmer mkMerMod
#' @importFrom Matrix Matrix t chol
#' @export
Almer <- function(formula, 
                  data = NULL, 
                  A = list(), 
                  REML = TRUE, 
                  control = lme4::lmerControl(check.nobs.vs.nlev = "ignore",
                                              check.nobs.vs.rankZ = "ignore", 
                                              check.nobs.vs.nRE = "ignore"
                                              ), 
                  start = NULL,
                  verbose = 0L, 
                  weights = NULL, 
                  na.action = "na.omit", 
                  offset = NULL, 
                  contrasts = NULL,
                  devFunOnly = FALSE, 
                  ...) {
  # combine this with function with Almer_SE add a phylogeny argument add a
  # pedigree argument
  mc <- match.call()
  cholA <- lapply(A, function(x) chol(x))
  mod <- 
      lFormula(formula, 
               data,
               REML = REML, 
               weights = weights, 
               na.action = na.action,
               offset = offset, 
               contrasts = contrasts, 
               control = control, 
               ...)
  for (i in seq_along(cholA)) {
    j <- match(names(cholA)[i], names(mod$reTrms$cnms))
    if (length(j) > 1) {
      stop("an A matrix can only be associated with one random effect term")
    }
    ranef_order <- match(rownames(mod$reTrms$Ztlist[[j]]), rownames(cholA[[i]]))
    mod$reTrms$Ztlist[[j]] <- 
        cholA[[i]][ranef_order, ranef_order] %*% mod$reTrms$Ztlist[[j]]
  }
  mod$reTrms$Zt <- do.call(rbind, mod$reTrms$Ztlist)
  devfun <- do.call(mkLmerDevfun, mod)
  opt <- optimizeLmer(devfun, optimizer = "Nelder_Mead", ...)
  mod_obj <- 
      mkMerMod(environment(devfun), 
               opt = opt, 
               reTrms = mod$reTrms, 
               fr = mod$fr,
               mc)
  mod_obj@call <- evalq(mc)
  return(mod_obj)
}

#' Linear mixed model for response variables with uncertainty
#'
#' \code{Almer_SE} Linear mixed model for response variables with uncertainty
#'
#' @param formula as in \code{\link[lme4]{lmer}}.
#' @param SE A vector of standard errors associated with the response variable. NB! Must have column name "SE" in the data.
#' @param maxiter The maximum number of iterations.
#' @param control as in \code{\link[lme4]{lmer}}.
#' @param ... Further optional arguments, see \code{\link{Almer}}.
#' @return \code{Almer_SE} returns an object of class \code{\link[lme4]{merMod}}.
#' @author Geir H. Bolstad
#' @examples
#' # See the vignette 'Phylogenetic mixed model'.
#' @importFrom lme4 VarCorr lmerControl
#' @export
Almer_SE <- function(formula, 
                     SE = NULL, 
                     maxiter = 100, 
                     control = lme4::lmerControl(check.nobs.vs.nlev = "ignore",
                                                 check.nobs.vs.rankZ = "ignore",
                                                 check.nobs.vs.nRE = "ignore"), 
                     ...) {
  if (is.null(SE)) stop("No SE. Use Almer instead")
  mc <- match.call()
  wgth <- 1 / (1 + SE^2)
  mod <- Almer(formula, weights = wgth, control = control, ...)
  for (i in 1:maxiter) {
    rvariance <- attr(lme4::VarCorr(mod), "sc")^2
    wgth <- 1 / (1 + (SE^2) / rvariance)
    mod <- Almer(formula, weights = wgth, ...)
    if (abs(rvariance - attr(lme4::VarCorr(mod), "sc")^2) < 1e-08) {
      (break)()
    }
  }
  if (i == maxiter) {
    warning("Optimization of weights reached maximum number of iterations.")
  }
  mod@call <- evalq(mc)
  mod@frame$SE <- SE
  return(mod)
}

#' Simulate responses from \code{\link{Almer}} fit
#'
#' \code{Almer_sim} Simulate responses from an \code{\link{Almer}} model fit.
#'
#' @param mod A fitted object from \code{\link{Almer}}
#' @param nsim The number of simulations.
#' @details This function is only included because the \code{\link[lme4]{simulate.merMod}}
#'   function did not seem to work properly when the number of random effect
#'   levels equal the number of observations.
#' @return \code{Almer_sim} a matrix of simulated responses, columns correspond
#'   to each simulations.
#' @author Geir H. Bolstad
#' @examples
#' # See the vignette 'Phylogenetic mixed model'.
#' @importFrom lme4 VarCorr ranef fixef
#' @importFrom Matrix t
#' @importFrom stats rnorm residuals
#' @export
Almer_sim <- function(mod, nsim = 1000) {
  samp_size_ranef <- c(sapply(ranef(mod), function(x) length(x[, 1])))
  SD <- as.data.frame(VarCorr(mod))
  ranef_samples_list <- list()
  for (i in names(samp_size_ranef)) {
    rn <- rnorm(samp_size_ranef[i] * nsim, 0, SD[SD$grp == i, "sdcor"])
    ranef_samples_list[[i]] <- matrix(rn, ncol = nsim)
  }
  ranef_samples <- do.call(rbind, ranef_samples_list)
  Ranef <- do.call(cbind, apply(ranef_samples, 2, function(x) {
    t(mod@pp$Zt) %*%
      x
  }))
  Residuals <- 
      rnorm(length(residuals(mod)) * nsim, 
            mean = 0, 
            sd = SD[SD$grp == "Residual", "sdcor"]
            )
  if (any(names(mod@frame) %in% "SE")) {
      Residuals <- Residuals + rnorm(nrow(mod@frame), 0, sd = mod@frame$SE)
  }
  Fixef <- matrix(rep(mod@pp$X %*% fixef(mod), nsim), ncol = nsim)
  y <- Fixef + Ranef + Residuals
  colnames(y) <- paste("Sim", 1:nsim, sep = "_")
  return(y)
}

#' Parametric bootstrap on \code{\link{Almer}} model fit
#'
#' \code{Almer_boot} performs a parametric bootstrap from an \code{\link{Almer}}
#' model fit
#'
#' @param mod A fitted object from \code{\link{Almer}}
#' @param nsim The number of simulations.
#' @return \code{Almer_boot} a list with entries fixef, vcov, fixef_distribution
#'   and vcov_distribution, where the two first entries includes the means,
#'   standard deviations, and quantiles of the fixed effects means and
#'   (co)variances, respectively, and the two latter includes the complete
#'   bootstrap distribution.
#' @author Geir H. Bolstad
#' @examples
#' # See the vignette 'Phylogenetic mixed model'.
#' @importFrom lme4 VarCorr ranef fixef
#' @importFrom Matrix t
#' @importFrom stats update sd quantile
#' @export
Almer_boot <- function(mod, nsim = 1000) {
  y <- Almer_sim(mod, nsim)
  dt <- mod@frame
  boot <- 
      apply(y, 2, 
            function(x) {
                dt$x86eb0d23_ebe6_4b37_bbc7_1d164b2f7c26 <- x
                update(mod, x86eb0d23_ebe6_4b37_bbc7_1d164b2f7c26 ~ .,data = dt)
                }
            )
  fixef_boot <- sapply(boot, lme4::fixef)
  if (is.vector(fixef_boot)) {
    fixef_boot <- cbind(fixef_boot)
  } else {
    fixef_boot <- t(fixef_boot)
  }
  colnames(fixef_boot) <- names(lme4::fixef(boot[[1]]))
  VarCorr_list <- lapply(boot, function(x) as.data.frame(lme4::VarCorr(x)))
  vcov_boot <- t(sapply(VarCorr_list, function(x) x$vcov))
  colnames(vcov_boot) <- VarCorr_list[[1]]$grp
  list(
    fixef = cbind(Mean = apply(fixef_boot, 2, mean), 
                  `Std. Err.` = apply(fixef_boot, 2, sd), 
                  `2.5%` = apply(fixef_boot, 2, quantile, probs = 0.025), 
                  `97.5%` = apply(fixef_boot, 2, quantile, probs = 0.975)
                  ), 
    vcov = cbind(Mean = apply(vcov_boot, 2, mean),
                 `Std. Err.` = apply(vcov_boot, 2, sd), 
                 `2.5%` = apply(vcov_boot, 2, quantile, probs = 0.025), 
                 `97.5%` = apply(vcov_boot, 2, quantile, probs = 0.975)),
    fixef_distribution = fixef_boot, 
    vcov_distribution = vcov_boot
    )
}
