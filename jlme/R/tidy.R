#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidier methods for Julia regression models
#'
#' @param x An object of class `jlme`
#' @param effects One of "var_model", "ran_pars", or "fixed"
#' @param ... Unused
#'
#' @name jlme_tidiers
#' @return A data frame
NULL

#' @rdname jlme_tidiers
#' @method tidy jlme
#' @export
tidy.jlme <- function(x, effects = c("var_model", "ran_pars", "fixed"), ...) {
  out <- as.data.frame(JuliaConnectoR::juliaCall("coeftable", x))[, 1:5]
  colnames(out) <- c("term", "estimate", "std.error", "statistic", "p.value")
  out$term <- backtrans_interaction(out$term)
  is_mixed <- is_jl(x, "MixedModel")
  effects <- match.arg(effects)
  if (is_mixed && effects != "fixed") {
    vc <- jl_get(JuliaConnectoR::juliaCall("VarCorr", x))[[1]]
    re_flatten <- lapply(vc, function(g) lapply(g, unlist))
    re_sd <- lapply(re_flatten, function(g) {
      stats::setNames(g[[1]], paste0("sd__", backtrans_interaction(names(g[[1]]))))
    })
    re_cor <- lapply(re_flatten, function(g) {
      re_terms <- backtrans_interaction(names(g[[1]]))
      re_term_matrix <- outer(re_terms, re_terms, function(i, j) {
        vapply(seq_along(i), function(ind) {
          paste0(sort(c(i[ind], j[ind]))[1], ".", sort(c(i[ind], j[ind]))[2])
        }, character(1))
      })
      re_cor_terms <- re_term_matrix[lower.tri(re_term_matrix)]
      if (!is.null(g[[2]])) {
        stats::setNames(g[[2]], paste0("cor__", re_cor_terms))
      }
    })
    re <- Filter(Negate(is.null), c(re_sd, re_cor))
    re_dfs <- lapply(seq_along(re), function(i) {
      data.frame(group = names(re)[i], term = names(re[[i]]), estimate = unname(re[[i]]))
    })
    re_df <- do.call(rbind, re_dfs)
    re_df <- re_df[order(gsub("(sd|cor)__", "", re_df$term)), ]
    sigma <- x$sigma %||% NA
    if (!is.na(sigma)) {
      re_df <- rbind(re_df, data.frame(group = "Residual", term = "sd__Observation", estimate = sigma))
    }
    if (effects == "ran_pars") {
      out <- cbind(effect = "ran_pars", re_df)
    } else {
      out$effect <- "fixed"
      out$group <- NA_character_
      re_df$effect <- "ran_pars"
      re_df[, setdiff(names(out), names(re_df))] <- NA
      out <- rbind(out, re_df)[, c("effect", "group", "term", "estimate", "std.error", "statistic", "p.value")]
    }
    zerocorr <- (out$effect == "ran_pars") & (out$estimate == 0) & grepl("cor__", out$term)
    out <- out[!zerocorr, ]
    if (effects == "ran_pars") {
      out$effect <- NULL
    }
  }
  out$term <- gsub(" & ", ":", out$term)
  out$term <- gsub(": ", "", out$term)
  maybe_as_tibble(out)
}

#' @importFrom generics glance
#' @export
generics::glance

#' @rdname jlme_tidiers
#' @method glance jlme
#' @export
glance.jlme <- function(x, ...) {
  is_mixed <- is_jl(x, "MixedModel")
  is_reml <- is_mixed && x$optsum$REML
  nobs <- JuliaConnectoR::juliaCall("nobs", x)
  sigma <- if (is_mixed) {
    x$sigma %||% NA
  } else {
    has_dispersion <- JuliaConnectoR::juliaCall("GLM.dispersion_parameter", x)
    if (has_dispersion) JuliaConnectoR::juliaLet("dispersion(x.model)", x = x) else NA
  }
  ll <- if (is_reml) {
    list(logLik = NA, AIC = NA, BIC = NA)
  } else {
    list(
      logLik = JuliaConnectoR::juliaCall("loglikelihood", x),
      AIC = JuliaConnectoR::juliaCall("aic", x),
      BIC = JuliaConnectoR::juliaCall("bic", x)
    )
  }
  deviance <- JuliaConnectoR::juliaCall("deviance", x)
  dof <- JuliaConnectoR::juliaCall("dof", x)
  out <- data.frame(
    nobs = nobs, df = dof, sigma = sigma,
    logLik = ll$logLik, AIC = ll$AIC, BIC = ll$BIC,
    deviance = deviance, df.residual = nobs - dof
  )
  maybe_as_tibble(out)
}

backtrans_interaction <- function(x) {
  gsub("__", ":", x, fixed = TRUE)
}

maybe_as_tibble <- function(x) {
  if ("tibble" %in% loadedNamespaces()) {
    rownames(x) <- NULL
    class(x) <- c("tbl_df", "tbl", class(x))
  }
  x
}

resolve_effects <- function(x, effects) {
  switch(effects,
    var_model = x,
    fixed = {
      res <- x[x$effect == "fixed",]
      res[c("effect", "group")] <- NULL
      res
    },
    ran_pars = {
      res <- x[x$effect == "ran_pars",]
      res$effect <- NULL
      res
    }
  )
}
