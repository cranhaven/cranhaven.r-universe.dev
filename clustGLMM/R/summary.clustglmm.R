summary.clustglmm <- function(object, ...) {
  
  coda <- as_coda(object, ...)
  
  # computes necessary outputs for summary
  coda_summary <- fnames <- gnames <- cnames <- lnames <- list()
  for (ch in object$chains) {
    if (anyNA(coda[[ch]])) {
      stats <- data.frame(Mean = apply(coda[[ch]], 2, mean, na.rm = TRUE),
                          SD = apply(coda[[ch]], 2, sd, na.rm = TRUE))
      quants <- t(apply(coda[[ch]], 2, quantile, 
                        probs = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = TRUE))
      coda_summary[[ch]] <- list(statistics = stats,
                                 quantiles = quants,
                                 start = 1,
                                 end = dim(coda[[ch]])[1],
                                 thin = 1,
                                 nchain = 1)
    } else {
      coda_summary[[ch]] <- summary(coda[[ch]])
    }
  
    settings <- object$settings[[ch]]
    output <- ifelse(length(intersect(rownames(settings),
                                      c("deviance", "dev_i", "pUig_int"))) > 0,
                     "probs", "mcmc")
  
    if (output == "probs") {
      fnames[[ch]] <- c("deviance")
      gnames[[ch]] <- object$param_names[[ch]][["pUig_int"]]
      cnames[[ch]] <- c()
      lnames[[ch]] <- object$param_names[[ch]][["dev_i"]]
    } else {
      fset <- lset <- settings[settings$save & !settings$gspec & settings$dims>0, ]
      fset <- fset[setdiff(rownames(fset), c("loglik", "b", "U", "Gplus", "e0", "naY")), ]
      fnames[[ch]] <- unlist(lapply(rownames(fset), function(p) object$param_names[[ch]][[p]]))
      lnames[[ch]] <- unlist(lapply(intersect(rownames(lset), c("b", "U", "naY")),
                                    function(p) object$param_names[[ch]][[p]]))
      
      gnames[[ch]] <- rownames(coda_summary[[1]]$statistics)[
        grep("\\(1\\)", rownames(coda_summary[[1]]$statistics))]
      gnames[[ch]] <- gnames[[ch]][!(grepl("^w", gnames[[ch]]) |
                                     grepl("^ng", gnames[[ch]]) |
                                     grepl("^pUig", gnames[[ch]]))]
      
      cnames[[ch]] <- c("Gplus", "e0", paste0("w(", 1:object$G[ch], ")"))
    }
  }
  
  res <- list(call = object$call,
              G = object$G,
              modeGplus = object$modeGplus,
              clustering = object$clustering,
              certainty = object$certainty,
              clusters = object$clusters,
              coda_summary = coda_summary,
              output = output,
              fnames = fnames,
              gnames = gnames,
              lnames = lnames,
              cnames = cnames)
  
  class(res) <- "summary.clustglmm"
  return(res)
}
