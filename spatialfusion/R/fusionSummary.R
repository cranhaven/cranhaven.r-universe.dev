
# generic S3 method -------------------------------------------------------

"summary.fusionModel" <- function(object, digits = 3, ...){
  if (inherits(object$model, "inla")){
    summaryINLA(object, digits)
  } else {
    summaryStan(object, digits)
  }
}

# INLA model prediction ----------------------------------------------------------

summaryINLA <- function(fusion.model, digits){

  model <- fusion.model$model
  data <- fusion.model$data
  rownames(model$summary.hyperpar) <- gsub("Precision for the Gaussian observations", "Gaussian precision", rownames(model$summary.hyperpar))
  # update name to match formula
  if (data$n_point_var + data$n_area_var > 0){
    names.fixed <- c(if (data$n_point_var > 0) rep(c(rep("intercept", attr(terms(data$geo.formula), "intercept")),
                                                     attr(terms(data$geo.formula), "term.labels")), data$n_point_var),
                     if (data$n_area_var > 0) rep(c(rep("intercept", attr(terms(data$lattice.formula), "intercept")),
                                                   attr(terms(data$lattice.formula), "term.labels")), data$n_area_var))
    rownames(model$summary.fixed) <- paste0(names.fixed, " (", rownames(model$summary.fixed), ")")
  }
  out <- rbind(model$summary.fixed[,-7], model$summary.hyperpar)

  cat("Model:\n")
  cat(paste("geostatistical formula:", deparse(data$geo.formula),"\n"))
  cat(paste("lattice formula:", deparse(data$lattice.formula),"\n"))
  cat(paste("point pattern variables:", data$n_pp_var,"\n"))
  cat(paste("latent process(es):", data$n_w,"\n"))
  cat("--------------\n")
  cat("Fixed effect coefficients:\n")
  print(signif(model$summary.fixed[,-7], digits))
  cat("\nLatent parameters:\n")
  print(signif(model$summary.hyperpar, digits))

  return(invisible(signif(out, digits)))
}

# Stan model prediction ----------------------------------------------------------

summaryStan <- function(fusion.model, digits){

  model <- fusion.model$model
  data <- fusion.model$data

  pars.fixed <- c(grep("beta",model@model_pars, value = T))
  pars.latent <- c("phi",grep("Z_[1-9]+",model@model_pars, value = T))
  if (fusion.model$data$n_norm > 0) pars.latent <- c("tau_sq", pars.latent)

  out <- summary(model, pars = c(pars.fixed, pars.latent))$summary

  # update name to match formula
  if (data$n_point_var + data$n_area_var > 0){
    names.fixed <- c(if (data$n_point_var > 0) rep(c(rep("intercept", attr(terms(data$geo.formula), "intercept")),
                                                     attr(terms(data$geo.formula), "term.labels")), data$n_point_var),
                     if (data$n_area_var > 0) rep(c(rep("intercept", attr(terms(data$lattice.formula), "intercept")),
                                                    attr(terms(data$lattice.formula), "term.labels")), data$n_area_var))
    rownames(out)[1:length(names.fixed)] <- paste0(names.fixed, " (", rownames(out)[1:length(names.fixed)], ")")
  }

  cat("Model:\n")
  cat(paste("geostatistical formula:", deparse(data$geo.formula),"\n"))
  cat(paste("lattice formula:", deparse(data$lattice.formula),"\n"))
  cat(paste("point pattern variables:", data$n_pp_var,"\n"))
  cat(paste("latent process(es):", data$n_w, "\n"))
  cat("--------------\n")
  cat("Fixed effect coefficients:\n")
  if (length(pars.fixed) > 0){
    print(signif(out[1:length(names.fixed),], digits))
  } else {print("NULL")}
  cat("\nLatent parameters:\n")
  if (length(pars.fixed) > 0){
    print(signif(out[-c(1:length(names.fixed)),], digits))
  } else {print(signif(out, digits))}

  return(invisible(signif(out, digits)))
}

