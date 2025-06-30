
# generic S3 method -------------------------------------------------------

"plot.fusionModel" <- function(x, posterior = TRUE, interactive = TRUE, ...){
  op <- par(no.readonly = TRUE)
  if (inherits(x$model, "inla")){
    plotINLA(x, posterior = posterior, interactive = interactive)
  } else {
    plotStan(x, posterior = posterior, interactive = interactive)
  }
  on.exit(par(op))
}

# INLA model plot ----------------------------------------------------------

plotINLA <- function(fusion.model, posterior, interactive){

  model <- fusion.model$model
  mesh <- fusion.model$mesh
  data <- fusion.model$data

  if (!posterior){
    message("Showing mesh overlaying with spatial domain:\n")
    message("Mesh is shown in black, domain is shown in red, geostatistical data (if any) is shown as black dots, point pattern data (if any) is shown as blue dots.")
    plot(mesh, main = NULL)
    plot(data$domain, add = T, border = "red")
    if (data$n_point_var > 0) points(data$locs_point, pch = 20, col = "black")
    if (data$n_pp_var > 0){
      for (i in 1:data$n_pp_var) points(eval(parse(text = paste0("data$locs_pp",i))), pch =20, col = "blue")
    }
  } else {
    priors <- fusion.model$priors

    # update name to match formula
    if (data$n_point_var + data$n_area_var > 0){
      names.fixed <- c(if (data$n_point_var > 0) rep(c(rep("intercept", attr(terms(data$geo.formula), "intercept")),
                                                       attr(terms(data$geo.formula), "term.labels")), data$n_point_var),
                       if (data$n_area_var > 0) rep(c(rep("intercept", attr(terms(data$lattice.formula), "intercept")),
                                                      attr(terms(data$lattice.formula), "term.labels")), data$n_area_var))
      names(model$marginals.fixed) <- paste0(names.fixed, " (", names(model$marginals.fixed), ")")
    }

    post.fixed <- model$marginals.fixed
    post.latent <- model$marginals.hyperpar
    names(post.latent) <- gsub("Precision for the Gaussian observations", "Gaussian precision", names(post.latent))

    prior.fixed <- getPriorFixedINLA(model$all.hyper$fixed)
    prior.latent <- getPriorLatentINLA(model, priors, data$n_w)

    message("Showing marginal posterior densities of fixed effect coefficients:")

    par(mfrow = getDim(length(post.fixed)), mar = c(4,5,1,1) + 0.1)
    if (length(post.fixed) > 0){
      for (i in 1:length(post.fixed)){
        plot(post.fixed[[i]], type = "l", xlab = names(post.fixed)[i], ylab = "Density")
        eval(parse(text = paste0("curve(",prior.fixed[i],
                                 ",from = min(post.fixed[[i]][,1]), to = max(post.fixed[[i]][,1]), lty = 2, col = 'blue', add = TRUE)")))
      }
    } else {message("no fixed effect coefficients to plot.")}
    if (interactive) readline("Enter <return> to proceed ...")

    message("Showing marginal posterior densities of latent parameters:")
    par(mfrow = getDim(length(post.latent)), mar = c(4,5,1,1) + 0.1)
    for (i in 1:length(post.latent)){
      lims <- getLim(post.latent[[i]])
      plot(post.latent[[i]], type = "l", xlab = names(post.latent)[i], ylab = "Density", xlim = lims)
      eval(parse(text = paste0("curve(",prior.latent[i],
                               ",from = lims[1], to = lims[2], lty = 2, col = 'blue', add = TRUE)")))
    }
  }
  invisible()
}

# Stan model plot ----------------------------------------------------------

plotStan <- function(fusion.model, posterior, interactive){
  model <- fusion.model$model
  data <- fusion.model$data

  pars.fixed <- c(grep("beta",model@model_pars, value = T))
  pars.latent <- c("phi",grep("Z_[1-9]+",model@model_pars, value = T))
  if (fusion.model$data$n_norm > 0) pars.latent <- c("tau_sq", pars.latent)

  if (!posterior){
    message("Showing traceplot of fixed effect coefficients:")
    if (length(pars.fixed) > 0){
      print(rstan::traceplot(model, pars.fixed))
    } else {message("no fixed effect coefficients.")}

    if (interactive) readline("Enter <return> to proceed ...")
    message("Showing traceplot of latent parameters:")
    print(rstan::traceplot(model, pars.latent))
  } else {
    priors <- fusion.model$priors

    if (length(pars.fixed) > 0){
      post.fixed <- as.matrix(model, pars = pars.fixed)
    }
    # update name to match formula
    if (data$n_point_var + data$n_area_var > 0){
      names.fixed <- c(if (data$n_point_var > 0) rep(c(rep("intercept", attr(terms(data$geo.formula), "intercept")),
                                                       attr(terms(data$geo.formula), "term.labels")), data$n_point_var),
                       if (data$n_area_var > 0) rep(c(rep("intercept", attr(terms(data$lattice.formula), "intercept")),
                                                      attr(terms(data$lattice.formula), "term.labels")), data$n_area_var))
      colnames(post.fixed) <- paste0(names.fixed, " (", colnames(post.fixed), ")")
    }

    post.latent <- as.matrix(model, pars = pars.latent)

    prior.fixed <- getPriorFixedStan(priors, n.pointbeta = ncol(data$X_point) * ncol(data$Y_point), n.areabeta = ncol(data$X_area) * ncol(data$Y_area))

    message("Showing marginal posterior densities of fixed effect coefficients:")
    if (length(pars.fixed) > 0){
      par(mfrow = getDim(ncol(post.fixed)), mar = c(4,5,1,1) + 0.1)
      for (i in 1:ncol(post.fixed)){
        plot(density(post.fixed[,i]), type = "l", xlab = colnames(post.fixed)[i], ylab = "Density", main = "")
        eval(parse(text = paste0("curve(",prior.fixed[i],
                                 ",from = min(post.fixed[,i]), to = max(post.fixed[,i]), lty = 2, col = 'blue', add = TRUE)")))
      }
    } else {message("no fixed effect coefficients.")}

    if (interactive) readline("Enter <return> to proceed ...")

    prior.latent <- getPriorLatentStan(priors, colnames(post.latent))

    message("Showing marginal posterior densities of latent parameters:")
    par(mfrow = getDim(ncol(post.latent)),  mar = c(4,5,1,1) + 0.1)
    for (i in 1:ncol(post.latent)){
      if (all(post.latent[,i] == 0)){
        plot(density(post.latent[,i]), type = "n", xlab = colnames(post.latent)[i], ylab = "Density", main = "")
      } else {
        plot(density(post.latent[,i]), type = "l", xlab = colnames(post.latent)[i], ylab = "Density", main = "")
        eval(parse(text = paste0("curve(",prior.latent[i],
                                 ",from = min(post.latent[,i]), to = max(post.latent[,i]), lty = 2, col = 'blue', add = TRUE)")))
      }
    }
  }
  invisible()
}

