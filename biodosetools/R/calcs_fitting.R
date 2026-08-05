#' Calculate model statistics
#'
#' @param model_data Data of the model.
#' @param fit_coeffs_vec Vector of fitting coefficients.
#' @param glm_results Results of glm.
#' @param fit_algorithm String of the algorithm used.
#' @param response Type of response.
#' @param link Fit link.
#' @param type Theoretical or raw glm model statistics.
#' @param Y Y response (required in constraint-maxlik-optimization).
#' @param mu mu response required in constraint-maxlik-optimization).
#' @param n number of parameters (required in constraint-maxlik-optimization).
#' @param npar number of parameters (required in constraint-maxlik-optimization).
#' @param genome_factor Genomic conversion factor used in translocations.
#' @param calc_type Calculation type, either "fitting" or "estimation".
#' @param model_family Model family.
#'
#' @return Data frame of model statistics.
calculate_model_stats <- function(model_data, fit_coeffs_vec, glm_results = NULL, fit_algorithm = NULL,
                                  response = "yield", link = c("identity", "log"), type = c("theory", "raw"),
                                  Y = NULL, mu = NULL, n = NULL, npar = NULL,
                                  genome_factor = NULL, calc_type = c("fitting", "estimation"), model_family) {
  # Validate parameters
  link <- match.arg(link)
  type <- match.arg(type)
  calc_type <- match.arg(calc_type)

  # Auxiliary functions
  renormalise_model_data <- function(model_data, genome_factor, calc_type) {
    if (calc_type == "estimation") {
      model_data[["X"]] <- model_data[["X"]] / (model_data[["N"]] * genome_factor)
    } else if (calc_type == "fitting") {
      model_data[["aberr"]] <- model_data[["aberr"]] / model_data[["coeff_C"]]
      model_data[["coeff_alpha"]] <- model_data[["coeff_alpha"]] / model_data[["coeff_C"]]
      model_data[["coeff_beta"]] <- model_data[["coeff_beta"]] / model_data[["coeff_C"]]
      model_data[["coeff_C"]] <- model_data[["coeff_C"]] / model_data[["coeff_C"]]
    }

    return(model_data)
  }

  calculate_eta_sat <- function(model_data, calc_type) {
    if (calc_type == "estimation") {
      eta_sat <- model_data[["X"]]
    } else if (calc_type == "fitting") {
      eta_sat <- model_data[["aberr"]]
    }

    return(eta_sat)
  }

  predict_eta <- function(data, coeffs, calc_type) {
    if (calc_type == "estimation") {
      eta <- coeffs[["coeff_C"]] * rep(1, nrow(data)) +
        coeffs[["coeff_alpha"]] * data[["D"]] +
        coeffs[["coeff_beta"]] * data[["D"]] * data[["D"]]
    } else if (calc_type == "fitting") {
      eta <- coeffs[["coeff_C"]] * data[["coeff_C"]] +
        coeffs[["coeff_alpha"]] * data[["coeff_alpha"]] +
        coeffs[["coeff_beta"]] * data[["coeff_beta"]]
    }

    return(eta)
  }

  # Calculate from theory or use statistics calculated by glm
  if (fit_algorithm == "constraint-maxlik-optimization") {
    # Renormalise data if necessary
    # if (response == "yield") {
    #   model_data <- renormalise_model_data(model_data, genome_factor, calc_type)
    # }

    # Generalised fit coefficients
    general_fit_coeffs <- generalise_fit_coeffs(fit_coeffs_vec)

    eta_sat <- calculate_eta_sat(model_data, calc_type)
    eta <- predict_eta(model_data, general_fit_coeffs, calc_type)

    num_data <- length(eta_sat)
    num_params <- sum(fit_coeffs_vec != 0)

    # Calculate logLik depending on fitting link
    if (link == "identity") {
      logLik <- sum(log(eta) * eta_sat - eta - lfactorial(eta_sat))
    } else if (link == "log") {
      logLik <- sum(eta * eta_sat - exp(eta) - lfactorial(eta_sat))
    }

    # Calculate model-specific statistics
    if(model_family == "quasipoisson"){
      fit_model_statistics <- cbind(
        logLik = NA,
        deviance = sum(2 * (eta_sat * log(eta_sat / eta) - (eta_sat - eta))),
        df = num_data - num_params,
        AIC = NA,
        BIC = NA)
    }else{
      fit_model_statistics <- cbind(
        logLik = logLik,
        deviance = sum(2 * (eta_sat * log(eta_sat / eta) - (eta_sat - eta))),
        df = num_data - num_params,
        AIC = 2 * num_params - 2 * logLik,
        BIC = log(num_data) * num_params - 2 * logLik)
    }
  } else if (fit_algorithm == "glm") {
    # Get model-specific statistics
    fit_model_statistics <- cbind(
      logLik = as.numeric(stats::logLik(glm_results)),
      deviance = stats::deviance(glm_results),
      df = stats::df.residual(glm_results),
      AIC = stats::AIC(glm_results),
      BIC = stats::BIC(glm_results)
    )
  }
    #else if (type == "raw" & fit_algorithm == "constraint-maxlik-optimization") {
  #   # Get model-specific statistics
  #   fit_model_statistics <- cbind(
  #     logLik = stats::logLik(glm_results),
  #     deviance = sum(stats::poisson(link = "identity")$dev.resids(Y, mu, 1)),
  #     df = n - npar,
  #     AIC = 2 * length(fit_coeffs_vec) - 2 * stats::logLik(glm_results),
  #     BIC = log(n) * length(fit_coeffs_vec) - 2 * stats::logLik(glm_results)
  #   )
  # }

  return(fit_model_statistics)
}

#' Prepare count data for max-likelihood optimization fitting
#'
#' @param count_data Count data in data frame form.
#' @param model_formula Model formula.
#' @param aberr_module Aberration module.
#'
#' @return Data frame of parsed count data.
#' @importFrom rlang .data
prepare_maxlik_count_data <- function(count_data, model_formula, aberr_module = c("dicentrics", "translocations", "micronuclei")) {
  # Validate parameters
  aberr_module <- match.arg(aberr_module)

  if (ncol(count_data) > 3 & aberr_module != "translocations") {
    # Full distribution data
    dose_vec <- rep(
      count_data[["D"]],
      count_data[["N"]]
    )

    cell_vec <- rep(
      rep(1, nrow(count_data)),
      count_data[["N"]]
    )

    dics_vec <- rep(
      count_data %>%
        names() %>%
        grep("C", ., value = TRUE) %>%
        gsub("C", "", .) %>%
        rep(nrow(count_data)) %>%
        as.numeric(),
      count_data %>%
        .[, grep("C", names(.), value = TRUE)] %>%
        as.matrix() %>%
        t() %>%
        as.numeric()
    )

    parsed_data <- data.frame(
      aberr = dics_vec,
      dose = dose_vec,
      coeff_C = cell_vec
    ) %>%
      dplyr::mutate(
        coeff_alpha = .data$dose * .data$coeff_C,
        coeff_beta = .data$dose^2 * .data$coeff_C
      ) %>%
      dplyr::select("aberr", "coeff_C", "coeff_alpha", "coeff_beta", "dose")
  } else {
    # Aggregated data only or if using translocations
    parsed_data <- count_data %>%
      dplyr::rename(
        aberr = "X",
        coeff_C = "N"
      ) %>%
      dplyr::mutate(
        coeff_alpha = .data$D * .data$coeff_C,
        coeff_beta = .data$D^2 * .data$coeff_C
      ) %>%
      dplyr::select("aberr", "coeff_C", "coeff_alpha", "coeff_beta")
  }

  # Return data frame
  return(parsed_data)
}

#' Perform GLM (Generalised Linear Model) fitting
#'
#' Method based on the paper by Edwards, A. A. et al. (1979). Radiation induced
#' chromosome aberrations and the Poisson distribution. Radiation and
#' Environmental Biophysics, 16(2), 89-100. <doi:10.1007/BF01323216>.
#'
#' @param count_data Count data in data frame form.
#' @param model_formula Model formula.
#' @param model_family Model family.
#' @param fit_link Family link.
#' @param aberr_module Aberration module.
#'
#' @return List object containing GLM fit results.
fit_glm_method <- function(count_data, model_formula,
                           model_family = c("automatic", "poisson", "quasipoisson", "nb2"), fit_link = "identity",
                           aberr_module = c("dicentrics", "translocations", "micronuclei")) {
  # Validate parameters
  model_family <- match.arg(model_family)
  aberr_module <- match.arg(aberr_module)
  # Store fit algorithm as a string
  fit_algorithm <- "glm"

  # Parse count data
  doses <- count_data[["D"]]
  aberr <- count_data[["X"]]
  cells <- count_data[["N"]]

  # Construct predictors and model data
  coeff_C <- cells
  coeff_alpha <- cells * doses
  coeff_beta <- cells * doses * doses
  model_data <- list(coeff_C = coeff_C, coeff_alpha = coeff_alpha, coeff_beta = coeff_beta, aberr = aberr)

  # Select model formula
  parsed_model_formula <- parse_model_formula(model_formula)
  fit_formula_raw <- parsed_model_formula$fit_formula_raw
  fit_formula_tex <- parsed_model_formula$fit_formula_tex
  fit_formula <- stats::as.formula(fit_formula_raw)

  # Perform automatic fit calculation
  if (model_family == "automatic") {
    # Automatic (quasi-Poisson) model
    fit_results <- stats::glm(
      formula = fit_formula,
      family = stats::quasipoisson(link = fit_link),
      data = model_data
    )
    fit_dispersion <- summary(fit_results)$dispersion
    fit_final_model <- "quasipoisson"

    # Check if Poisson model is more suitable
    if (fit_dispersion <= 1) {
      fit_results <- stats::glm(
        formula = fit_formula,
        family = stats::poisson(link = fit_link),
        data = model_data
      )
      fit_dispersion <- NULL
      fit_final_model <- "poisson"
    }
  } else if (model_family == "poisson") {
    # Poisson model
    fit_results <- stats::glm(
      formula = fit_formula,
      family = stats::poisson(link = fit_link),
      data = model_data
    )
    fit_dispersion <- NULL
    fit_final_model <- "poisson"
  } else if (model_family == "quasipoisson") {
    # Quasi-Poisson model
    fit_results <- stats::glm(
      formula = fit_formula,
      family = stats::quasipoisson(link = fit_link),
      data = model_data
    )
    fit_dispersion <- summary(fit_results)$dispersion
    fit_final_model <- "quasipoisson"
  } else if (model_family == "nb2") {
    fit_results <- MASS::glm.nb(
      formula = fit_formula,
      link = fit_link,
      data = model_data
    )
    fit_dispersion <- summary(fit_results)$dispersion
    fit_final_model <- "nb2"
  }

  # Summarise fit
  fit_summary <- summary(fit_results, correlation = TRUE)
  fit_cor_mat <- fit_summary$correlation
  fit_var_cov_mat <- stats::vcov(fit_results)
  fit_coeffs_vec <- stats::coef(fit_results)


  # Model-specific statistics
  fit_model_statistics <- calculate_model_stats(
    model_data = model_data, fit_coeffs_vec = fit_coeffs_vec,
    glm_results = fit_results, fit_algorithm = fit_algorithm,
    response = "yield", link = "identity", type = "theory",
    Y = NULL, mu = NULL, n = NULL, npar = NULL,
    genome_factor = NULL, calc_type = "fitting"
  )

  # Correct p-values depending on model dispersion
  t_value <- fit_coeffs_vec / sqrt(diag(fit_var_cov_mat))

  # Make coefficients table
  if (fit_final_model == "poisson") {
    # For Poisson model
    fit_coeffs <- cbind(
      estimate = fit_coeffs_vec,
      std.error = sqrt(diag(fit_var_cov_mat)),
      statistic = t_value,
      p.value = 2 * stats::pnorm(-abs(t_value))
    ) %>%
      `row.names<-`(names(fit_coeffs_vec)) %>%
      `colnames<-`(c("estimate", "std.error", "statistic", "p.value"))

    # Summary of model used
    fit_model_summary <- paste("A Poisson model assuming equidispersion was used as the model dispersion <= 1.")
  } else if (fit_final_model == "quasipoisson") {
    # For quasi-Poisson model
    fit_coeffs <- cbind(
      estimate = fit_coeffs_vec,
      std.error = sqrt(diag(fit_var_cov_mat)),
      statistic = t_value,
      p.value = 2 * stats::pt(-abs(t_value), fit_results$df.residual)
    ) %>%
      `row.names<-`(names(fit_coeffs_vec)) %>%
      `colnames<-`(c("estimate", "std.error", "statistic", "p.value"))

    # Summary of model used
    if (aberr_module != "micronuclei") {
      fit_model_summary <- paste0("A quasi-Poisson model accounting for overdispersion was used as the model dispersion (=", round(fit_dispersion, 2), ") > 1.")
    } else {
      fit_model_summary <- paste0("A quasi-Poisson model was used, with a model dispersion of ", round(fit_dispersion, 2), ".")
    }
  } else if (fit_final_model == "nb2") {
    # For Poisson model
    fit_coeffs <- cbind(
      estimate = fit_coeffs_vec,
      std.error = sqrt(diag(fit_var_cov_mat)),
      statistic = t_value,
      p.value = 2 * stats::pnorm(-abs(t_value))
    ) %>%
      `row.names<-`(names(fit_coeffs_vec)) %>%
      `colnames<-`(c("estimate", "std.error", "statistic", "p.value"))

    # Summary of model used
    fit_model_summary <- paste0("A negative binomial (NB2) model was used, with a model dispersion of ", round(fit_dispersion, 2), ".")
  }

  # Return objects
  fit_results_list <- list(
    # Raw data
    fit_raw_data = as.matrix(count_data),
    # Formulas
    fit_formula_raw = fit_formula_raw,
    fit_formula_tex = fit_formula_tex,
    # Coefficients
    fit_coeffs = fit_coeffs,
    fit_cor_mat = fit_cor_mat,
    fit_var_cov_mat = fit_var_cov_mat,
    # Model statistics
    fit_dispersion = fit_dispersion,
    fit_model_statistics = fit_model_statistics,
    # Algorithm and model summary
    fit_algorithm = "glm",
    fit_model_summary = fit_model_summary
  )

  return(fit_results_list)
}

#' Perform max-likelihood optimization fitting
#'
#' Method based on the paper by Oliveira, M. et al. (2016). Zero-inflated
#' regression models for radiation-induced chromosome aberration data:
#' A comparative study. Biometrical Journal, 58(2), 259-279.
#' <doi:10.1002/bimj.201400233>.
#'
#' @param data Count data.
#' @param model_formula Model formula.
#' @param model_family Model family.
#' @param fit_link Family link.
#' @param aberr_module Aberration module.
#'
#' @return List object containing maxLik fit results.
fit_maxlik_method <- function(data, model_formula,
                              model_family = c("automatic", "poisson", "quasipoisson", "nb2"), fit_link,
                              aberr_module = c("dicentrics", "translocations", "micronuclei")) {
  # Validate parameters
  model_family <- match.arg(model_family)
  aberr_module <- match.arg(aberr_module)

  # Store fit algorithm as a string
  fit_algorithm <- "constraint-maxlik-optimization"

  # Parse full data into aggregated format
  if ("dose" %in% colnames(data)) {
    data_aggr <- data %>%
      dplyr::group_by(.data$aberr, .data$dose) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::group_by(.data$dose) %>%
      dplyr::summarise(
        coeff_C = sum(.data$n),
        X = sum(ifelse(.data$aberr > 0, n * .data$aberr, 0))
      ) %>%
      dplyr::mutate(
        coeff_alpha = .data$dose * .data$coeff_C,
        coeff_beta = .data$dose^2 * .data$coeff_C
      ) %>%
      dplyr::rename(aberr = "X") %>%
      dplyr::select("aberr", "dose", "coeff_C", "coeff_alpha", "coeff_beta")
  } else {
    data_aggr <- data
  }

  # Select model formula
  parsed_model_formula <- parse_model_formula(model_formula)
  fit_formula_raw <- parsed_model_formula$fit_formula_raw
  fit_formula_tex <- parsed_model_formula$fit_formula_tex
  fit_formula <- stats::as.formula(fit_formula_raw)

  # Find starting values for the mean
  mustart <- stats::lm(fit_formula, data = data_aggr)$coefficients
  if (mustart[1] <= 0) {
    mustart[1] <- 0.001
  }

  # Extract the model frame from fitting formula
  mf <- match.call()
  m <- match(c("formula", "data"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE

  if (length(fit_formula[[3]]) > 1 & identical(fit_formula[[3]][[1]], as.name("|"))) {
    ff <- fit_formula
    fit_formula[[3]][1] <- call("+")
    mf$formula <- fit_formula
    ffc <- . ~ .
    ffz <- ~.
    ffc[[2]] <- ff[[2]]
    ffc[[3]] <- ff[[3]][[2]]
    ffz[[3]] <- ff[[3]][[3]]
    ffz[[2]] <- NULL
  } else {
    ffz <- ffc <- ff <- fit_formula
    ffz[[2]] <- NULL
  }

  if (inherits(try(stats::terms(ffz), silent = TRUE), "try-error")) {
    ffz <- eval(parse(text = sprintf(paste("%s -", deparse(ffc[[2]])), deparse(ffz))))
  }

  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mtX <- stats::terms(ffc, data = data)
  mtZ <- stats::terms(ffz, data = data)
  mtZ <- stats::terms(stats::update(mtZ, ~.), data = data)
  X <- stats::model.matrix(mtX, mf)
  Z <- stats::model.matrix(mtZ, mf)
  Y <- stats::model.response(mf, "numeric")

  if (all(X[, 1] == 1)) {
    intercept <- TRUE
  } else {
    intercept <- FALSE
  }

  # Summarise model frame
  # ndic <- max(Y)
  n <- length(Y)
  npar <- NCOL(X)
  grad <- NULL

  # Find starting values for the mean
  if (fit_link == "log") {
    if (is.null(mustart)) mustart <- as.numeric(stats::glm.fit(X, Y, family = stats::poisson())$coefficients)
  } else {
    if (is.null(mustart)) {
      stop("If link=identity, starting values must be provided")
    } else {
      mustart <- mustart
    }
  }

  # Model constraints (A theta + B > 0)
  if (intercept) {
    A_constraint <- rbind(X, c(1, rep(0, npar - 1)))
    B_constraint <- rep(0, n + 1)
  } else {
    A_constraint <- X
    B_constraint <- rep(0, n)
  }

  if (fit_link == "log") {
    constraints <- NULL
  } else {
    constraints <- list(ineqA = A_constraint, ineqB = B_constraint)
  }

  # Loglikelihood function
  loglik <- function(parms) {
    if (fit_link == "log") {
      mu <- as.vector(exp(X %*% parms[1:npar]))
    } else {
      mu <- as.vector(X %*% parms[1:npar])
    }
    loglikh <- sum(-mu + Y * log(mu) - lgamma(Y + 1))

    return(loglikh)
  }

  # Perform fitting
  fit_results <- maxLik::maxLik(
    logLik = loglik,
    grad = grad,
    start = mustart,
    constraints = constraints,
    iterlim = 1000
  )
  hess <- maxLik::hessian(fit_results)

  if (fit_link == "log") {
    mu <- as.vector(exp(X %*% fit_results$estimate[1:npar]))
  } else {
    mu <- as.vector(X %*% fit_results$estimate[1:npar])
  }

  # Summarise fit
  fit_var_cov_mat <- base::solve(-hess)
  fit_coeffs_vec <- fit_results$estimate
  fit_dispersion <- sum(((Y - mu)^2) / (mu * (n - npar)))

  # Model-specific statistics
  fit_model_statistics <- calculate_model_stats(
    model_data = data_aggr, fit_coeffs_vec = fit_coeffs_vec,
    glm_results = fit_results, fit_algorithm = fit_algorithm,
    response = "yield", link = "identity", type = "theory",
    Y = Y, mu = mu, n = n, npar = npar,
    genome_factor = NULL, calc_type = "fitting", model_family
  )


  # Correct p-values depending on model dispersion
  if (model_family == "poisson" | (model_family == "automatic" & fit_dispersion <= 1)) {
    t_value <- fit_coeffs_vec / sqrt(diag(fit_var_cov_mat))

    # For Poisson model
    fit_coeffs <- cbind(
      estimate = fit_coeffs_vec,
      std.error = sqrt(diag(fit_var_cov_mat)),
      statistic = t_value,
      p.value = 2 * stats::pnorm(-abs(t_value))
    ) %>%
      `row.names<-`(names(fit_coeffs_vec))

    # Summary of model used
    fit_model_summary <- paste("A Poisson model assuming equidispersion was used as the model dispersion <= 1.")
  } else if (model_family == "quasipoisson" | (model_family == "automatic" & fit_dispersion > 1)) {
    fit_var_cov_mat <- fit_var_cov_mat * fit_dispersion
    t_value <- fit_coeffs_vec / sqrt(diag(fit_var_cov_mat))

    # For quasi-Poisson model
    fit_coeffs <- cbind(
      estimate = fit_coeffs_vec,
      std.error = sqrt(diag(fit_var_cov_mat)),
      statistic = t_value,
      p.value = 2 * 2 * stats::pt(-abs(t_value), as.numeric(fit_model_statistics[, "df"]))
    ) %>%
      `row.names<-`(names(fit_coeffs_vec))

    # Summary of model used
    fit_model_summary <- paste0("A quasi-Poisson model accounting for overdispersion was used as the model dispersion (=", round(fit_dispersion, 2), ") > 1.")
  } else if (model_family == "nb2") {
    # TODO: update coefficients for NB2
    fit_var_cov_mat <- fit_var_cov_mat * fit_dispersion
    t_value <- fit_coeffs_vec / sqrt(diag(fit_var_cov_mat))

    # For quasi-Poisson model
    fit_coeffs <- cbind(
      estimate = fit_coeffs_vec,
      std.error = sqrt(diag(fit_var_cov_mat)),
      statistic = t_value,
      p.value = 2 * 2 * stats::pt(-abs(t_value), as.numeric(fit_model_statistics[, "df"]))
    ) %>%
      `row.names<-`(names(fit_coeffs_vec))

    # Summary of model used
    fit_model_summary <- paste("WIP: A negative binomial (NB2) model was used, with a model dispersion of ", round(fit_dispersion, 2), ".")
  }

  # Calculate correlation matrix
  fit_cor_mat <- fit_var_cov_mat
  for (x_var in rownames(fit_var_cov_mat)) {
    for (y_var in colnames(fit_var_cov_mat)) {
      fit_cor_mat[x_var, y_var] <- fit_var_cov_mat[x_var, y_var] / (fit_coeffs[x_var, "std.error"] * fit_coeffs[y_var, "std.error"])
    }
  }

  # Return objects
  fit_results_list <- list(
    # Raw data
    fit_raw_data = as.matrix(data_aggr),
    # Formulas
    fit_formula_raw = fit_formula_raw,
    fit_formula_tex = fit_formula_tex,
    # Coefficients
    fit_coeffs = fit_coeffs,
    fit_cor_mat = fit_cor_mat,
    fit_var_cov_mat = fit_var_cov_mat,
    # Model statistics
    fit_dispersion = fit_dispersion,
    fit_model_statistics = fit_model_statistics,
    # Algorithm and model summary
    fit_algorithm = fit_algorithm,
    fit_model_summary = fit_model_summary
  )

  return(fit_results_list)
}

#' Perform dose-effect fitting algorithm
#'
#' Perform dose-effect fitting. A generalized linear model (GLM) is used by
#' default, with a maximum likelihood estimation (MLE) as a fallback method.
#'
#' The GLM method is based on the paper by Edwards, A. A. et al. (1979).
#' Radiation induced chromosome aberrations and the Poisson distribution.
#' Radiation and Environmental Biophysics, 16(2), 89-100.
#' <doi:10.1007/BF01323216>.
#'
#' The MLE method is based on the paperby Oliveira, M. et al. (2016).
#' Zero-inflated regression models for radiation-induced chromosome aberration
#' data: A comparative study. Biometrical Journal, 58(2), 259-279.
#' <doi:10.1002/bimj.201400233>.
#'
#' @param count_data Count data in data frame form.
#' @param model_formula Model formula.
#' @param model_family Model family.
#' @param fit_link Family link.
#' @param aberr_module Aberration module.
#' @param algorithm Optional selection of algorithm to be used, either "glm" (for GLM) or "maxlik" (for MLE). By default, "glm" is used, with "maxlik" as a fallback method.
#'
#' @return List object containing fit results either using GLM or maxLik optimization.
#' @example man/examples/fit_example.R
#' @export
fit <- function(count_data, model_formula,
                model_family, fit_link = "identity",
                aberr_module = c("dicentrics", "translocations", "micronuclei"),
                algorithm = c("glm", "maxlik")) {
  # Validate parameters
  algorithm <- match.arg(algorithm)
  aberr_module <- match.arg(aberr_module)

  if (algorithm == "maxlik") {
    # Perform fitting via maxlik method
    prepared_data <- prepare_maxlik_count_data(count_data, model_formula, aberr_module)
    fit_results_list <- fit_maxlik_method(prepared_data, model_formula, model_family, fit_link, aberr_module)
    fit_results_list[["fit_raw_data"]] <- as.matrix(count_data)

    return(fit_results_list)
  }

  # If glm() produces an error, constraint ML maximization is performed
  tryCatch(
    {
      # Perform fitting via glm()
      fit_results_list <- fit_glm_method(count_data, model_formula, model_family, fit_link, aberr_module)

      return(fit_results_list)
    },
    error = function(error_message) {
      cli::cli_alert_warning("Problem with {.fn glm} -> constraint ML optimization will be used instead")

      # Perform fitting via maxlik method
      prepared_data <- prepare_maxlik_count_data(count_data, model_formula, aberr_module)
      fit_results_list <- fit_maxlik_method(prepared_data, model_formula, model_family, fit_link, aberr_module)
      fit_results_list[["fit_raw_data"]] <- as.matrix(count_data)

      return(fit_results_list)
    }
  )
}
