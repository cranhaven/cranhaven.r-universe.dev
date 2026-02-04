#' Coefficient estimation in the longitudinal model
#'
#' This function offers a collection of methods of coefficient estimation in a
#' longitudinal model with possibly informative observation time.
#' These methods include Standard linear mixed-effect model (`standard_LME`),
#' Linear mixed-effect model adjusted for the historical number of visits (`VA_LME`),
#' Joint model of the visiting process and the longitudinal process accounting for measured confounders (`JMVL_LY`),
#' Inverse-intensity-rate-ratio weighting approach (`IIRR_weighting`),
#' Joint model of the visiting process and the longitudinal process with dependent latent variables (`JMVL_Liang`),
#' Imputation-based approach with linear mixed-effect model (`imputation_LME`), and
#' Joint model of the visiting process and the longitudinal process with a shared random intercept (`JMVL_G`).
#'
#' @param long_data Longitudinal dataset
#' @param method The following methods are available:
#' - `standard_LME`: Standard linear mixed-effect model.
#' - `VA_LME`: Linear mixed-effect model adjusted for the historical number of visits.
#' - `JMVL_LY`: Joint model of the visiting process and the longitudinal process accounting for measured confounders.
#' - `IIRR_weighting`: Inverse-intensity-rate-ratio weighting approach.
#' - `JMVL_Liang`: Joint model of the visiting process and the longitudinal process with dependent latent variables.
#' - `imputation_LME`: Imputation-based approach with linear mixed-effect model.
#' - `JMVL_G`: Joint model of the visiting process and the longitudinal process with a shared random intercept.
#' @param id_var Variable for the subject ID to indicate the grouping
#'   structure.
#' @param time Variable for the observational time.
#' @param outcome_var Variable name for the longitudinal outcome variable.
#' @param LM_fixedEffect_variables Vector input of variable names with fixed
#'   effects in the longitudinal model. Variables should not contain `time`.
#' @param LM_randomEffect_variables Vector input of variable names with random
#'   effects in the longitudinal model. This argument is `NULL` for methods
#'   including `JMVL_LY`, `JMVL_G` and `IIRR_weighting`.
#' @param VPM_variables Vector input of variable names in the visiting process
#'   model.
#' @param imp_time_factor Scale factor for the time variable. This argument is
#'   only needed in the imputation-based methods i.e., `imputation_LME`.
#' @param optCtrl Control parameters for running the mixed-effect model. See
#'   the `control` argument in [lme4::lmer()].
#' @param control Control parameters for the `JMVL_G` method:
#'
#' - `verbose`: `TRUE` or `FALSE` for outputting checkpoint after each iteration. Default is `FALSE`.
#' - `tol`: Tolerance for convergence.
#' - `GHk`: Number of gaussian-hermite quadrature points. Default is `10`.
#' - `maxiter`: Maximum number of iteration. Default is `150`.
#'
#' @param ... Additional arguments to [nleqslv::nleqslv()].
#' @import dplyr
#'
#' @return `beta_hat`: Estimated coefficients in the longitudinal model.
#'
#' Other output in each method:
#' * `standard_LME`:
#'     * `beta_sd`: Standard deviation of the estimated coefficients.
#' * `VA_LME`:
#'     * `beta_sd`: Standard deviation of the estimated coefficients.
#' * `JMVL_LY`:
#'    * `gamma_hat`: Estimated coefficients in the visiting process model.
#' * `IIRR_weighting`:
#'    * `gamma_hat`: Estimated coefficients in the visiting process model.
#' * `JMVL_Liang`:
#'    * `gamma_hat`: Estimated coefficients in the visiting process model.
#' @export
#'
#' @references
#'
#' Buzkova, P. and Lumley, T. (2007). Longitudinal data analysis for generalized linear models with follow-up dependent
#' on outcome-related variables. Canadian Journal of Statistics, 35(4):485–500.
#'
#' Gasparini, A., Abrams, K. R., Barrett, J. K., Major, R. W., Sweeting, M. J., Brunskill, N. J., and Crowther, M. J. (2020).
#' Mixed-effects models for health care longitudinal data with an informative visiting process: A monte carlo simulation
#' study. Statistica Neerlandica, 74(1):5–23.
#'
#' Liang, Y., Lu, W., and Ying, Z. (2009). Joint modeling and analysis of longitudinal data with informative observation
#' times. Biometrics, 65(2):377–384.
#'
#' Lin, D. Y. and Ying, Z. (2001). Semiparametric and nonparametric regression analysis of longitudinal data. Journal of
#' the American Statistical Association, 96(453):103–126.
#'
#' @examples
#' # Setup arguments
#' train_data
#'
#' time_var = "time"
#' id_var = "id"
#' outcome_var = "Y"
#' VPM_variables = c("Z", "X")
#' LM_fixedEffect_variables = c("Z", "X")
#' LM_randomEffect_variables = "Z"
#'
#' # Run the standard LME model
#' fit_standardLME = long_est(long_data=train_data,
#'                            method="standard_LME",
#'                            id_var=id_var,
#'                            outcome_var=outcome_var,
#'                            LM_fixedEffect_variables = LM_fixedEffect_variables,
#'                            time = time_var,
#'                            LM_randomEffect_variables = LM_randomEffect_variables,
#'                            VPM_variables = VPM_variables)
#' # Return the coefficient estimates
#' fit_standardLME$beta_hat
#'
#' # Run the VA_LME model
#' fit_VALME = long_est(long_data=train_data,
#'                      method="VA_LME",
#'                      id_var=id_var,
#'                      outcome_var=outcome_var,
#'                      LM_fixedEffect_variables = LM_fixedEffect_variables,
#'                      time = time_var,
#'                      LM_randomEffect_variables = LM_randomEffect_variables,
#'                      VPM_variables = VPM_variables)
#' # Return the coefficient estimates
#' fit_VALME$beta_hat
long_est <- function(long_data,
                     method,
                     id_var,
                     outcome_var,
                     LM_fixedEffect_variables = NULL,
                     time = NULL,
                     LM_randomEffect_variables = NULL,
                     VPM_variables = NULL,
                     imp_time_factor = NULL,
                     optCtrl = list(method = "nlminb", kkt = FALSE, tol = 0.2, maxit = 20000),
                     control = list(verbose = FALSE,
                                    tol = 1e-3,
                                    GHk = 10,
                                    maxiter = 150),
                     ...) {

  # stopifnot("Variable name for the observational time (`time`) must be named as 'time'."  != (time=="time"))

  colnames(long_data)[which(colnames(long_data) == id_var)] <- "id"
  colnames(long_data)[which(colnames(long_data) == outcome_var)] <- "Y"
  colnames(long_data)[which(colnames(long_data) == time)] <- "time"
  LM_fixedEffect_withTime_variables <- c(LM_fixedEffect_variables, "time")

  if (method == "standard_LME") {
    # Check Inputs
    stopifnot("`LM_fixedEffect_variables` must be provided." = !is.null(LM_fixedEffect_variables))
    stopifnot("`LM_randomEffect_variables` must be provided." = !is.null(LM_randomEffect_variables))

    lme_model_formula <- paste(
      "Y ~",
      paste(LM_fixedEffect_withTime_variables, collapse = "+"),
      # paste(LM_fixedEffect_variables, collapse = "+"),"+time",
      "+(1+",
      paste(LM_randomEffect_variables, collapse = "+"),
      "|id)"
    )
    control_params <- lme4::lmerControl(optimizer = "optimx", optCtrl = optCtrl)
    lme_model <- suppressWarnings(lme4::lmer(lme_model_formula,
                                             data = long_data,
                                             REML = FALSE,
                                             control = lme4::lmerControl(optCtrl = control_params)))
    # Print beta_hat
    beta_hat <- summary(lme_model)$coef[, 1]
    beta_sd <- summary(lme_model)$coef[, 2]
    names(beta_hat)[which(names(beta_hat) == "time")] <- time
    names(beta_sd)[which(names(beta_sd) == "time")] <- time

    # Include beta_hat and beta_sd in a result list
    result <- list(
      beta_hat = beta_hat,
      beta_sd = beta_sd
    )

    return(result)
  } else if (method == "VA_LME") {
    # Check Inputs
    stopifnot("`LM_fixedEffect_variables` must be provided." = !is.null(LM_fixedEffect_variables))
    stopifnot("`LM_randomEffect_variables` must be provided." = !is.null(LM_randomEffect_variables))

    # add the visit number
    long_data <- long_data %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(n_visits = 1:length(id))

    lme_model_formula <- paste(
      "Y ~",
      paste(LM_fixedEffect_withTime_variables, collapse = "+"),
      "+n_visits",
      "+(1+",
      paste(LM_randomEffect_variables, collapse = "+"),
      "|id)"
    )
    control_params <- lme4::lmerControl(optimizer = "optimx", optCtrl = optCtrl)
    lme_model <- suppressWarnings(lme4::lmer(lme_model_formula,
                            data = long_data, REML = FALSE,
                            control = lme4::lmerControl(optCtrl = control_params)
    ))
    # Print beta_hat
    beta_hat <- summary(lme_model)$coef[, 1]
    beta_sd <- summary(lme_model)$coef[, 2]
    names(beta_hat)[which(names(beta_hat) == "time")] <- time
    names(beta_sd)[which(names(beta_sd) == "time")] <- time

    # Include beta_hat and beta_sd in a result list
    result <- list(
      beta_hat = beta_hat,
      beta_sd = beta_sd
    )

    return(result)
  } else if (method == "JMVL_LY") {
    # Check Inputs
    stopifnot("`VPM_variables` must be provided." = !is.null(VPM_variables))
    stopifnot("`time` must be provided." = !is.null(time))

    data <- long_data

    # variables in the visiting process
    V <- data[, c("id", VPM_variables)] # row = N
    VV <- as.matrix(V[!duplicated(V), -1]) # row = m
    V <- as.matrix(V[, VPM_variables]) # variables affecting the visiting process

    # variables in the longitudinal process
    X <- data[, c("id", LM_fixedEffect_variables)]
    XX <- as.matrix(X[!duplicated(X), -1])
    X <- as.matrix(X[, LM_fixedEffect_variables])

    temp <- data %>%
      dplyr::group_by(id) %>%
      dplyr::summarize(C = max(time), ni = dplyr::n())
    C <- rep(max(na.omit(data$time)), nrow(VV)) # censoring time, in our case, it is the same for all subjects

    ni <- temp$ni # number of observations per subject
    id.unique <- unique(data$id)
    id <- data$id
    Time <- data$time
    Time.unique <- sort(unique(Time), decreasing = FALSE)
    y <- data$Y

    dNit <- as.numeric(!is.na(data$Y))


    message("Estimating parameters in the VP model...")
    gamma.est.fun <- function(V, VV) {
      # data,data_base,vp_pred
      m <- nrow(VV)
      VV <- as.matrix(VV)

      gamma_function <- function(gamma) {
        gamma <- matrix(gamma, ncol = 1)
        # compute the average value
        exp.gamma <- exp(as.matrix(VV) %*% gamma)
        V.denom <- rep(sum(na.omit(exp.gamma)), length(Time))
        V.numer <- matrix(rep(apply(na.omit(as.vector(exp.gamma)) * VV, 2, sum), length(Time)), nrow = length(Time), byrow = TRUE)
        V.weightedAve <- V.numer / V.denom
        (obj <- apply(V - V.weightedAve, 2, mean))
      }

      gamma <- rep(0, ncol(V))
      gamma.solve <- nleqslv::nleqslv(x = gamma, fn = gamma_function, ..., control = list(trace = 0))
      (gamma_hat <- gamma.solve$x)

      return(gamma_hat)
    }

    (gamma_hat <- gamma.est.fun(V, VV))
    names(gamma_hat) <- colnames(V)

    exp.gamma <- as.vector(exp(VV %*% gamma_hat))
    y.data <- data.frame(id, Time, y)
    y.star.fun <- function(t) {
      y.data1 <- y.data %>%
        dplyr::mutate(time_diff = abs(Time - t)) %>%
        dplyr::group_by(id) %>%
        dplyr::summarize(y_star = y[which.min(time_diff)][1])
      return(y.data1$y_star)
    }


    message("Neighborhood smoothing...")
    y.bar.numer <- sapply(Time.unique, function(u) {
      sum(na.omit(y.star.fun(u) * exp.gamma))
    })
    y.bar.denom <- rep(sum(exp.gamma), length(Time.unique))
    y.bar.ordered <- y.bar.numer / y.bar.denom
    y.bar <- y.bar.ordered[match(Time, Time.unique)]

    X.bar.ordered <- t(sapply(Time.unique, function(t) {
      X.bar.numer <- c(colSums(XX * exp.gamma * (t <= C)))
      X.bar.denom <- sum(exp.gamma * (t <= C))
      X.bar.numer / X.bar.denom
    }))

    X.bar.denom <- rep(sum(exp.gamma), length(Time.unique))
    # replicate X.numer.ave to match the length of Time.unique
    X.bar.numer <- matrix(rep(apply(XX * exp.gamma, 2, sum), length(Time.unique)), nrow = length(Time.unique), byrow = TRUE)
    X.bar.ordered <- X.bar.numer / X.bar.denom
    X.bar <- X.bar.ordered[match(Time, Time.unique), ]


    message("Estimating parameters in the LP model...")
    LY.function <- function(beta) {
      res <- as.vector((y - y.bar) - (X - X.bar) %*% beta)
      (obj <- apply(X - X.bar, 2, function(x) {
        sum(na.omit(x * res))
      }))
      return(obj)
    }

    beta <- matrix(rep(0, ncol(X)), ncol = 1)
    beta_hat_LY.solve <- nleqslv::nleqslv(x = beta, fn = LY.function, ..., control = list(trace = 0))
    (beta_hat_LY <- beta_hat_LY.solve$x)
    names(beta_hat_LY) <- colnames(X)

    results <- list(
      gamma_hat = gamma_hat,
      beta_hat = beta_hat_LY
    )

    return(results)
  } else if (method == "IIRR_weighting") {
    # Check Inputs
    stopifnot("`VPM_variables` must be provided." = !is.null(VPM_variables))
    stopifnot("`LM_fixedEffect_variables` must be provided." = !is.null(LM_fixedEffect_variables))


    data <- long_data

    # variables in the visiting process
    V <- data[, c("id", VPM_variables)] # row = n
    VV <- as.matrix(V[!duplicated(V), -1]) # row=m
    V <- as.matrix(V[, VPM_variables]) # variables affecting the visiting process

    # variables in the longitudinal process
    X <- data[, c("id", LM_fixedEffect_variables)]
    XX <- as.matrix(X[!duplicated(X), -1])
    X <- as.matrix(X[, LM_fixedEffect_variables])

    temp <- data %>%
      dplyr::group_by(id) %>%
      dplyr::summarize(C = max(time), ni = dplyr::n())
    C <- rep(max(na.omit(data$time)), nrow(VV)) # censoring time
    ni <- temp$ni # number of observations per subject
    id.unique <- unique(data$id)
    id <- data$id
    Time <- data$time
    Time.unique <- sort(unique(Time), decreasing = FALSE)
    y <- data$Y
    dNit <- as.numeric(!is.na(data$Y))


    message("Estimating parameters in the VP model...")
    gamma.est.fun <- function(V, VV) {
      # data,data_base,vp_pred
      m <- nrow(VV)
      VV <- as.matrix(VV)

      gamma_function <- function(gamma) {
        gamma <- matrix(gamma, ncol = 1)
        # compute the average value
        exp.gamma <- exp(as.matrix(VV) %*% gamma)

        V.denom <- rep(sum(na.omit(exp.gamma)), length(Time))
        V.numer <- matrix(rep(apply(na.omit(as.vector(exp.gamma)) * VV, 2, sum), length(Time)), nrow = length(Time), byrow = TRUE)

        V.weightedAve <- V.numer / V.denom
        (obj <- apply(V - V.weightedAve, 2, mean))
      }

      gamma <- rep(0, ncol(V))
      gamma.solve <- nleqslv::nleqslv(x = gamma, fn = gamma_function, ..., control = list(trace = 0))
      (gamma_hat <- gamma.solve$x)

      return(gamma_hat)
    }

    gamma_hat <- gamma.est.fun(V, VV)
    names(gamma_hat) <- colnames(V)

    ###### weightedGEE with the intercept and time ######
    message("Estimating parameters in the LP model...")
    weights <- as.vector(exp(V %*% gamma_hat))
    X.GEE <- as.matrix(data[, c(LM_fixedEffect_withTime_variables)])
    X.GEE <- cbind(1, X.GEE)
    Y.function <- function(beta) {
      res <- as.vector(1 / weights * (y - (X.GEE %*% beta)))
      (obj <- apply(X.GEE, 2, function(x) {
        sum(na.omit(x * res))
      }))
      return(obj)
    }
    beta <- matrix(rep(0.1, ncol(X.GEE)), ncol = 1)
    beta_hat_weightedGEE.solve <- nleqslv::nleqslv(x = beta, fn = Y.function, ..., control = list(trace = 0))
    (beta_hat_weightedGEE <- beta_hat_weightedGEE.solve$x)
    names(beta_hat_weightedGEE) <- colnames(X.GEE)
    names(beta_hat_weightedGEE)[1] <- "(intercept)"
    names(beta_hat_weightedGEE)[which(names(beta_hat_weightedGEE)=="time")] <- time_var

    results <- list(
      gamma_hat = gamma_hat,
      beta_hat = beta_hat_weightedGEE
    )

    return(results)
  } else if (method == "JMVL_Liang") {
    # Check Inputs
    stopifnot("`VPM_variables` must be provided." = !is.null(VPM_variables))
    stopifnot("`time` must be provided." = !is.null(time))
    stopifnot("`LM_randomEffect_variables` must be provided." = !is.null(LM_randomEffect_variables))

    data <- long_data

    # variables in the visiting process
    V <- data[, c("id", VPM_variables)] # row = n
    VV <- as.matrix(V[!duplicated(V), -1]) # row=m
    V <- as.matrix(V[, VPM_variables]) # variables affecting the visiting process

    # variables in the longitudinal process
    X <- data[, c("id", LM_fixedEffect_variables)]
    XX <- as.matrix(X[!duplicated(X), -1])
    X <- as.matrix(X[, LM_fixedEffect_variables])

    # variables with random effect in the longitudinal process
    Q <- data[, LM_randomEffect_variables, drop = FALSE] # variables with random effect
    dNit <- as.numeric(!is.na(data$Y))

    temp <- as.data.frame(data) %>%
      dplyr::group_by(id) %>%
      dplyr::summarize(C = max(time), ni = sum(!is.na(Y)))
    C <- rep(max(na.omit(data$time)), nrow(VV)) # censoring time
    ni <- temp$ni # number of observations per subject
    id.unique <- unique(data$id)
    id <- data$id
    Time <- data$time
    Time.unique <- sort(unique(Time), decreasing = FALSE)
    y <- data$Y

    message("Estimating parameters in the VP model...")
    gamma.est.fun <- function(V, VV) {
      m <- nrow(VV)
      VV <- as.matrix(VV)

      gamma_function <- function(gamma) {
        gamma <- matrix(gamma, ncol = 1)
        # compute the average value
        exp.gamma <- exp(as.matrix(VV) %*% gamma)

        V.denom <- rep(sum(na.omit(exp.gamma)), length(Time))
        V.numer <- matrix(rep(apply(na.omit(as.vector(exp.gamma)) * VV, 2, sum), length(Time)), nrow = length(Time), byrow = TRUE)

        V.weightedAve <- V.numer / V.denom
        (obj <- apply(V - V.weightedAve, 2, mean))
      }

      gamma <- rep(0.1, ncol(V))
      gamma.solve <- nleqslv::nleqslv(x = gamma, fn = gamma_function, ..., control = list(trace = 0))
      (gamma_hat <- gamma.solve$x)

      return(gamma_hat)
    }

    (gamma_hat <- gamma.est.fun(V, VV))
    names(gamma_hat) <- colnames(V)

    # Lambda_estimation
    exp_gamma <- as.vector(exp(VV %*% gamma_hat))
    denom_Lambda <- rep(sum(exp_gamma), length(Time.unique))
    Time_freqtable <- table(Time)
    Time_freqtable1 <- Time_freqtable[match(as.character(Time.unique), names(Time_freqtable))]
    Lambda_t_est <- as.numeric(Time_freqtable1 / denom_Lambda)
    Lambda_C_est <- rep(sum(Lambda_t_est[Time.unique <= C[1]]), length(C))

    # sigma^2 estimation
    exp_gamma_Lambda <- exp_gamma * Lambda_C_est
    sigma_hat_sq <- max((sum(ni^2 - ni - exp_gamma_Lambda^2) / sum(exp_gamma_Lambda^2)), 0)

    # B estimation
    denom <- rep(sum(ni / Lambda_C_est), length(Time))

    Exp_eta <- ((1 + ni * sigma_hat_sq) / (1 + exp_gamma_Lambda * sigma_hat_sq) - 1)
    id_freqtable <- table(id)
    id_freqtable1 <- id_freqtable[match(as.character(id.unique), names(id_freqtable))]
    Exp_eta_long <- rep(Exp_eta, id_freqtable1)
    # if time is not included in Q
    if ("time" %in% colnames(Q)) {
      # remove time column in Q
      Q_noTime <- Q[, -which(colnames(Q) == "time"), drop = FALSE]
      Q0_noTime <- cbind(1, Q_noTime)
      Bhat <- Exp_eta_long * Q0_noTime
      Bhat_base0 <- cbind(data$id, Bhat)
      Bhat_base <- Bhat_base0[!duplicated(Bhat_base0), -1]
      numer_B1 <- apply(Bhat_base, 2, function(x) {
        rep(sum(na.omit(ni * x / Lambda_C_est)), length(Time))
      })
      numer_BTime <- unlist(lapply(Time, function(x) {
        sum(na.omit(ni * x * Exp_eta / Lambda_C_est))
      }))
      B_bar <- cbind(numer_B1 / denom, numer_BTime / denom)
      apply(na.omit(B_bar), 2, sd)
    } else {
      Q0 <- cbind(1, Q)
      Bhat <- Exp_eta_long * Q0
      Bhat_base0 <- cbind(data$id, Bhat)
      Bhat_base <- Bhat_base0[!duplicated(Bhat_base0), -1]
      numer_B <- apply(Bhat_base, 2, function(x) {
        rep(sum(na.omit(ni * x / Lambda_C_est)), length(Time))
      })
      B_bar <- numer_B / denom
    }
    Bhat <- Exp_eta_long * cbind(1, Q)

    # X estimation
    numer_X <- apply(XX, 2, function(x) {
      rep(sum(ni * x / Lambda_C_est), length(Time))
    })
    X_bar <- numer_X / denom

    message("Estimating parameters in the LP model...")
    if (sigma_hat_sq != 0) {
      design_M <- as.matrix(cbind(X, Bhat))
      design_Mbar <- as.matrix(cbind(X_bar, B_bar))
      Liang.function <- function(beta) {
        tmp1 <- (design_M - design_Mbar) * as.vector(data$Y - design_M %*% beta)
        value <- apply(na.omit(tmp1), 2, sum)
        return(value)
      }
      beta <- as.matrix(rep(0.1, ncol(design_M)), ncol = 1)
      beta.solve <- nleqslv::nleqslv(x = beta, fn = Liang.function, ..., control = list(trace = 0))
      (beta.hat <- beta.solve$x[1:ncol(X)])
    }

    if (sigma_hat_sq == 0) {
      design_M <- as.matrix(cbind(X))
      design_Mbar <- as.matrix(cbind(X_bar))
      Liang.function <- function(beta) {
        tmp1 <- (design_M - design_Mbar) * as.vector(data$Y - design_M %*% beta)
        value <- apply(na.omit(tmp1), 2, sum)
        return(value)
      }
      beta <- as.matrix(rep(0, ncol(design_M)), ncol = 1)
      beta.solve <- nleqslv::nleqslv(x = beta, fn = Liang.function, ..., control = list(trace = 0))
      (beta.hat <- beta.solve$x[1:ncol(X)])
    }
    names(beta.hat) <- colnames(X)
    (beta.hat)

    results <- list(
      gamma_hat = gamma_hat,
      beta_hat = beta.hat
    )

    return(results)
  } else if (method == "imputation_LME") {
    # Check Inputs
    stopifnot("`imp_time_factor` must be provided." = !is.null(imp_time_factor))
    stopifnot("`LM_fixedEffect_variables` must be provided." = !is.null(LM_fixedEffect_variables))
    stopifnot("`LM_randomEffect_variables` must be provided." = !is.null(LM_randomEffect_variables))
    stopifnot("`time` must be provided." = !is.null(time))

    data <- long_data
    if (is.null(imp_time_factor)) {
      imp_time_factor <- 1
      data3 <- data
    } else {
      # If the imp_time_factor is specified and is not 1
      # imp_time_factor = 0.5
      # group the data based on the time factor
      data <- data %>%
        dplyr::mutate(time_new = ceiling(time / imp_time_factor)) %>%
        dplyr::group_by(id, time_new) %>%
        dplyr::mutate(Y_mean = mean(Y, na.rm = TRUE)) %>%
        # mutate(Y_mean = ifelse(is.numeric(Y_mean),Y_mean,NA)) %>%
        dplyr::select(id, Y_mean, time_new, setdiff(names(data), c("id", "Y", "time"))) %>%
        dplyr::ungroup()

      colnames(data)[2:3] <- c("Y", "time")

      # create a new dataset with the same columns in data1. For a given id, the time is from 1 to max_time. If there is no value of Y at a given time, then Y is NA.
      max_time <- max(data$time, na.rm = TRUE)
      id_all <- rep(unique(data$id), each = max_time)
      time_all <- rep(as.numeric(1:max_time), length(unique(data$id)))
      data_base <- data %>%
        dplyr::select(-Y, -time) %>%
        unique()
      data_full <- data_base %>%
        dplyr::slice(rep(1:dplyr::n(), each = max_time)) %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(time = rep(1:max_time))

      # merge two datasets, this is the dataset to be imputed
      data3 <- dplyr::left_join(data_full, data, by = setdiff(names(data_full), "Y"))

      # rescale the time variable for imputation and analysis
      data3$time <- data3$time * imp_time_factor
    }

    message("start imputation...")
    # empty imputation
    imp0 <- mice::mice(as.matrix(data3), maxit = 0)
    predM <- imp0$predictorMatrix
    impM <- imp0$method

    # specify predictor matrix and method
    predM1 <- predM
    predM1["Y", "id"] <- -2
    predM1["Y", LM_fixedEffect_withTime_variables] <- 1 # fixed x effects imputation
    impM1 <- impM
    impM1["Y"] <- "2l.lmer"

    # multilevel imputation
    imp1 <- mice::mice(as.matrix(data3),
                       m = 5,
                       predictorMatrix = predM1, method = impM1, maxit = 5
    )

    lme_imp <- function(data_imp) {
      lme_model_formula <- paste("Y ~", paste(LM_fixedEffect_withTime_variables, collapse = "+"), "+(1+", paste(LM_randomEffect_variables, collapse = "+"), "|id)")
      lme_model <- suppressWarnings(lme4::lmer(lme_model_formula,
                              data = data_imp, REML = TRUE,
                              control = lme4::lmerControl(optCtrl = list(optimizer = "optimx", optCtrl = list(method = "L-BFGS-B"), maxfun = 50000))
      ))
      (beta_hat <- summary(lme_model)$coef[, 1])
    }

    message("Imputation finished.")

    fit <- lapply(1:5, function(i) lme_imp(mice::complete(imp1, action = i)))
    beta_hat <- sapply(seq_along(fit[[1]]), function(i) mean(sapply(fit, `[`, i)))
    names(beta_hat) <- names(fit[[1]])
    names(beta_hat)[which(names(beta_hat) == "time")] <- time

    results <- list(beta_hat = beta_hat)

    return(results)
  } else if (method == "JMVL_G") {
    # Check Inputs
    stopifnot("`VPM_variables` must be provided." = !is.null(VPM_variables))
    stopifnot("`time` must be provided." = !is.null(time))
    stopifnot("`LM_fixedEffect_variables` must be provided." = !is.null(LM_fixedEffect_variables))

    time.end <- max(long_data$time, na.rm = TRUE)
    long_data_org <- long_data

    # Check if long_data has measurements when time=0. If so, stop the program
    if (sum(long_data$time == 0, na.rm = TRUE) > 0) {
      stop("There are measurements when time=0. Please remove them.")
    }

    # Data preparation: VP data
    vp_data_censor <- long_data_org %>%
      dplyr::select(c("id", all_of(VPM_variables))) %>%
      dplyr::group_by(id) %>%
      dplyr::slice(dplyr::n()) %>%
      dplyr::mutate(time = max(long_data_org$time, na.rm = TRUE))

    vp_data0 <- long_data_org[, c("id", VPM_variables, "time")]
    vp_data1 <- merge(vp_data0, vp_data_censor, by = c("id", VPM_variables, "time"), all = TRUE) %>%
      dplyr::arrange(id, time)

    # create three intervisit time interval
    vp_data <- vp_data1 %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(d = ifelse(time == time.end, 0, 1)) %>%
      dplyr::mutate(
        # time0 = lag(time), # Note: this was a bug
        time0 = ifelse(is.na(lag(time)),0,lag(time)),
        s = time - time0
      ) %>%
      dplyr::select(id, all_of(VPM_variables), s, d) %>%
      dplyr::filter(s != 0) |>
      na.omit()

    # Data preparation: LP data
    long_data_censor <- long_data_org %>%
      dplyr::select(c("id", all_of(LM_fixedEffect_variables))) %>%
      dplyr::group_by(id) %>%
      dplyr::slice(dplyr::n()) %>%
      dplyr::mutate(time = max(long_data_org$time, na.rm = TRUE), Y = NA)

    long_data1 <- merge(long_data_org, long_data_censor, by = c("id", LM_fixedEffect_withTime_variables), all = TRUE) %>%
      dplyr::arrange(id, time)

    long_data <- long_data1 %>%
      dplyr::filter(!is.na(time)) %>%
      dplyr::select(id, all_of(LM_fixedEffect_withTime_variables), Y.x) %>%
      dplyr::rename(Y = Y.x)

    message("Initialize the parameters...")
    # fit an lme object
    lme_model_fixed_formula <- as.formula(paste0("Y~", paste0(LM_fixedEffect_withTime_variables, collapse = "+")))
    lme_model_random_formula <- as.formula(paste("~1|id"))
    lme_model <- nlme::lme(lme_model_fixed_formula, data = na.omit(long_data), random = lme_model_random_formula, method = "ML", control = nlme::lmeControl(opt = "optim", msMaxIter = 5000, msMaxEval = 5000))
    lmeObject <- lme_model

    initial_values <- NULL
    initial_values$betas <- coef(summary(lme_model))[, 1]
    initial_values$sigma2_e <- (lme_model$sigma)^2
    initial_values$sigma2_b <- 0.5^2
    initial_values$rho <- 0

    vp_model_formula <- as.formula(paste0("survival::Surv(s, d)~", paste(VPM_variables, collapse = "+")))
    vp_model <- survival::survreg(vp_model_formula, data = vp_data, dist = "exponential")
    # Note that when setting dist="exponential", the link function is "log" of the expected of the survival time, not the rate.
    # therefore, I need to take the negative of the coefficients
    initial_values$gammas <- -coef(summary(vp_model))

    # input
    y <- long_data$Y
    X <- cbind(1, long_data[, c(LM_fixedEffect_withTime_variables)]) |> as.matrix()
    s <- vp_data$s
    d <- vp_data$d
    W <- cbind(1, vp_data[, c(VPM_variables)]) |> as.matrix()
    id <- long_data$id

    # sample size settings
    ncx <- ncol(X)
    ncw <- if (is.null(W)) 0 else ncol(W)
    N <- length(y)
    n <- length(unique(long_data$id))
    ni <- as.vector(tapply(id, id, length))

    # crossproducts and others
    ncz <- 1

    # Gauss-Hermite quadrature rule components
    gh_rule <- statmod::gauss.quad(control$GHk)
    b <- gh_rule$nodes
    wGH <- gh_rule$weights
    b <- as.matrix(expand.grid(rep(list(b), ncz)))
    wGH <- as.matrix(expand.grid(rep(list(wGH), ncz)))
    wGH <- 2^(ncz / 2) * apply(wGH, 1, prod) * exp(rowSums(b * b))
    b <- sqrt(2) * b

    # initial values
    betas <- as.vector(initial_values$betas)
    sigma2_e <- initial_values$sigma2_e
    gammas <- as.vector(initial_values$gammas)
    sigma2_b <- initial_values$sigma2_b
    rho <- initial_values$rho

    ############# direct solve the score function ##########
    iter <- control$maxiter
    Y_mat <- matrix(0, iter + 1, ncx + 1)
    S_mat <- matrix(0, iter + 1, ncw)
    B_mat <- matrix(0, iter + 1, ncz)
    Rho_mat <- matrix(0, iter + 1, 1)
    lgLik <- numeric(iter + 1)
    conv <- TRUE

    # Start the iteration
    it <- 1
    message("Set the initial values to be:")
    message(initial_values)
    message("Start the optimization...")

    for (it in 1:iter) {
      message(paste("Iteration: ", it))
      # save parameter values in matrix
      Y_mat[it, ] <- c(betas, sigma2_e)
      S_mat[it, ] <- c(gammas)
      B_mat[it, ] <- sigma2_b
      Rho_mat[it] <- rho

      bN <- rep(1, N) %*% t(b)
      bn <- rep(1, n) %*% t(b)

      # linear predictors
      gamma_eval <- function(gammas) {
        rho_b <- rep(rho, N) %*% t(b)
        # longitudinal process
        eta_yx <- as.vector(X %*% betas)
        mu_y <- eta_yx + rho_b
        log_p_yb_N <- dnorm(y, mu_y, sqrt(sigma2_e), log = TRUE)
        log_p_yb <- rowsum(log_p_yb_N, id, na.rm = TRUE)

        # visiting process
        eta_sw <- as.vector(W %*% gammas)
        mu_s <- eta_sw + bN
        log_p_sb_N <- mu_s * d - exp(mu_s) * s
        log_p_sb <- rowsum(log_p_sb_N, id)

        # random effect
        log_p_b_i <- matrix(dnorm(b, rep(0, ncz), sd = sqrt(sigma2_b), log = TRUE), nrow = 1)
        log_p_b <- apply(log_p_b_i, 2, function(x) {
          rep(x, n)
        })

        # joint probability
        p_ysb <- exp(log_p_yb + log_p_sb + log_p_b)
        p_ys <- c(p_ysb %*% wGH)
        p_ys[which(p_ys == 0)] <- 1e-10
        p_bys <- p_ysb / p_ys

        # partial derivative
        partial_gamma <- d - exp(as.vector(W %*% gammas) + bN) * s
        partial_gamma <- rowsum(partial_gamma, id)

        # take the product of each columns of p_bs and partial_gamma
        W_unique <- unique(cbind(id, W))[, -1]
        (f <- crossprod(W_unique, (partial_gamma * p_bys) %*% wGH))
        return(f)
      }
      # gammas_solve <- nleqslv::nleqslv(x = initial_values$gammas, gamma_eval, ..., control = list(trace = 0)) # Note: this was a bug
      gammas_solve = nleqslv::nleqslv(x=initial_values$gammas,gamma_eval,control = list(trace=0))
      (gammas_new <- gammas_solve$x)

      # longitudinal model
      beta_eval <- function(betasrho) {
        betas <- betasrho[1:ncx]
        rho <- betasrho[ncx + 1]
        rho_b <- rep(rho, N) %*% t(b)

        # longitudinal process
        eta_yx <- as.vector(X %*% betas)
        mu_y <- eta_yx + rho_b
        log_p_yb_N <- dnorm(y, mu_y, sqrt(sigma2_e), log = TRUE)
        log_p_yb <- rowsum(log_p_yb_N, id, na.rm = TRUE)

        # visiting process
        eta_sw <- as.vector(W %*% gammas_new)
        mu_s <- eta_sw + bN
        log_p_sb_N <- mu_s * d - exp(mu_s) * s
        log_p_sb <- rowsum(log_p_sb_N, id)

        # random effect
        log_p_b_i <- matrix(dnorm(b, rep(0, ncz), sd = sqrt(sigma2_b), log = TRUE), nrow = 1)
        log_p_b <- apply(log_p_b_i, 2, function(x) {
          rep(x, n)
        })

        # joint probability
        p_ysb <- exp(log_p_yb + log_p_sb + log_p_b)
        p_ys <- c(p_ysb %*% wGH)
        p_ys[which(p_ys == 0)] <- 1e-10
        p_bys <- p_ysb / p_ys

        p_bys_N <- do.call(rbind, lapply(
          1:nrow(p_bys),
          function(i) matrix(rep(p_bys[i, ], each = ni[i]), nrow = ni[i], byrow = FALSE)
        ))

        # partial derivatives
        res <- y - mu_y
        temp <- as.vector((-res * p_bys_N / sigma2_e) %*% wGH)
        score_betas <- colSums(X * temp, na.rm = TRUE)

        temp1 <- as.vector((-res * bN * p_bys_N / sigma2_e) %*% wGH)
        score_rho <- sum(temp1, na.rm = TRUE)

        (f <- c(score_betas, score_rho))
        return(f)
      }

      betasrho_solve_fun <- function(betas, rho) {
        initial_guesses <- list(c(betas, rho), c(betas, 0), c(betas, 0.05), c(betas, -0.05), c(betas, 1.5))

        results <- lapply(initial_guesses, function(guess) {
          tryCatch(
            {
              nleqslv::nleqslv(guess, beta_eval, method = "Newton") # Note: this was a bug
            },
            error = function(e) {
              NULL
            }
          )
        })

        # Filter successful results
        successful_results <- Filter(function(res) !is.null(res) && res$termcd == 1, results)

        # Choose the best result (e.g., with the lowest residual)
        if (length(successful_results) > 0) {
          best_result <- successful_results[[which.min(sapply(successful_results, function(res) sum(res$fvec^2)))]]
        } else {
          best_result <- NULL
          message("No successful solution found.")
        }

        return(best_result)
      }

      betasrho <- c(betas, rho)
      betasrho_solve <- betasrho_solve_fun(betas, rho)
      if (is.null(betasrho_solve)) {
        betas <- NA
        stop("\n\nnot converged!\n")
      } else {
        betasrho_new <- betasrho_solve$x
      }
      # (betasrho_new = betasrho_solve$x)
      betas_new <- betasrho_new[1:ncx]
      rho_new <- betasrho_new[ncx + 1]

      # update the variance components
      # longitudinal process
      eta_yx <- as.vector(X %*% betas_new)
      mu_y <- eta_yx + rep(rho_new, N) %*% t(b)
      log_p_yb_N <- dnorm(y, mu_y, sqrt(sigma2_e), log = TRUE)
      log_p_yb <- rowsum(log_p_yb_N, id, na.rm = TRUE)

      # visiting process
      eta_sw <- as.vector(W %*% gammas_new)
      mu_s <- eta_sw + bN
      log_p_sb_N <- mu_s * d - exp(mu_s) * s
      log_p_sb <- rowsum(log_p_sb_N, id)

      # random effect
      log_p_b_i <- matrix(dnorm(b, rep(0, ncz), sd = sqrt(sigma2_b), log = TRUE), nrow = 1)
      log_p_b <- apply(log_p_b_i, 2, function(x) {
        rep(x, n)
      })

      # joint probability
      p_ysb <- exp(log_p_yb + log_p_sb + log_p_b)
      p_ys <- c(p_ysb %*% wGH)
      # which(p_ys==0)
      p_ys[which(p_ys == 0)] <- 1e-10
      p_bys <- p_ysb / p_ys

      post_b <- p_bys %*% (b * wGH)
      post_b2 <- p_bys %*% (b^2 * wGH)
      post_vb <- post_b2 - post_b^2
      post_b_N <- rep(post_b, ni)
      post_b2_N <- rep(post_b2, ni)

      res1 <- y - eta_yx
      (sigma2_e_new <- sum_rmna(sum_rmna(res1^2) - 2 * rho_new * sum_rmna(res1 * post_b_N) + rho^2 * sum_rmna(post_b2_N)) / N)
      if (sigma2_e_new <= 0) {
        # annother approach: directly integrate b
        res1 <- y - eta_yx
        nna_index <- which(!is.na(res1))
        res_sq <- ((res1 - bN) * (res1 - bN))[nna_index, ]
        id_nna <- id[nna_index]
        id_select <- which(rownames(p_ysb) %in% unique(id_nna))
        p_ysb_nna <- p_ysb[rownames(p_ysb) %in% unique(id_nna), ]
        p_ys_nna <- p_ys[id_select]
        (sigma2_e_new <- sum((rowsum(res_sq, id_nna) * p_ysb_nna / p_ys_nna) %*% wGH) / dim(res_sq)[1])
      }

      (sigma2_b_new <- sum_rmna(post_b^2 + post_vb) / n)

      # update parameter values
      betas <- betasrho_new[1:ncx]
      rho <- betasrho_new[ncx + 1]
      gammas <- gammas_new
      sigma2_e <- sigma2_e_new
      sigma2_b <- sigma2_b_new

      # compute log-likelihood
      log_p_ys <- log(p_ys)
      lgLik[it] <- sum(log_p_ys[is.finite(log_p_ys)], na.rm = TRUE)

      # print results if verbose
      if (control$verbose) {
        cat("\n\niter:", it, "\n")
        cat("log-likelihood:", lgLik[it], "\n")
        cat("betas:", round(betas, 4), "\n")
        cat("sigma2_e:", round(sigma2_e, 4), "\n")
        cat("gammas:", round(gammas, 4), "\n")
        cat("sigma2_b:", round(sigma2_b, 4), "\n")
        cat("rho:", round(rho, 4), "\n")
      }

      # check convergence
      if (it > 1) {
        if (lgLik[it] > lgLik[it - 1]) {
          thetas1 <- c(Y_mat[it - 1, ], S_mat[it - 1, ], B_mat[it - 1, ], Rho_mat[it - 1])
          thetas2 <- c(Y_mat[it, ], S_mat[it, ], B_mat[it, ], Rho_mat[it])
          check1 <- max(abs(thetas2 - thetas1) / (abs(thetas1) + control$tol)) < control$tol
          check2 <- (lgLik[it] - lgLik[it - 1]) < control$tol * (abs(lgLik[it - 1]) + control$tol)
          if (check1 || check2) {
            # if (check1) {
            conv <- FALSE # why set this to FALSE?
            if (control$verbose) {
              conv <- FALSE
            }
            message("\n\nconverged!\n")
            break
          }
        }
      }
    }

    # if there's no converge message, print "do not converge" and exit the function
    if (it == control$maxiter) {
      betas <- NA
      stop("\n\nnot converged! Increase maxiter. \n")
    }

    names(betas) <- colnames(X)
    names(betas)[1] <- "(intercept)"
    names(betas)[which(names(betas) == "time")] <- time_var

    check_points <- list(
      Y_mat = Y_mat,
      S_mat = S_mat,
      B_mat = B_mat,
      Rho_mat = Rho_mat
    )
    estimates <- list(
      gamma_hat = gammas,
      sigma2_e = sigma2_e,
      sigma2_b = sigma2_b,
      rho = rho
    )
    results <- list(beta_hat = betas)
    return(results)
  }
}
