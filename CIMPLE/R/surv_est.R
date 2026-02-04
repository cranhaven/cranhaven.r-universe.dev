#' Coefficient estimation in the survival model with longitudinal measurements.
#'
#' This function offers a collection of methods of coefficient estimation in a
#' survival model with a longitudinally measured predictor. These methods
#' include Cox proportional hazard model with time-varying covariates (`cox`),
#' Joint modeling the longitudinal and disease diagnosis processes (`JMLD`), Joint
#' modeling the longitudinal and disease diagnosis processes with an adjustment
#' for the historical number of visits in the longitudinal model (`VA_JMLD`), Cox
#' proportional hazard model with time-varying covariates after imputation
#' (`Imputation_Cox`), Cox proportional hazard model with time-varying covariates
#' after imputation with an adjustment for the historical number of visits in
#' the longitudinal model (`VAImputation_Cox`).
#'
#' @param long_data Longitudinal dataset.
#' @param surv_data Survival dataset.
#' @param method The following methods are available:
#' - `cox`: Cox proportional hazard model with time-varying covariates.
#' - `JMLD`: Joint modeling the longitudinal and disease diagnosis processes.
#' - `VA_JMLD`: Joint modeling the longitudinal and disease diagnosis processes with an adjustment for the historical number of visits in the longitudinal model.
#' - `Imputation_Cox`: Cox proportional hazard model with time-varying covariates after imputation.
#' - `VAImputation_Cox`: Cox proportional hazard model with time-varying covariates after imputation with an adjustment for the historical number of visits in the longitudinal model.
#' @param id_var Variable for the subject ID to indicate the grouping
#'   structure.
#' @param time Variable for the observational time.
#' @param survTime Variable for the survival time.
#' @param survEvent Variable for the survival event.
#' @param LM_fixedEffect_variables Vector input of variable names with fixed
#'   effects in the longitudinal model. Variables should not contain time.
#' @param LM_randomEffect_variables Vector input of variable names with random
#'   effects in the longitudinal model.
#' @param SM_timeVarying_variables Vector input of variable names for
#'   time-varying variables in the survival model.
#' @param SM_timeInvariant_variables Vector input of variable names for
#'   time-invariant variables in the survival model.
#' @param imp_time_factor Scale factor for the time variable. This argument is
#'   only needed in the imputation-based methods, e.g., `Imputation_Cox` and
#'   `VAImputation_Cox`. The default is `NULL` (no scale).
#'
#' @return `alpha_hat`: Estimated coefficients for the survival model.
#'
#'   Other output in each method:
#' * `JMLD`:
#'     * `beta_hat`: Estimated coefficients for the longitudinal model.
#' * `VA_JMLD`:
#'     * `beta_hat`: Estimated coefficients for the longitudinal model.
#' @export
#'
#' @references
#'
#' Rizopoulos, D. (2010). Jm: An r package for the joint modelling of longitudinal and time-to-event data. Journal of
#' statistical software, 35:1–33.
#'
#' Rizopoulos, D. (2012b). Joint models for longitudinal and time-to-event data: With applications in R. CRC press.
#'
#' @examples
#' # Setup arguments
#'
#' id_var = "id"
#' time = "time"
#' survTime = "D"
#' survEvent = "d"
#' LM_fixedEffect_variables = c("Age","Sex","SNP")
#' LM_randomEffect_variables = c("SNP")
#' SM_timeVarying_variables = c("Y")
#' SM_timeInvariant_variables = c("Age","Sex","SNP")
#' imp_time_factor = 1
#'
#' # Run the cox model
#' fit_cox = surv_est(surv_data = surv_data,
#'                    long_data = long_data,
#'                    method = "cox",
#'                    id_var = id_var,
#'                    time = time,
#'                    survTime = survTime,
#'                    survEvent = survEvent,
#'                    LM_fixedEffect_variables = LM_fixedEffect_variables,
#'                    LM_randomEffect_variables = LM_randomEffect_variables,
#'                    SM_timeVarying_variables = SM_timeVarying_variables,
#'                    SM_timeInvariant_variables = SM_timeInvariant_variables,
#'                    imp_time_factor = imp_time_factor)
#' # Return the coefficient estimates
#' fit_cox$alpha_hat
surv_est <- function(long_data,
                     surv_data,
                     method,
                     id_var,
                     time = NULL,
                     survTime = NULL,
                     survEvent = NULL,
                     LM_fixedEffect_variables = NULL,
                     LM_randomEffect_variables = NULL,
                     SM_timeVarying_variables = NULL,
                     SM_timeInvariant_variables = NULL,
                     imp_time_factor = NULL) {

  colnames(long_data)[which(colnames(long_data) == id_var)] <- "id"
  colnames(long_data)[which(colnames(long_data) == SM_timeVarying_variables)] <- "Y"
  colnames(long_data)[which(colnames(long_data) == time)] <- "time"
  colnames(surv_data)[which(colnames(surv_data) == id_var)] <- "id"
  colnames(surv_data)[which(colnames(surv_data) == survTime)] <- "D"
  colnames(surv_data)[which(colnames(surv_data) == survEvent)] <- "d"

  LM_fixedEffect_withTime_variables <- c(LM_fixedEffect_variables, "time")
  SM_variables <- c("Y", SM_timeInvariant_variables)

  if(length(setdiff(unique(surv_data$id), unique(long_data$id))) > 0 | (length(setdiff(unique(long_data$id), unique(surv_data$id)))>0)) {
    warning("ids in the long_data and surv_data are not the same.")
  }

  if (method == "cox") {
    # Check Inputs
    stopifnot("`LM_fixedEffect_variables` must be provided." = !is.null(LM_fixedEffect_variables))
    stopifnot("`time` must be provided." = !is.null(time))
    stopifnot("`LM_randomEffect_variables` must be provided." = !is.null(LM_randomEffect_variables))
    stopifnot("`SM_timeVarying_variables` must be provided." = !is.null(SM_timeVarying_variables))
    stopifnot("`SM_timeInvariant_variables` must be provided." = !is.null(SM_timeInvariant_variables))

    long_data2 <- long_data %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(time0 = ifelse(is.na(dplyr::lag(time)), 0, dplyr::lag(time))) %>%
      dplyr::filter(time != 0) %>%
      dplyr::filter(time0 < time)
    long_data2$d0 <- surv_data$d[match(long_data2$id, surv_data$id)]

    long_data2 <- na.omit(long_data2) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(d = ifelse(time < max(time, na.rm = TRUE), 0, d0))
    model_formula <- as.formula(paste("survival::Surv(time0, time, d) ~ ", paste(SM_variables, collapse = "+")))
    model <- survival::coxph(model_formula, data = long_data2)
    alpha.hat <- summary(model)$coef[, 1]

    names(alpha.hat)[which(names(alpha.hat) == "Y")] <- SM_timeVarying_variables

    return(list(alpha_hat = alpha.hat))

  } else if (method == "JMLD") {
    # Check Inputs
    stopifnot("`LM_fixedEffect_variables` must be provided." = !is.null(LM_fixedEffect_variables))
    stopifnot("`time` must be provided." = !is.null(time))
    stopifnot("`LM_randomEffect_variables` must be provided." = !is.null(LM_randomEffect_variables))
    stopifnot("`SM_timeVarying_variables` must be provided." = !is.null(SM_timeVarying_variables))
    stopifnot("`SM_timeInvariant_variables` must be provided." = !is.null(SM_timeInvariant_variables))

    # remove patients with no Y measurement
    zeroRecords_id <- long_data[is.na(long_data$Y), "id"]
    long_data <- long_data[!long_data$id %in% zeroRecords_id, ]
    surv_data <- surv_data[!surv_data$id %in% zeroRecords_id, ]

    # longitudinal submodel, try nlminb optimizer first, if there is an error, then use optim optimizer
    lmeFit_fixedformula <- as.formula(paste("Y ~ ", paste(LM_fixedEffect_withTime_variables, collapse = "+")))
    lmeFit_randomformula <- as.formula(paste("~", paste(LM_randomEffect_variables, collapse = "+"), "|id"))
    control_optim <- nlme::lmeControl(msMaxIter = 1000, msMaxEval = 1000, opt = "optim")
    control_nlminb <- nlme::lmeControl(msMaxIter = 1000, msMaxEval = 1000, opt = "nlminb")
    lmeFit <- tryCatch(
      {
        lmeFit <- nlme::lme(lmeFit_fixedformula, random = lmeFit_randomformula, data = long_data, control = control_optim)
      },
      error = function(e) {
        message(paste0("Error with optim: ", e))
        lmeFit <- nlme::lme(lmeFit_fixedformula, random = lmeFit_randomformula, data = long_data, control = control_nlminb)
      }
    )
    # message(lmeFit)

    # survival submodel
    coxFit_formula <- as.formula(paste("survival::Surv(D, d) ~ ", paste(SM_timeInvariant_variables, collapse = "+")))
    coxFit <- survival::coxph(coxFit_formula, data = surv_data, x = TRUE)
    # summary(coxFit)

    jointFit = JMbayes2::jm(coxFit,lmeFit,time_var="time", n_chains=1L,n_iter=2000L,n_burnin=500L)
    # jointFit <- JMbayes2::jm(coxFit, lmeFit, time_var = "time", n_chains = 1L)

    # message(summary(jointFit))

    surv_proc <- unlist(coef(jointFit)) # change the name of the output to alpha
    long_proc <- unlist(nlme::fixef(jointFit))

    names(long_proc) <- gsub("Y.", "", names(long_proc))
    names(long_proc)[which(names(long_proc) == "time")] <- time
    names(surv_proc) <- gsub("gammas.", "", names(surv_proc))
    names(surv_proc)[length(names(surv_proc))] <- SM_timeVarying_variables

    results <- list(
      beta_hat = long_proc, # beta_hat
      alpha_hat = surv_proc # alpha_hat
    )

    return(results)
  } else if (method == "VA_JMLD") {
    # Check Inputs
    stopifnot("`LM_fixedEffect_variables` must be provided." = !is.null(LM_fixedEffect_variables))
    stopifnot("`time` must be provided." = !is.null(time))
    stopifnot("`LM_randomEffect_variables` must be provided." = !is.null(LM_randomEffect_variables))
    stopifnot("`SM_timeVarying_variables` must be provided." = !is.null(SM_timeVarying_variables))
    stopifnot("`SM_timeInvariant_variables` must be provided." = !is.null(SM_timeInvariant_variables))


    # remove patients with no Y measurement
    zeroRecords_id <- long_data[is.na(long_data$Y), "id"]
    long_data <- long_data[!long_data$id %in% zeroRecords_id, ]
    surv_data <- surv_data[!surv_data$id %in% zeroRecords_id, ]

    # add Ni(t) as a predictor
    long_data$Ni <- NA
    for (i in 1:nrow(long_data)) {
      long_data$Ni[i] <- sum(!is.na(long_data$Y[long_data$id == long_data$id[i] & long_data$time <= long_data$time[i]]))
    }

    # longitudinal submodel, try nlminb optimizer first, if there is an error, then use optim optimizer
    lmeFit_fixedformula <- as.formula(paste("Y ~ ", paste(LM_fixedEffect_withTime_variables, collapse = "+"), "+Ni"))
    lmeFit_randomformula <- as.formula(paste("~", paste(LM_randomEffect_variables, collapse = "+"), "|id"))
    control_optim <- nlme::lmeControl(msMaxIter = 1000, msMaxEval = 1000, opt = "optim")
    control_nlminb <- nlme::lmeControl(msMaxIter = 1000, msMaxEval = 1000, opt = "nlminb")
    lmeFit <- tryCatch(
      {
        lmeFit <- nlme::lme(lmeFit_fixedformula, random = lmeFit_randomformula, data = long_data, control = control_optim)
      },
      error = function(e) {
        message(paste0("Error with optim: ", e))
        lmeFit <- nlme::lme(lmeFit_fixedformula, random = lmeFit_randomformula, data = long_data, control = control_nlminb)
      }
    )

    # survival submodel
    coxFit_formula <- as.formula(paste("survival::Surv(D, d) ~ ", paste(SM_timeInvariant_variables, collapse = "+")))
    coxFit <- survival::coxph(coxFit_formula, data = surv_data, x = TRUE)
    summary(coxFit)

    jointFit = JMbayes2::jm(coxFit,lmeFit,time_var="time", n_chains=1L,n_iter=2000L,n_burnin=500L)
    # jointFit <- JMbayes2::jm(coxFit, lmeFit, time_var = "time", n_chains = 1L)

    surv_proc <- unlist(coef(jointFit))
    long_proc <- unlist(nlme::fixef(jointFit))
    names(long_proc) <- gsub("Y.", "", names(long_proc))
    names(long_proc)[which(names(long_proc) == "time")] <- time
    names(surv_proc) <- gsub("gammas.", "", names(surv_proc))
    names(surv_proc)[length(names(surv_proc))] <- SM_timeVarying_variables

    results <- list(
      beta_hat = long_proc, # beta_hat
      alpha_hat = surv_proc # alpha_hat
    )

    return(results)
  } else if (method == "Imputation_Cox") {
    # Check Inputs
    stopifnot("`LM_fixedEffect_variables` must be provided." = !is.null(LM_fixedEffect_variables))
    stopifnot("`time` must be provided." = !is.null(time))
    stopifnot("`LM_randomEffect_variables` must be provided." = !is.null(LM_randomEffect_variables))
    stopifnot("`SM_timeVarying_variables` must be provided." = !is.null(SM_timeVarying_variables))
    stopifnot("`SM_timeInvariant_variables` must be provided." = !is.null(SM_timeInvariant_variables))


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
        dplyr::select(id, Y_mean, time_new, dplyr::setdiff(names(data), c("id", "Y", "time"))) %>%
        dplyr::ungroup()
      colnames(data)[2:3] <- c("Y", "time")

      # create a new dataset with the same columns in data1. For a given id, the time is from 1 to max_time. If there is no value of Y at a given time, then Y is NA.
      max_time <- max(data$time, na.rm = TRUE)
      id_all <- rep(unique(data$id), each = max_time)
      time_all <- rep(as.numeric(1:max_time), length(unique(data$id)))
      data_base <- data %>%
        dplyr::select(-Y, -time) %>%
        unique()
      data2 <- data_base %>%
        dplyr::slice(rep(1:dplyr::n(), each = max_time)) %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(time = rep(1:max_time))
      # delete time after the disease time
      data2$D <- surv_data$D[match(data2$id, surv_data$id)]
      data2 <- data2[data2$time <= ceiling(data2$D / imp_time_factor), dplyr::setdiff(names(data2), c("Y"))]

      # join the two datasets
      data3 <- dplyr::left_join(data2, data, by = dplyr::setdiff(names(data), c("Y")),multiple = "all")

      data3$time <- data3$time * imp_time_factor # convert time back to the original scale
    }

    message(paste0("imp factor: ", imp_time_factor))

    df_full <- data3
    dim(df_full)

    #### Start imputation ####
    # empty imputation
    imp0 <- mice::mice(as.matrix(df_full), maxit = 0)
    predM <- imp0$predictorMatrix
    impM <- imp0$method

    # specify predictor matrix and method
    predM1 <- predM
    predM1["Y", "id"] <- -2
    predM1["Y", LM_fixedEffect_withTime_variables] <- 1 # fixed x effects imputation
    impM1 <- impM
    impM1["Y"] <- "2l.lmer"

    # multilevel imputation
    imp1 <- mice::mice(as.matrix(df_full),
                       m = 5,
                       predictorMatrix = predM1, method = impM1, maxit = 5
    )

    # fit the cox-ph model
    coxph_imp <- function(data_imp) {
      # cox-ph model
      long_data_imp2 <- data_imp %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(time0 = ifelse(is.na(dplyr::lag(time)), 0, dplyr::lag(time)))
      long_data_imp2$d0 <- surv_data$d[match(long_data_imp2$id, surv_data$id)]
      long_data_imp2 <- na.omit(long_data_imp2) %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(d = ifelse(time < max(time, na.rm = TRUE), 0, d0)) %>%
        dplyr::filter(time0 < time)
      model_formula <- as.formula(paste("survival::Surv(time0, time, d) ~ ", paste(SM_variables, collapse = "+")))
      model <- survival::coxph(model_formula, data = long_data_imp2)
      (alpha.hat <- summary(model)$coef[, 1])
    }
    fit <- lapply(1:5, function(i) coxph_imp(mice::complete(imp1, action = i)))
    alpha.hat <- sapply(seq_along(fit[[1]]), function(i) mean(sapply(fit, `[`, i)))
    names(alpha.hat) <- names(fit[[1]])
    names(alpha.hat)[which(names(alpha.hat) == "Y")] <- SM_timeVarying_variables

    return(list(alpha_hat = alpha.hat))

  } else if (method == "VAImputation_Cox") {
    # Check Inputs
    stopifnot("`LM_fixedEffect_variables` must be provided." = !is.null(LM_fixedEffect_variables))
    stopifnot("`time` must be provided." = !is.null(time))
    stopifnot("`LM_randomEffect_variables` must be provided." = !is.null(LM_randomEffect_variables))
    stopifnot("`SM_timeVarying_variables` must be provided." = !is.null(SM_timeVarying_variables))
    stopifnot("`SM_timeInvariant_variables` must be provided." = !is.null(SM_timeInvariant_variables))


    data <- long_data

    if (is.null(imp_time_factor)) {
      imp_time_factor <- 1
      data3 <- data
    } else {
      # If the imp_time_factor is specified and is not 1
      # group the data based on the time factor
      data <- data %>%
        dplyr::mutate(time_new = ceiling(time / imp_time_factor)) %>%
        dplyr::group_by(id, time_new) %>%
        dplyr::mutate(Y_mean = mean(Y, na.rm = TRUE)) %>%
        # mutate(Y_mean = ifelse(is.numeric(Y_mean),Y_mean,NA)) %>%
        dplyr::select(id, Y_mean, time_new, dplyr::setdiff(names(data), c("id", "Y", "time"))) %>%
        dplyr::ungroup()
      colnames(data)[2:3] <- c("Y", "time")

      # create a new dataset with the same columns in data1. For a given id, the time is from 1 to max_time. If there is no value of Y at a given time, then Y is NA.
      max_time <- max(data$time, na.rm = TRUE)
      id_all <- rep(unique(data$id), each = max_time)
      time_all <- rep(as.numeric(1:max_time), length(unique(data$id)))
      data_base <- data %>%
        dplyr::select(-Y, -time) %>%
        unique()
      data2 <- data_base %>%
        dplyr::slice(rep(1:dplyr::n(), each = max_time)) %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(time = rep(1:max_time))
      # delete time after the disease time
      data2$D <- surv_data$D[match(data2$id, surv_data$id)]
      data2 <- data2[data2$time <= ceiling(data2$D / imp_time_factor), dplyr::setdiff(names(data2), c("Y"))]

      # join the two datasets
      data3 <- dplyr::left_join(data2, data, by = dplyr::setdiff(names(data), c("Y")),multiple = "all")
      data3$time <- data3$time * imp_time_factor # convert time back to the original scale
    }

    # insert the predictor Ni(t)
    df_full <- data3
    df_full$Ni <- NA
    for (i in 1:nrow(df_full)) {
      df_full$Ni[i] <- sum(!is.na(df_full$Y[df_full$id == df_full$id[i] & df_full$time <= df_full$time[i]]))
    }
    df_full <- df_full %>%
      as.data.frame() %>%
      dplyr::select(all_of(c(colnames(data), "Ni")))
    head(df_full)

    #### Start imputation ####
    # empty imputation
    imp0 <- mice::mice(as.matrix(df_full), maxit = 0)
    predM <- imp0$predictorMatrix
    impM <- imp0$method

    # specify predictor matrix and method
    predM1 <- predM
    predM1["Y", "id"] <- -2
    predM1["Y", c(LM_fixedEffect_withTime_variables, "Ni")] <- 1 # fixed x effects imputation
    impM1 <- impM
    impM1["Y"] <- "2l.lmer"

    # multilevel imputation
    imp1 <- mice::mice(as.matrix(df_full),
                       m = 5,
                       predictorMatrix = predM1, method = impM1, maxit = 5
    )

    # fit the cox-ph model
    coxph_imp <- function(data_imp) {
      # cox-ph model
      long_data_imp2 <- data_imp %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(time0 = ifelse(is.na(dplyr::lag(time)), 0, dplyr::lag(time)))
      long_data_imp2$d0 <- surv_data$d[match(long_data_imp2$id, surv_data$id)]
      long_data_imp2 <- na.omit(long_data_imp2) %>%
        dplyr::group_by(id) %>%
        dplyr::filter(time0 < time) %>%
        dplyr::mutate(d = ifelse(time < max(time, na.rm = TRUE), 0, d0))
      model_formula <- as.formula(paste("survival::Surv(time0, time, d) ~ ", paste(SM_variables, collapse = "+")))
      model <- survival::coxph(model_formula, data = long_data_imp2)
      (alpha.hat <- summary(model)$coef[, 1])
    }

    fit <- lapply(1:5, function(i) coxph_imp(mice::complete(imp1, action = i)))
    alpha.hat <- sapply(seq_along(fit[[1]]), function(i) mean(sapply(fit, `[`, i)))
    names(alpha.hat) <- names(fit[[1]])
    names(alpha.hat)[which(names(alpha.hat) == "Y")] <- SM_timeVarying_variables

    return(list(alpha_hat = alpha.hat))
  }
}


