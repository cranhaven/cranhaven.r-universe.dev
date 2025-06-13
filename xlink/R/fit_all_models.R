#' Model fitting results for each SNP considering as XCI, XCI-E and XCI-S type
#'
#' \code{fit_all_models} returns model fitting results for each SNP understanding as XCI, XCI-E and XCI-S type respectively. Model comparison results is provided by using AIC as a criterion.
#'
#' @param resp Response variable for continuous or binary model fitting.
#' @param os Survival indicator, 1 for death, 0 for censoring.
#' @param ostime Duration time of survival.
#' @param snp Single SNP name.
#' @param gender Gender variable.
#' @param male Male indicator in gender variable.
#' @param female Female indicator in gender variable.
#' @param covars Covariates list.
#' @param model Fitting model type. For 'linear', fitting linear model. For 'binary', fitting logistic regression model. For 'survival', fitting survival model.
#' @param data Data set.
#' @return It returns estimated parameters, confidence interval and P value for each variable. Baseline model and full model maximum likelihood estimation are provided.
#' @seealso \code{\link{lm}{stats}} for linear model, \code{\link{glm}{stats}} for logistic regression model, and \code{\link{coxph}{survival}} for survival model.
#' @import  survival
#' @import  stats
fit_all_models <- function(resp, os, ostime, snp, gender, male, female, covars, model, data) {

    MAF_value <- MAF(snp = snp, gender = gender, male = male, MAF_v = 0, data = data)[2]

    if (model == "survival") {
        var_list <- c(os, ostime, snp, gender, covars)
        var_n <- length(var_list)
        data <- data[, var_list]

        snp_var <- data[, snp]
        n_na <- (!is.na(snp_var))
        data <- data[n_na, ]

        ind <- !((data[, 3] == 1) & (data[, 4] == female))

        male_snp <- data[data[, 4] == male, 3]
        snp_type <- sum(levels(as.factor(male_snp)) %in% c("2"))

        if (snp_type == 1) {
            new_col <- data[, 3]
            new_col[((data[, 4] == male) & (data[, 3] == 2))] <- 1
            snp_xci <- data[, 3]
        } else {
            new_col <- data[, 3]
            new_col[((data[, 4] == male) & (data[, 3] == 1))] <- 2
            snp_xci <- new_col
        }

        model_list <- c("XCI", "XCI-E")
        model_list <- c(model_list[2 - snp_type], model_list[1 + snp_type])

        data <- cbind(data, new_col)
        snp_new <- paste(snp, c("XCI_E", "XCI")[2 - snp_type], sep = "_")
        colnames(data)[var_n + 1] <- snp_new

        formula_bl <- paste("Surv", "(", ostime, ",", os, ")", "~", gender)
        formula_g <- paste("Surv", "(", ostime, ",", os, ")", "~", snp, "+", gender)
        formula_a <- paste("Surv", "(", ostime, ",", os, ")", "~", snp_new, "+", gender)
        formula_sk <- paste("Surv", "(", ostime, ",", os, ")", "~", "snp_sk", "+", gender)

        if (length(covars) != 0) {
            covar_formula <- paste(covars, collapse = "+")
            formula_bl <- paste(formula_bl, covar_formula, sep = "+")
            formula_g <- paste(formula_g, covar_formula, sep = "+")
            formula_a <- paste(formula_a, covar_formula, sep = "+")
            formula_sk <- paste(formula_sk, covar_formula, sep = "+")
        }

        Model_bl <- survival::coxph(stats::as.formula(formula_bl), data <- data)
        Model_g <- survival::coxph(stats::as.formula(formula_g), data <- data)
        Model_a <- survival::coxph(stats::as.formula(formula_a), data <- data)

        coef_g <- Model_g$coefficients
        coef_a <- Model_a$coefficients

        LR_g <- Model_g$loglik[2] - Model_bl$loglik[2]
        LR_a <- Model_a$loglik[2] - Model_bl$loglik[2]


        snp_skew <- function(x) {
            value <- ind * snp_xci + (1 - ind) * x
            return(value)
        }

        formula_s <- paste(formula_bl, "+snp_skew(x)", step = "")

        goal_fun <- function(x) {
            max_loglik <- survival::coxph(stats::as.formula(formula_s), data = cbind(data, snp_skew(x)))$loglik[2]
            return(-max_loglik)
        }

        result <- stats::optim(par = 1, fn = goal_fun, lower = 0, upper = 2, method = "L-BFGS-B")
        gamma <- result$par
        LR_s <- -result$value - Model_bl$loglik[2]
        snp_sk <- ind * snp_xci + (1 - ind) * gamma

        Model_s <- survival::coxph(stats::as.formula(formula_sk), data = cbind(data, snp_sk))
        Coef_AIC <- Model_s$coefficients

        if (is.na(Coef_AIC[1]) == 1) {
            gamma <- NA
        }

        model_ch <- c("XCI-S")
        model_ind <- ((LR_a - 2) > (LR_s - 3) && (LR_a > LR_g)) + 2 * ((LR_g - 2) > (LR_s - 3) && (LR_g > LR_a)) + 3 * ((LR_a == LR_g) && (LR_g - 2) > (LR_s -
            3))

        if (model_ind == 1) {
            model_ch <- model_list[2]
        }

        if (model_ind == 2) {
            model_ch <- model_list[1]
        }

        if (model_ind == 3) {
            model_ch <- c("XCI")
        }

        infor_g <- infor_table(x = summary(Model_g)$coefficients, snp = snp, covar_n = rownames(summary(Model_g)$coefficients), MAF_value = MAF_value, model)
        loglik_infor <- t(c(Model_bl$loglik[2], Model_g$loglik[2], LR_g))
        colnames(loglik_infor) <- c("Baseline", "Full model", "Loglik ratio")
        mylist_g <- list(coefficients = infor_g, loglik = loglik_infor)

        infor_a <- infor_table(x = summary(Model_a)$coefficients, snp = snp, covar_n = rownames(summary(Model_a)$coefficients), MAF_value = MAF_value, model)
        loglik_infor <- t(c(Model_bl$loglik[2], Model_a$loglik[2], LR_a))
        colnames(loglik_infor) <- c("Baseline", "Full model", "Loglik ratio")
        mylist_a <- list(coefficients = infor_a, loglik = loglik_infor)

        infor_s <- infor_table(x = summary(Model_s)$coefficients, snp = snp, covar_n = rownames(summary(Model_s)$coefficients), MAF_value = MAF_value, model)
        loglik_infor <- t(c(Model_bl$loglik[2], Model_s$loglik[2], LR_s))
        colnames(loglik_infor) <- c("Baseline", "Full model", "Loglik ratio")
        mylist_s <- list(coefficients = infor_s, loglik = loglik_infor, Gamma = gamma)

        mylist <- list(mylist_g, mylist_a, mylist_s, model_ch)
        names(mylist) <- c(model_list, "XCI-S", "Best model by AIC")

        return(mylist)
    }

    if (model == "binary") {
        var_list <- c(resp, snp, gender, covars)
        var_n <- length(var_list)
        data <- data[, var_list]

        snp_var <- data[, snp]
        n_na <- (!is.na(snp_var))
        data <- data[n_na, ]
        ind <- !((data[, 2] == 1) & (data[, 3] == female))

        male_snp <- data[data[, 3] == male, 2]
        snp_type <- sum(levels(as.factor(male_snp)) %in% c("2"))

        if (snp_type == 1) {
            new_col <- data[, 2]
            new_col[((data[, 3] == male) & (data[, 2] == 2))] <- 1
            snp_xci <- data[, 2]
        } else {
            new_col <- data[, 2]
            new_col[((data[, 3] == male) & (data[, 2] == 1))] <- 2
            snp_xci <- new_col
        }

        model_list <- c("XCI", "XCI-E")
        model_list <- c(model_list[2 - snp_type], model_list[1 + snp_type])

        data <- cbind(data, new_col)
        snp_new <- paste(snp, c("XCI_E", "XCI")[2 - snp_type], sep = "_")
        colnames(data)[var_n + 1] <- snp_new

        formula_bl <- paste(resp, "~", gender)
        formula_g <- paste(resp, "~", snp, "+", gender)
        formula_a <- paste(resp, "~", snp_new, "+", gender)
        formula_sk <- paste(resp, "~", "snp_sk", "+", gender)

        if (length(covars) != 0) {
            covar_formula <- paste(covars, collapse = "+")
            formula_bl <- paste(formula_bl, covar_formula, sep = "+")
            formula_g <- paste(formula_g, covar_formula, sep = "+")
            formula_a <- paste(formula_a, covar_formula, sep = "+")
            formula_sk <- paste(formula_sk, covar_formula, sep = "+")
        }

        Model_bl <- stats::glm(stats::as.formula(formula_bl), data <- data, family = binomial(link = "logit"))
        Model_g <- stats::glm(stats::as.formula(formula_g), data <- data, family = binomial(link = "logit"))
        Model_a <- stats::glm(stats::as.formula(formula_a), data <- data, family = binomial(link = "logit"))

        coef_g <- Model_g$coefficients
        coef_a <- Model_a$coefficients

        LR_g <- stats::logLik(Model_g) - stats::logLik(Model_bl)
        LR_a <- stats::logLik(Model_a) - stats::logLik(Model_bl)


        snp_skew <- function(x) {
            value <- ind * snp_xci + (1 - ind) * x
            return(value)
        }

        formula_s <- paste(formula_bl, "+snp_skew(x)", step = "")

        goal_fun <- function(x) {
            max_loglik <- stats::logLik(stats::glm(stats::as.formula(formula_s), data = cbind(data, snp_skew(x)), family = binomial(link = "logit")))
            return(-max_loglik)
        }

        result <- stats::optim(par = 1, fn = goal_fun, lower = 0, upper = 2, method = "L-BFGS-B")
        gamma <- result$par
        LR_s <- -result$value - stats::logLik(Model_bl)
        snp_sk <- ind * snp_xci + (1 - ind) * gamma

        Model_s <- stats::glm(stats::as.formula(formula_sk), data = cbind(data, snp_sk), family = binomial(link = "logit"))
        Coef_AIC <- Model_s$coefficients

        if (is.na(Coef_AIC[1]) == 1) {
            gamma <- NA
        }

        model_ch <- c("XCI-S")
        model_ind <- ((LR_a - 2) > (LR_s - 3) && (LR_a > LR_g)) + 2 * ((LR_g - 2) > (LR_s - 3) && (LR_g > LR_a)) + 3 * ((LR_a == LR_g) && (LR_g - 2) > (LR_s -
            3))

        if (model_ind == 1) {
            model_ch <- model_list[2]
        }

        if (model_ind == 2) {
            model_ch <- model_list[1]
        }

        if (model_ind == 3) {
            model_ch <- c("XCI")
        }

        infor_g <- infor_table(x = summary(Model_g)$coefficients, snp = snp, covar_n = rownames(summary(Model_g)$coefficients), MAF_value = MAF_value, model)
        loglik_infor <- t(c(stats::logLik(Model_bl), stats::logLik(Model_g), LR_g))
        colnames(loglik_infor) <- c("Baseline", "Full model", "Loglik ratio")
        mylist_g <- list(coefficients = infor_g, loglik = loglik_infor)

        infor_a <- infor_table(x = summary(Model_a)$coefficients, snp = snp, covar_n = rownames(summary(Model_a)$coefficients), MAF_value = MAF_value, model)
        loglik_infor <- t(c(stats::logLik(Model_bl), stats::logLik(Model_a), LR_a))
        colnames(loglik_infor) <- c("Baseline", "Full model", "Loglik ratio")
        mylist_a <- list(coefficients = infor_a, loglik = loglik_infor)

        infor_s <- infor_table(x = summary(Model_s)$coefficients, snp = snp, covar_n = rownames(summary(Model_s)$coefficients), MAF_value = MAF_value, model)
        loglik_infor <- t(c(stats::logLik(Model_bl), stats::logLik(Model_s), LR_s))
        colnames(loglik_infor) <- c("Baseline", "Full model", "Loglik ratio")
        mylist_s <- list(coefficients = infor_s, loglik = loglik_infor, Gamma = gamma)

        mylist <- list(mylist_g, mylist_a, mylist_s, model_ch)
        names(mylist) <- c(model_list, "XCI-S", "Best model by AIC")

        return(mylist)

    }

    if (model == "linear") {
        var_list <- c(resp, snp, gender, covars)
        var_n <- length(var_list)
        data <- data[, var_list]

        snp_var <- data[, snp]
        n_na <- (!is.na(snp_var))
        data <- data[n_na, ]
        ind <- !((data[, 2] == 1) & (data[, 3] == female))

        male_snp <- data[data[, 3] == male, 2]
        snp_type <- sum(levels(as.factor(male_snp)) %in% c("2"))

        if (snp_type == 1) {
            new_col <- data[, 2]
            new_col[((data[, 3] == male) & (data[, 2] == 2))] <- 1
            snp_xci <- data[, 2]
        } else {
            new_col <- data[, 2]
            new_col[((data[, 3] == male) & (data[, 2] == 1))] <- 2
            snp_xci <- new_col
        }

        model_list <- c("XCI", "XCI-E")
        model_list <- c(model_list[2 - snp_type], model_list[1 + snp_type])

        data <- cbind(data, new_col)
        snp_new <- paste(snp, c("XCI_E", "XCI")[2 - snp_type], sep = "_")
        colnames(data)[var_n + 1] <- snp_new

        formula_bl <- paste(resp, "~", gender)
        formula_g <- paste(resp, "~", snp, "+", gender)
        formula_a <- paste(resp, "~", snp_new, "+", gender)
        formula_sk <- paste(resp, "~", "snp_sk", "+", gender)

        if (length(covars) != 0) {
            covar_formula <- paste(covars, collapse = "+")
            formula_bl <- paste(formula_bl, covar_formula, sep = "+")
            formula_g <- paste(formula_g, covar_formula, sep = "+")
            formula_a <- paste(formula_a, covar_formula, sep = "+")
            formula_sk <- paste(formula_sk, covar_formula, sep = "+")
        }

        Model_bl <- stats::lm(stats::as.formula(formula_bl), data <- data)
        Model_g <- stats::lm(stats::as.formula(formula_g), data <- data)
        Model_a <- stats::lm(stats::as.formula(formula_a), data <- data)

        coef_g <- Model_g$coefficient
        coef_a <- Model_a$coefficient

        LR_g <- stats::logLik(Model_g) - stats::logLik(Model_bl)
        LR_a <- stats::logLik(Model_a) - stats::logLik(Model_bl)


        snp_skew <- function(x) {
            value <- ind * snp_xci + (1 - ind) * x
            return(value)
        }

        formula_s <- paste(formula_bl, "+snp_skew(x)", step = "")

        goal_fun <- function(x) {
            max_loglik <- stats::logLik(stats::lm(stats::as.formula(formula_s), data = cbind(data, snp_skew(x))))
            return(-max_loglik)
        }

        result <- stats::optim(par = 1, fn = goal_fun, lower = 0, upper = 2, method = "L-BFGS-B")
        gamma <- result$par
        LR_s <- -result$value - stats::logLik(Model_bl)
        snp_sk <- ind * snp_xci + (1 - ind) * gamma

        Model_s <- stats::lm(stats::as.formula(formula_sk), data = cbind(data, snp_sk))
        Coef_AIC <- Model_s$coefficient

        if (is.na(Coef_AIC[1]) == 1) {
            gamma <- NA
        }

        model_ch <- c("XCI-S")
        model_ind <- ((LR_a - 2) > (LR_s - 3) && (LR_a > LR_g)) + 2 * ((LR_g - 2) > (LR_s - 3) && (LR_g > LR_a)) + 3 * ((LR_a == LR_g) && (LR_g - 2) > (LR_s -
            3))

        if (model_ind == 1) {
            model_ch <- model_list[2]
        }

        if (model_ind == 2) {
            model_ch <- model_list[1]
        }

        if (model_ind == 3) {
            model_ch <- c("XCI")
        }

        infor_g <- infor_table(x = summary(Model_g)$coefficient, snp = snp, covar_n = rownames(summary(Model_g)$coefficient), MAF_value = MAF_value, model)
        loglik_infor <- t(c(stats::logLik(Model_bl), stats::logLik(Model_g), LR_g))
        colnames(loglik_infor) <- c("Baseline", "Full model", "Loglik ratio")
        mylist_g <- list(coefficients = infor_g, loglik = loglik_infor)

        infor_a <- infor_table(x = summary(Model_a)$coefficient, snp = snp, covar_n = rownames(summary(Model_a)$coefficient), MAF_value = MAF_value, model)
        loglik_infor <- t(c(stats::logLik(Model_bl), stats::logLik(Model_a), LR_a))
        colnames(loglik_infor) <- c("Baseline", "Full model", "Loglik ratio")
        mylist_a <- list(coefficients = infor_a, loglik = loglik_infor)

        infor_s <- infor_table(x = summary(Model_s)$coefficient, snp = snp, covar_n = rownames(summary(Model_s)$coefficient), MAF_value = MAF_value, model)
        loglik_infor <- t(c(stats::logLik(Model_bl), stats::logLik(Model_s), LR_s))
        colnames(loglik_infor) <- c("Baseline", "Full model", "Loglik ratio")
        mylist_s <- list(coefficients = infor_s, loglik = loglik_infor, Gamma = gamma)

        mylist <- list(mylist_g, mylist_a, mylist_s, model_ch)
        names(mylist) <- c(model_list, "XCI-S", "Best model by AIC")

        return(mylist)

    }


}
