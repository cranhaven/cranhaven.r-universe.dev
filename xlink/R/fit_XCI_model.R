#' Model fitting results for each SNP considering as XCI type
#'
#' \code{fit_XCI_model} returns model fitting results for each SNP understanding as XCI type.
#'
#' @param resp Response variable for continuous or binary model fitting.
#' @param os Survival indicator, 1 for death, 0 for censoring.
#' @param ostime Duration time of survival.
#' @param snp  Single SNP name.
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
fit_XCI_model <- function(resp, os, ostime, snp, gender, male, female, covars, model, data) {

    MAF_value <- MAF(snp = snp, gender = gender, male = male, MAF_v = 0, data = data)[2]

    if (model == "survival") {
        var_list <- c(os, ostime, snp, gender, covars)
        var_n <- length(var_list)
        data <- data[, var_list]

        snp_var <- data[, snp]
        n_na <- (!is.na(snp_var))
        data <- data[n_na, ]

        male_snp <- data[data[, 4] == male, 3]
        snp_type <- sum(levels(as.factor(male_snp)) %in% c("2"))


        if (snp_type == 0) {
            new_col <- data[, 3]
            new_col[((data[, 4] == male) & (data[, 3] == 1))] <- 2
            data <- cbind(data, new_col)
            snp_new <- paste(snp, "XCI", sep = "_")
            colnames(data)[var_n + 1] <- snp_new
            formula <- paste("Surv", "(", ostime, ",", os, ")", "~", snp_new, "+", gender)
        } else {
            formula <- paste("Surv", "(", ostime, ",", os, ")", "~", snp, "+", gender)
        }

        formula_bl <- paste("Surv", "(", ostime, ",", os, ")", "~", gender)


        if (length(covars) != 0) {
            covar_formula <- paste(covars, collapse = "+")
            formula_bl <- paste(formula_bl, covar_formula, sep = "+")
            formula <- paste(formula, covar_formula, sep = "+")
        }

        Model_o <- survival::coxph(stats::as.formula(formula_bl), data <- data)
        Model <- survival::coxph(stats::as.formula(formula), data <- data)

        LR_AIC <- Model$loglik[2] - Model_o$loglik[2]

        infor <- infor_table(x = summary(Model)$coefficients, snp = snp, covar_n = rownames(summary(Model)$coefficients), MAF_value = MAF_value, model)

        loglik_infor <- t(c(Model_o$loglik[2], Model$loglik[2], LR_AIC))
        colnames(loglik_infor) <- c("Baseline", "Full model", "Loglik ratio")

        mylist <- list(coefficients = infor, loglik = loglik_infor)

        return(mylist)
    }

    if (model == "binary") {
        var_list <- c(resp, snp, gender, covars)
        var_n <- length(var_list)

        data <- data[, var_list]

        snp_var <- data[, snp]
        n_na <- (!is.na(snp_var))
        data <- data[n_na, ]

        male_snp <- data[data[, 3] == male, 2]
        snp_type <- sum(levels(as.factor(male_snp)) %in% c("2"))

        if (snp_type == 0) {
            new_col <- data[, 2]
            new_col[((data[, 3] == male) & (data[, 2] == 1))] <- 2
            data <- cbind(data, new_col)
            snp_new <- paste(snp, "XCI", sep = "_")
            colnames(data)[var_n + 1] <- snp_new
            formula <- paste(resp, "~", snp_new, "+", gender)
        } else {
            formula <- paste(resp, "~", snp, "+", gender)
        }


        formula_bl <- paste(resp, "~", gender)

        if (length(covars) != 0) {
            covar_formula <- paste(covars, collapse = "+")
            formula_bl <- paste(formula_bl, covar_formula, sep = "+")
            formula <- paste(formula, covar_formula, sep = "+")
        }

        Model_bl <- stats::glm(stats::as.formula(formula_bl), data <- data, family = binomial(link = "logit"))
        Model <- stats::glm(stats::as.formula(formula), data <- data, family = binomial(link = "logit"))

        LR_AIC <- stats::logLik(Model) - stats::logLik(Model_bl)

        infor <- infor_table(x = summary(Model)$coefficients, snp = snp, covar_n = rownames(summary(Model)$coefficients), MAF_value = MAF_value, model)

        loglik_infor <- t(c(stats::logLik(Model_bl), stats::logLik(Model), LR_AIC))
        colnames(loglik_infor) <- c("Baseline", "Full model", "Loglik ratio")

        mylist <- list(coefficients = infor, loglik = loglik_infor)

        return(mylist)

    }

    if (model == "linear") {
        var_list <- c(resp, snp, gender, covars)
        var_n <- length(var_list)

        data <- data[, var_list]

        snp_var <- data[, snp]
        n_na <- (!is.na(snp_var))
        data <- data[n_na, ]

        male_snp <- data[data[, 3] == male, 2]
        snp_type <- sum(levels(as.factor(male_snp)) %in% c("2"))

        if (snp_type == 0) {
            new_col <- data[, 2]
            new_col[((data[, 3] == male) & (data[, 2] == 1))] <- 2
            data <- cbind(data, new_col)
            snp_new <- paste(snp, "XCI", sep = "_")
            colnames(data)[var_n + 1] <- snp_new
            formula <- paste(resp, "~", snp_new, "+", gender)
        } else {
            formula <- paste(resp, "~", snp, "+", gender)
        }


        formula_bl <- paste(resp, "~", gender)

        if (length(covars) != 0) {
            covar_formula <- paste(covars, collapse = "+")
            formula_bl <- paste(formula_bl, covar_formula, sep = "+")
            formula <- paste(formula, covar_formula, sep = "+")
        }

        Model_bl <- stats::lm(stats::as.formula(formula_bl), data <- data)
        Model <- stats::lm(stats::as.formula(formula), data <- data)

        LR_AIC <- stats::logLik(Model) - stats::logLik(Model_bl)

        infor <- infor_table(x = summary(Model)$coefficient, snp = snp, covar_n = rownames(summary(Model)$coefficient), MAF_value = MAF_value, model)

        loglik_infor <- t(c(stats::logLik(Model_bl), stats::logLik(Model), LR_AIC))
        colnames(loglik_infor) <- c("Baseline", "Full model", "Loglik ratio")

        mylist <- list(coefficients = infor, loglik = loglik_infor)

        return(mylist)

    }


}
