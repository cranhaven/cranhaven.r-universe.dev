#' Extract fitted parameters
#'
#' This function extracts all untransformed parameters from the output of optim (i.e. the fitted model).
#' @param fit the output of optim i.e. the fitted model for a particular subject
#' @param initial_params named vector of the initial parameter guess
#' @param free_param_index logical TRUE/FALSE vector indicating whether the parameters A, delta, B, gamma are to be recovered. This should be c(TRUE, TRUE, TRUE, TRUE) for the biphasic model and c(FALSE, FALSE, TRUE, TRUE) for the single phase model.
#' @param param_names character vector of the parameter names. This should be c("A", "delta", "B", "gamma") for the biphasic model or c("B", "gamma") for the single phase model.
#' @param inv_param_transform_fn list of transformation functions to be used when back-transforming the transformed parameters. Should be the inverse of the forward transformation functions.
#' @param index indicator value used inside the master function to indicate the subject number.
#'
get_params = function(fit, initial_params, free_param_index, param_names, inv_param_transform_fn, index = NULL){

    # get initial guesses
    final_params <- initial_params

    # get the transformed, fitted parameters
    fitted_params <- fit$par

    # now untransform these values
    untransformed_fitted_params <- get_transformed_params(params = fitted_params, param_transform_fn = inv_param_transform_fn)

    # replace the initial guesses of the free parameters with the fitted values
    final_params[free_param_index] <- untransformed_fitted_params

    return(c(final_params[free_param_index], index = index) )
}


#' Calculate parameter confidence intervals
#'
#' This function calculates parameter 95% confidence intervals from the fitted model (using the hessian supplied from optim).
#' @param fit the output of optim i.e. the fitted model for a particular subject
#'
get_CI <- function(fit){

    if( det(fit$hessian) == 0){
        stop("The determinant of the Hessian is zero: cannot calculate confidence intervals")
    }

    fisher_info <- solve(fit$hessian)
    prop_sigma <- sqrt(diag(fisher_info))

    upper <- fit$par + 1.96 * prop_sigma
    lower <- fit$par - 1.96 * prop_sigma

    interval <- data.frame(estimate = fit$par, upperCI = upper, lowerCI = lower)

    return(exp(interval))
}


#' Make parameter summary table
#'
#' This function collate confidence intervals and parameter estimates from all subjects (fitted with the same model) into a nice table.
#' @param CIlist a list of confidence intervals and parameter estimates obtained from fitting either the single or biphasic model to each eligible subject.
#' @param param_names character vector of the parameter names. This should be c("A", "delta", "B", "gamma") for the biphasic model or c("B", "gamma") for the single phase model.
#' @param free_param_index logical vector indicating whether the parameters A, delta, B, gamma are to be included. This should be c(TRUE, TRUE, TRUE, TRUE) for the biphasic model and c(FALSE, FALSE, TRUE, TRUE) for the single phase model.
#' @param fitted data frame with an 'id' column of the unique identifiers for each subject represented in CIlist. Identifiers should be ordered according to their appearance in CIlist.
#'
get_CItable <- function(CIlist, param_names, free_param_index, fitted){

    CItable <- bind_rows(CIlist) %>%
        mutate(param = rep(param_names[free_param_index], times = nrow(fitted)),
               id = rep(fitted$id, each = length(param_names[free_param_index]))) %>%
        mutate(CIrange = upperCI - lowerCI, relativerange = CIrange/lowerCI)

    return(CItable)
}


#' Switch names of rate parameters
#'
#' This function switches the names of delta and gamma estimates if gamma > delta.
#' @param biphasicCI data frame of parameter estimates and confidence intervals for the biphasic model.
#'
switch_params <- function(biphasicCI){
    replace_cols <- c("estimate", "lowerCI", "upperCI")

    if (biphasicCI$estimate[biphasicCI$param == "gamma"] > biphasicCI$estimate[biphasicCI$param == "delta"]) {

        tmpRate <- biphasicCI[biphasicCI$param == "gamma", replace_cols]
        tmpConst <- biphasicCI[biphasicCI$param == "B", replace_cols]

        biphasicCI[biphasicCI$param == "gamma", replace_cols] <- biphasicCI[biphasicCI$param == "delta", replace_cols]
        biphasicCI[biphasicCI$param == "delta", replace_cols] <- tmpRate

        biphasicCI[biphasicCI$param == "B", replace_cols] <- biphasicCI[biphasicCI$param == "A", replace_cols]
        biphasicCI[biphasicCI$param == "A", replace_cols] <- tmpConst
    }
    return(biphasicCI)
}

