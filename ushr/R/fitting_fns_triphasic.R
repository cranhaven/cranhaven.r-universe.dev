#' Compute the triphasic model curve
#'
#' This function calculates the triphasic model, V(t), for a vector of input times, t
#' @param params named numeric vector of all parameters needed to compute the triphasic model, V(t)
#' @param timevec numeric vector of the times, t, at which V(t) should be calculated
#' @return numeric vector of viral load predictions, V(t), for each time point in 'timevec'
#' @export
#' @examples
#'
#' get_triphasic(params = c(A = 10000, delta = 1, B = 1000, gamma = 0.1, C = 100, omega = 0.03),
#'              timevec = seq(1, 100, length.out = 100))
#'
get_triphasic <- function(params, timevec){
    if(length(params) < 6){
        stop("The triphasic model needs 6 parameters: A, delta, A_b, delta_b, B, gamma")
    }
    params["A"] * exp(- params["delta"] * timevec) + params["A_b"] * exp(- params["delta_b"] * timevec + params["B"] * exp(- params["gamma"] * timevec))
}


#' Evaluate error metric between data and model prediction
#'
#' For a given parameter set, this function computes the predicted viral load curve and evaluates the error metric between the prediction and observed data (to be passed to optim).
#'
#' @param params named vector of the parameters from which the model prediction should be generated.
#' @param param_names names of parameter vector.
#' @param free_param_index logical TRUE/FALSE vector indicating whether the parameters A, delta, A_b, delta_b, B, gamma are to be recovered. This should be c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE) for the triphasic model.
#' @param data dataframe with columns for the subject's viral load measurements ('vl'), and timing of sampling ('time').
#' @param inv_param_transform_fn list of transformation functions to be used when back-transforming the transformed parameters. Should be the inverse of the forward transformation functions. Defaults to exponential.
#'
get_error_triphasic <- function(params, param_names, free_param_index, data,
                      inv_param_transform_fn){

    # Free and fixed params are transformed so their values are unconstrained during optimization
    # For model evaluations they must first be back-transformed
    untransformed_params <- get_transformed_params(params = params,
                                                          param_transform_fn = inv_param_transform_fn[free_param_index])
    names(untransformed_params) <- param_names[free_param_index]

    # Simulate VL with current parameters --------------------
    timevec <- data$time
    VLdata <- data$vl

    VLoutput <- get_triphasic(params = untransformed_params, timevec)

    ## Calculate SSRs and associated errors --------------------

    # 1. transform VL data and output to log10 scale
    transformed_data <- transformVL(VLdata)
    transformed_output <- transformVL(VLoutput)

    # 2. calculate residuals and SSRs
    resids <- transformed_data - transformed_output
    SSRs <- sum(resids^2)

    # 3. calculate error to be minimised: negloglik from Hogan et al (2015)
    # (correct up to a constant for each time-series)
    negloglik <- 0.5 * length(VLdata) * log(SSRs)

    return(negloglik)
}



#' Fit model and obtain parameter estimates
#'
#' This function fits the triphasic model to the processed data and extracts the best-fit parameters.
#'
#' @param data dataframe with columns for each subject's identifier ('id'), viral load measurements ('vl'), and timing of sampling ('time')
#' @param id_vector vector of identifiers corresponding to the subjects to be fitted.
#' @param param_names names of parameter vector.
#' @param initial_params named vector of the initial parameter guess.
#' @param free_param_index logical vector indicating whether the parameters A, delta, A_b, delta_b, B, gamma are to be recovered. This should be c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE) for the triphasic model.
#' @param n_min_triphasic the minimum number of data points required to fit the triphasic model.
#' @param forward_param_transform_fn list of transformation functions to be used when fitting the model in optim. Defaults to log transformations for all parameters (to allow unconstrained optimization).
#' @param inv_param_transform_fn list of transformation functions to be used when back-transforming the transformed parameters. Should be the inverse of the forward transformation functions. Defaults to exponential.
#' @param searchmethod optimization algorithm to be used in optim. Defaults to Nelder-Mead.
#'
fit_model_triphasic <- function(data, id_vector, param_names,
                      initial_params, free_param_index,
                      n_min_triphasic,
                      forward_param_transform_fn, inv_param_transform_fn,
                      searchmethod){

    fitlist <- vector("list", length = length(id_vector))
    CIlist <- vector("list", length = length(id_vector))
    model_fitlist <- vector("list", length = length(id_vector))

    nofitlist <- vector("list", length = length(id_vector))
    noCIlist <- vector("list", length = length(id_vector))

    for (i in 1:length(id_vector)){
        datasubset = data %>% filter(id == id_vector[i])

        # If there are less than user defined 'n_min_triphasic'dps,
        # store as failed fit and move onto the next subject
        if (nrow(datasubset) < n_min_triphasic) {
            nofitlist[[i]] <- data.frame(index = i, id = id_vector[i], reason = "not enough data")
            next
        }

        fitlist[[i]] <- data.frame(index = i, id = id_vector[i])

        fit <- get_optim_fit_triphasic(initial_params, param_names, free_param_index, data = datasubset,
                             forward_param_transform_fn = forward_param_transform_fn,
                             inv_param_transform_fn = inv_param_transform_fn,
                             searchmethod = searchmethod)

        best_param <- get_params(fit, initial_params, free_param_index, param_names, inv_param_transform_fn, index = i)

        # To get CIs, the Hessian has to be invertible (determinant nonzero).
        # record id of patients for which CIs cannot be obtained
        # for those which do have CIs (i.e. have reliable fits) - get fitted model prediction

        if (det(fit$hessian)!=0 & all(!is.na(fit$hessian)) ) {

            trySolve <- try(solve(fit$hessian))

            if (inherits(trySolve, "try-error")) {
                tmpCI <- NA
            } else {
                tmpCI <- get_CI(fit)
            }

            if( any(is.na(tmpCI) ) ){
                noCIlist[[i]] <- data.frame(index = i, id = id_vector[i], reason = "could not obtain CIs")
            } else {
                CIlist[[i]] <- tmpCI %>% mutate(id = id_vector[i])

                model_fitlist[[i]] <- get_curve(data = datasubset, best_param,
                                                       param_names = param_names[free_param_index], whichcurve = get_triphasic)
            }
        } else {
            noCIlist[[i]] <- data.frame(index = i, id = id_vector[i], reason = "could not obtain CIs")
        }
    }

    notfitted  <- bind_rows(nofitlist) %>% rbind( bind_rows(noCIlist))

    if (nrow(notfitted)) {
        notfitted <- notfitted %>% filter(!is.na(index)) %>% arrange(index)
    }

    if(length(notfitted$id) > 0){
        fitted <- bind_rows(fitlist) %>% filter(!id %in% notfitted$id)
    } else{
        fitted <- bind_rows(fitlist)
    }

    return(list(model_fitlist = model_fitlist, CIlist = CIlist, fitted = fitted, notfitted = notfitted))

}



#' Fit triphasic model to data using optim
#'
#' This function uses optim to fit the triphasic model to data from a given subject
#' @param param_names names of parameter vector.
#' @param initial_params named vector of the initial parameter guess.
#' @param free_param_index logical vector indicating whether the parameters A, delta, A_b, delta_b, B, gamma are to be recovered. This should be c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE) for the triphasic model.
#' @param data dataframe with columns for the subject's viral load measurements ('vl'), and timing of sampling ('time')
#' @param forward_param_transform_fn list of transformation functions to be used when fitting the model in optim. Defaults to log transformations for all parameters (to allow unconstrained optimization).
#' @param inv_param_transform_fn list of transformation functions to be used when back-transforming the transformed parameters. Should be the inverse of the forward transformation functions. Defaults to exponential.
#' @param searchmethod optimization algorithm to be used in optim. Defaults to Nelder-Mead.
#'
get_optim_fit_triphasic <- function(initial_params, param_names, free_param_index, data,
                          forward_param_transform_fn = forward_param_transform_fn,
                          inv_param_transform_fn = inv_param_transform_fn,
                          searchmethod){

    tmp_params <- initial_params[free_param_index]

    transformed_params <- get_transformed_params(params = tmp_params,
                                                        param_transform_fn = forward_param_transform_fn[free_param_index])

    fit <- stats::optim(par = transformed_params,
                        fn = get_error_triphasic,
                        method = searchmethod,
                        param_names = param_names,
                        free_param_index = free_param_index,
                        data = data,
                        inv_param_transform_fn = inv_param_transform_fn,
                        hessian = TRUE)

    return(fit)
}




#' Switch names of rate parameters
#'
#' This function switches the names of delta and gamma estimates if gamma > delta.
#' @param triphasicCI data frame of parameter estimates and confidence intervals for the biphasic model.
#'
tri_switch_params <- function(triphasicCI){
    replace_cols <- c("estimate", "lowerCI", "upperCI")

    if (triphasicCI$estimate[triphasicCI$param == "gamma"] > triphasicCI$estimate[triphasicCI$param == "delta_b"]) {

        tmpRate <- triphasicCI[triphasicCI$param == "gamma", replace_cols]
        tmpConst <- triphasicCI[triphasicCI$param == "B", replace_cols]

        triphasicCI[triphasicCI$param == "gamma", replace_cols] <- triphasicCI[triphasicCI$param == "delta_b", replace_cols]
        triphasicCI[triphasicCI$param == "delta_b", replace_cols] <- tmpRate

        triphasicCI[triphasicCI$param == "B", replace_cols] <- triphasicCI[triphasicCI$param == "A_b", replace_cols]
        triphasicCI[triphasicCI$param == "A_b", replace_cols] <- tmpConst
    }

    if (triphasicCI$estimate[triphasicCI$param == "delta_b"] > triphasicCI$estimate[triphasicCI$param == "delta"]) {

        tmpRate <- triphasicCI[triphasicCI$param == "delta_b", replace_cols]
        tmpConst <- triphasicCI[triphasicCI$param == "A_b", replace_cols]

        triphasicCI[triphasicCI$param == "delta_b", replace_cols] <- triphasicCI[triphasicCI$param == "delta", replace_cols]
        triphasicCI[triphasicCI$param == "delta", replace_cols] <- tmpRate

        triphasicCI[triphasicCI$param == "A_b", replace_cols] <- triphasicCI[triphasicCI$param == "A", replace_cols]
        triphasicCI[triphasicCI$param == "A", replace_cols] <- tmpConst
    }

    if (triphasicCI$estimate[triphasicCI$param == "gamma"] > triphasicCI$estimate[triphasicCI$param == "delta_b"]) {

        tmpRate <- triphasicCI[triphasicCI$param == "gamma", replace_cols]
        tmpConst <- triphasicCI[triphasicCI$param == "B", replace_cols]

        triphasicCI[triphasicCI$param == "gamma", replace_cols] <- triphasicCI[triphasicCI$param == "delta_b", replace_cols]
        triphasicCI[triphasicCI$param == "delta_b", replace_cols] <- tmpRate

        triphasicCI[triphasicCI$param == "B", replace_cols] <- triphasicCI[triphasicCI$param == "A_b", replace_cols]
        triphasicCI[triphasicCI$param == "A_b", replace_cols] <- tmpConst
    }

    return(triphasicCI)
}

