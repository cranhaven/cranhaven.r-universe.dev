#' Evaluate error metric between data and model prediction
#'
#' For a given parameter set, this function computes the predicted viral load curve and evaluates the error metric between the prediction and observed data (to be passed to optim).
#'
#' @param params named vector of the parameters from which the model prediction should be generated.
#' @param param_names names of parameter vector.
#' @param free_param_index logical TRUE/FALSE vector indicating whether the parameters A, delta, B, gamma are to be recovered. This should be c(TRUE, TRUE, TRUE, TRUE) for the biphasic model and c(FALSE, FALSE, TRUE, TRUE) for the single phase model.
#' @param data dataframe with columns for the subject's viral load measurements ('vl'), and timing of sampling ('time')
#' @param model_list character indicating which model is begin fit. Can be either 'four' for the biphasic model, or 'two' for the single phase model.
#' @param inv_param_transform_fn list of transformation functions to be used when back-transforming the transformed parameters. Should be the inverse of the forward transformation functions. Defaults to exponential.
#'
get_error <- function(params, param_names, free_param_index, data, model_list,
                      inv_param_transform_fn){

    # Free and fixed params are log-transformed so their values are unconstrained during optimization
    # For model evaluations they must first be back-transformed
    untransformed_params <- get_transformed_params(params = params, param_transform_fn = inv_param_transform_fn[free_param_index])
    names(untransformed_params) <- param_names[free_param_index]

    # Simulate VL with current parameters --------------------
    timevec <- data$time
    VLdata <- data$vl

    # NB type of model fit depends on the # fitted params (2 = single phase, 4 = biphasic)
    if (model_list == 'two'){

        VLoutput <- get_singlephase(params = untransformed_params, timevec)

    }  else if (model_list == 'four'){

        VLoutput <- get_biphasic(params = untransformed_params, timevec)
    }

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


#' Fit model to data using optim
#'
#' This function uses optim to fit either the biphasic or single phase model to data from a given subject
#' @param param_names names of parameter vector.
#' @param initial_params named vector of the initial parameter guess.
#' @param free_param_index logical vector indicating whether the parameters A, delta, B, gamma are to be recovered. This should be c(TRUE, TRUE, TRUE, TRUE) for the biphasic model and c(FALSE, FALSE, TRUE, TRUE) for the single phase model.
#' @param data dataframe with columns for the subject's viral load measurements ('vl'), and timing of sampling ('time')
#' @param model_list character indicating which model is begin fit. Can be either 'four' for the biphasic model, or 'two' for the single phase model. Defaults to 'four'.
#' @param forward_param_transform_fn list of transformation functions to be used when fitting the model in optim. Defaults to log transformations for all parameters (to allow unconstrained optimization).
#' @param inv_param_transform_fn list of transformation functions to be used when back-transforming the transformed parameters. Should be the inverse of the forward transformation functions. Defaults to exponential.
#' @param searchmethod optimization algorithm to be used in optim. Defaults to Nelder-Mead.
#'
get_optim_fit <- function(initial_params, param_names, free_param_index, data,
                          model_list = "four",
                          forward_param_transform_fn = forward_param_transform_fn,
                          inv_param_transform_fn = inv_param_transform_fn,
                          searchmethod){

    tmp_params <- initial_params[free_param_index]

    transformed_params <- get_transformed_params(params = tmp_params, param_transform_fn = forward_param_transform_fn[free_param_index])

    fit <- stats::optim(par = transformed_params, # fitting free parameters only
                 fn = get_error,
                 method = searchmethod,
                 param_names = param_names,
                 free_param_index = free_param_index,
                 data = data,
                 inv_param_transform_fn = inv_param_transform_fn,
                 model_list = model_list,
                 hessian = TRUE)

    return(fit)
}



#' Fit model and obtain parameter estimates
#'
#' This function fits either the biphasic or single phase model to the processed data and extracts the best-fit parameters.
#'
#' @param data dataframe with columns for each subject's identifier ('id'), viral load measurements ('vl'), and timing of sampling ('time')
#' @param id_vector vector of identifiers corresponding to the subjects to be fitted.
#' @param param_names names of parameter vector.
#' @param initial_params named vector of the initial parameter guess.
#' @param free_param_index logical vector indicating whether the parameters A, delta, B, gamma are to be recovered. This should be c(TRUE, TRUE, TRUE, TRUE) for the biphasic model and c(FALSE, FALSE, TRUE, TRUE) for the single phase model.
#' @param n_min_biphasic the minimum number of data points required to fit the biphasic model. Defaults to 6. It is highly advised not to go below this threshold.
#' @param model_list character indicating which model is to be fit. Can be either 'four' for the biphasic model, or 'two' for the single phase model. Defaults to 'four'.
#' @param whichcurve indicates which model prediction function to use. Should be get_biphasic for the biphasic model or get_singlephase for the singlephase model. Defaults to get_biphasic.
#' @param forward_param_transform_fn list of transformation functions to be used when fitting the model in optim. Defaults to log transformations for all parameters (to allow unconstrained optimization).
#' @param inv_param_transform_fn list of transformation functions to be used when back-transforming the transformed parameters. Should be the inverse of the forward transformation functions. Defaults to exponential.
#' @param searchmethod optimization algorithm to be used in optim. Defaults to Nelder-Mead.
#'
fit_model <- function(data, id_vector, param_names,
                      initial_params, free_param_index,
                      n_min_biphasic,
                      model_list, whichcurve = get_biphasic,
                      forward_param_transform_fn, inv_param_transform_fn,
                      searchmethod){

    fitlist <- vector("list", length = length(id_vector))
    CIlist <- vector("list", length = length(id_vector))
    model_fitlist <- vector("list", length = length(id_vector))

    nofitlist <- vector("list", length = length(id_vector))
    noCIlist <- vector("list", length = length(id_vector))

    for (i in 1:length(id_vector)){
        datasubset = data %>% filter(id == id_vector[i])

        # For biphasic model: if there are less than user defined 'n_min_biphasic' dps, store as failed fit and move onto the next subject
        if( (nrow(datasubset) < n_min_biphasic) && (model_list == "four")){

            nofitlist[[i]] <- data.frame(index = i, id = id_vector[i], reason = "not enough data")
            next
        }

        fitlist[[i]] <- data.frame(index = i, id = id_vector[i])

        fit <- get_optim_fit(initial_params, param_names, free_param_index, data = datasubset,
                             forward_param_transform_fn = forward_param_transform_fn,
                             inv_param_transform_fn = inv_param_transform_fn,
                             model_list = model_list,
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
            } else{
                CIlist[[i]] <- tmpCI %>% mutate(id = id_vector[i])

                model_fitlist[[i]] <- get_curve(data = datasubset, best_param, param_names = param_names[free_param_index], whichcurve = whichcurve)
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


