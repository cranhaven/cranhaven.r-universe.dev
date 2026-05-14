#' Prune viral load data
#'
#' This function removes the first viral load data point for specific subjects
#'
#' @param id vector of subject ids
#' @param which_ids vector of ids that should have the first point removed
#' @param subset data frame to which the function should be applied
#'
remove_vl0 <- function(id, which_ids, subset){
    if(id[1] %in% which_ids){ return(subset[-1,]) }
    else {return(subset)}
}


#' Transform viral load data
#'
#' This function takes the log10 transform of viral load data & checks for NAs
#'
#' @param VL vector of viral load data
#'
transformVL = function(VL){
    VL = log10(VL)

    if(any(is.na(VL))){
        print("Log10 transform produced NAs")
    }

    return(VL)
}


#' Transform parameters
#'
#' This function transforms parameter estimates according to user defined functions
#'
#' @param params vector of parameters
#' @param param_transform_fn vector of functions for parameter transformation
#'
get_transformed_params <- function(params, param_transform_fn){
    transformed_params = rep(NA, length(params))

    for(i in 1:length(params)){
        transformed_params[i] = param_transform_fn[[i]](params[i])
    }
    return(transformed_params)
}


