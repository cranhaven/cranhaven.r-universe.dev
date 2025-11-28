##############################################################
#' Modify Parameters Box
#'
#' @description
#' Internal method to modify specific parameters in a nested list structure by applying deviations.
#'
#' @param all_parms nested list of parameters from the pbox object.
#' @param params_list Named list where each name corresponds to a variable in the dataset
#'        and the value is a vector of parameter names to modify (e.g. list(Vietnam="mu")).
#' @param sigma Standard deviation used for calculating parameter deviations.
#' @param range Range values for generating deviations.
#' @return Modified list of parameters.
#' @name modify_pbox
#' @examples
#' some_distr<-list(A=list(mu = 31.07, sigma = 0.28),
#' B=list(mu = 34.4, sigma = 0.98, nu = 1.7),
#' C=list(mu = 31.4, sigma = 0.34),
#' D=list(mu = 25.6, sigma = 0.24))
#' modify_pbox(some_distr, list(A = "mu"))
#' @export
setGeneric("modify_pbox",
           def=function(all_parms, params_list, sigma = 0.05, range = seq(-3, 3, 1)) {
             standardGeneric("modify_pbox")
           })

#' @rdname modify_pbox
#' @export
setMethod("modify_pbox",
          definition=function(all_parms, params_list, sigma = 0.05, range = seq(-3, 3, 1)) {

            if(is.null(names(params_list))){
              stop("Unamed params_list. Each name must corresponds to a variable in the dataset!")
            }
            modified_parms <- rapply(all_parms, identity, how = "replace")
            for (colvar in names(params_list)) {
              param_names <- params_list[[colvar]]
              if (is.character(param_names)) {
                param_names <- as.list(param_names)
              }
              for (param_name in param_names) {
                if (!is.null(modified_parms[[colvar]][[param_name]])) {
                  modified_parms[[colvar]][[param_name]] <- param_dev(modified_parms[[colvar]][[param_name]], sigma, range)
                }
              }
            }
            return(modified_parms)
          })
