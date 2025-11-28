##############################################################
#' Generate Scenarios
#'
#' @description
#' Internal method to Generate scenarios based on parameter list variations.
#' @param params List of parameters where each parameter can vary across scenarios.
#' @return Nested list of scenarios.
#' @name gen_scenario
#' @examples
#' some_distr<-list(A=list(mu = 31.07, sigma = 0.28),
#' B=list(mu = c(34.4,31.4,25.6), sigma = 0.98, nu = 1.7),# note mu!
#' C=list(mu = 31.4, sigma = 0.34),
#' D=list(mu = 25.6, sigma = 0.24))
#' gen_scenario(some_distr)
#' @export
#' @importFrom purrr map_depth
setGeneric("gen_scenario",
           def=function(params="list") {
             standardGeneric("gen_scenario")
           })

#' @rdname gen_scenario
#' @export
setMethod("gen_scenario",
          definition=function(params="list") {

            if(length(params) == 0) {
              stop("The list is empty!")
            }
              # Determine the maximum length from the parameters
              max_len <- max(unlist(map_depth(params,2,length)))

              # Create a list to hold all scenarios
              scenarios <- vector("list", max_len)

              # Initialize each scenario with deep copies of the original structure
              for (i in seq_len(max_len)) {
                scenarios[[i]] <- lapply(params, function(varname) {
                  lapply(varname, function(param) {
                    ifelse(length(param) == 1,param,param[i])

                  })
                })
              }

              return(scenarios)
            })
