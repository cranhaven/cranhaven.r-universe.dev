#####################################################################
#' Scenario Analysis
#'
#' @description
#' Performs scenario analysis by modifying underlying parameters of a pbox object.
#' Query the probabilistic space under different scenarios with different combinations of parameters for a single query.
#'
#' @param pbx object of class pbox
#' @param param_list List specifying which parameters to modify.
#' @param sigma Standard deviation for parameter deviations, defaulting to 0.05.
#' @param range Range of deviation multipliers, default is seq(-3, 3, 1).
#' @param ... Additional arguments passed to qpbox.
#' @return Named list of results from each scenario evaluation.
#' @name scenario_pbox
#' @examples
#'   data("SEAex")
#'   pbx <- set_pbox(SEAex)
#'   scenario_pbox(pbx,mj = "Vietnam:31 & avgRegion:26", param_list = list(Vietnam="mu"))
#'
#' @export
setGeneric("scenario_pbox", function(pbx,param_list="list",sigma=0.05, range=seq(-3,3,1), ...) {
  standardGeneric("scenario_pbox")
})

#' @rdname scenario_pbox
#' @export

setMethod("scenario_pbox", signature = "pbox",
          definition = function(pbx,param_list="list",sigma=0.05, range=seq(-3,3,1), ...){

            if (!inherits(pbx, c("pbox"))) {
              stop("Input must be a pbox object!")
            }

            if(length(param_list) == 0) {
              stop("The list is empty!")
            }

            if(!is.numeric(range)) {
              stop("Non-numeric range input!")
            }

            if(!all(unique(unlist(unname(param_list)))%in%unique(names(unlist(pbx@copula@paramMargins))))){
              stop("The parameters you wish to modify do not exist in the current copula margins!")
            }


            allParms<-pbx@copula@paramMargins
            names(allParms)<-names(pbx@data)

            if(!all(names(param_list)%in% names(allParms))){
              stop("The variables you wish to modify do not exist in the dataframe!")
            }

            deviation_results<-modify_pbox(all_parms =allParms,params_list = param_list,sigma, range)
            scenarios<-gen_scenario(deviation_results)


            scenario_res<-lapply(scenarios,function(x){

              perCop<-copy(pbx)
              perCop@copula@paramMargins<-unname(x)
              qpbox(perCop,...)
            })
            names(scenario_res)<-paste0("SD",range)
            return(scenario_res)
          })


