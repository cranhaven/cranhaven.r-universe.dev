#' A function that add a likelihood for estimation to the pscObject
#'
#' The purpose of this function is to include the appropriate likelihood to the
#' psc object for estimation procedures
#'
#' @param CFM A counter factual model
#' @return a likelihood function
#' @importFrom survival Surv
pscData_addLik <- function(CFM){

  ret <- "No Likelihood specified"

  if("glm"%in%CFM$mod_class){
    ret <- lik.glm
  }

  if("flexsurvreg"%in%CFM$mod_class){
    ret <- lik.flexsurvreg
  }

  ret
}
