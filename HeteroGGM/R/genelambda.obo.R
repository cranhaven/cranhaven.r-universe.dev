#' Generate tuning parameters
#'
#' @author Mingyang Ren
#' @usage genelambda.obo(nlambda1=10,lambda1_max=1,lambda1_min=0.05,
#'                       nlambda2=10,lambda2_max=1,lambda2_min=0.01,
#'                       nlambda3=10,lambda3_max=5,lambda3_min=0.5)
#' @description Generating a sequence of the tuning parameters (lambda1, lambda2, and lambda3).
#'
#' @param nlambda1 The numbers of lambda 1.
#' @param lambda1_max The maximum values of lambda 1.
#' @param lambda1_min The minimum values of lambda 1.
#' @param nlambda2 The numbers of lambda 2.
#' @param lambda2_max The maximum values of lambda 2.
#' @param lambda2_min The minimum values of lambda 2.
#' @param nlambda3 The numbers of lambda 3.
#' @param lambda3_max The maximum values of lambda 3.
#' @param lambda3_min The minimum values of lambda 3.
#'
#' @return A sequence of the tuning parameters (lambda1, lambda2, and lambda3).
#' @export
#'
#' @examples
#' lambda <- genelambda.obo(nlambda1=5,lambda1_max=0.5,lambda1_min=0.1, nlambda2=15,lambda2_max=1.5,
#'                          lambda2_min=0.1, nlambda3=10,lambda3_max=3.5,lambda3_min=0.5)
#' lambda
genelambda.obo <- function(nlambda1=10,lambda1_max=1,lambda1_min=0.05,
                          nlambda2=10,lambda2_max=1,lambda2_min=0.01,
                          nlambda3=10,lambda3_max=5,lambda3_min=0.5){

  ## -----------------------------------------------------------------------------------------------------------------
  ## The name of the function: genelambda.obo
  ## -----------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Generating a sequence of the tuning parameters (lambda1, lambda2, and lambda3).
  ## -----------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages: No
  ## -----------------------------------------------------------------------------------------------------------------
  ## Input:
  ## @ nlambda1, nlambda2, and nlambda3: The numbers of lambda 1 2 3.
  ## @ lambda1_min, lambda2_min, and lambda3_min: The minimum values of lambda 1 2 3.
  ## @ lambda1_max, lambda2_max, and lambda3_max: The maximum values of lambda 1 2 3.
  ## -----------------------------------------------------------------------------------------------------------------
  ## Output:
  ## @ lambda: a sequence of the tuning parameters (lambda1, lambda2, and lambda3).
  ## -----------------------------------------------------------------------------------------------------------------

  lambda1 = exp(seq(log(lambda1_max),log(lambda1_min),len= nlambda1))
  lambda2 =exp(seq(log(lambda2_max),log(lambda2_min),len= nlambda2))
  lambda3 =exp(seq(log(lambda3_max),log(lambda3_min),len= nlambda3))
  lambda = list(lambda1=lambda1,lambda2=lambda2,lambda3=lambda3)
  return(lambda)
}
