#' A S4 class used to handle formatting ODE-Equation and calculate the needed functions for the seeds-algorithm
#'
#' @slot modelStr a vector of strings describing the ODE
#' @slot measureStr a vector of strings representing the equation of the measurement function
#' @slot origEq a vector of strings containing the original model function
#' @slot measureFunction a vector of strings containing the original measurement function
#' @slot costateEq a vector of strings describing the costate equation
#' @slot JhT a matrix of strings describing the jacobian matrix of the measurement function
#' @slot jacobian a matrix of strings representing the jacobian matrix model equations
#' @slot costFunction a string containing the cost function
#' @slot hamiltonian a string representing the Hamilton function of the model
#' @slot dynamicElasticNet boolean that indicates if the system equation should be calculated for the dynamic elastic net
#' @slot parameters parameters of the model
#' @slot cond a slot to save conditionals in equations, which are used for formatting the c files
#' @slot nnStates vector indicating which states should have a non negative solution
#' 
#' @return Returns a s4 class object containing the needed equations for the costate equation
#' 
odeEquations <- setClass(
#name of Class
  "odeEquations",
  slots = c(
    modelStr = "character",
    measureStr = "character",
    origEq = "character",
    measureFunction = "character",
    costateEq = "character",
    JhT = "matrix",
    jacobian = "matrix",
    costFunction = "character",
    hamiltonian = "character",
    dynamicElasticNet = "logical",
    parameters = "character",
    cond = "list",
    nnStates = "numeric"
  ),

  prototype = list(
    modelStr = character(0),
    measureStr = character(0),
    origEq = character(0),
    measureFunction = character(0),
    costateEq = character(0),
    costFunction = character(0),
    JhT = matrix(list(), nrow = 2, ncol = 2),
    jacobian = matrix(list(), nrow = 2, ncol = 2),
    costFunction = character(0),
    hamiltonian = character(0),
    dynamicElasticNet = FALSE,
    parameters = character(0),
    cond = list(),
    nnStates = numeric(0)
  ),
  validity = function(object) {
    # ...some validation stuff...
  }
)

setMethod('initialize', "odeEquations", function(.Object, ...) {
  .Object <- callNextMethod()
  return(.Object)
})

setGeneric(name = "setCostateEq",
           def = function(theObject, costate) {
            standardGeneric("setCostateEq")
           }
)

setMethod(f = "setCostateEq",
          signature = "odeEquations",
          definition = function(theObject, costate) {
            theObject@costateEq <- costate

            return(theObject)
          }
)

setGeneric(name = "setOrigEq",
           def = function(theObject, origEq) {
            standardGeneric("setOrigEq")
           }
)

setMethod(f = "setOrigEq",
          signature = "odeEquations",
          definition = function(theObject, origEq) {
            theObject@origEq <- origEq

            return(theObject)
          }
)

setGeneric(name = "setJacobian",
           def = function(theObject, jacobian) {
            standardGeneric("setJacobian")
           }
)

setMethod(f = "setJacobian",
          signature = "odeEquations",
          definition = function(theObject, jacobian) {
            theObject@jacobian <- jacobian

            return(theObject)
          }
)

setGeneric(name = "setHamiltonian",
           def = function(theObject, hamiltonian) {
            standardGeneric("setHamiltonian")
           }
)

setMethod(f = "setHamiltonian",
          signature = "odeEquations",
          definition = function(theObject, hamiltonian) {
            theObject@hamiltonian <- hamiltonian

            return(theObject)
          }
)

setGeneric(name = "calculateCostate",
           def = function(theObject) {
            standardGeneric("calculateCostate")
           }
)

setMethod(f = "calculateCostate",
          signature = "odeEquations",
          definition = function(theObject) {
            tempList <- symbolicDiff(theObject)
            theObject@costateEq <- tempList$costate
            theObject@JhT <- tempList$JhT
            theObject@jacobian <- tempList$jacobian
            theObject@origEq <- tempList$origEq
            theObject@hamiltonian <- tempList$Hamilton
            return(theObject)
          }
)

setGeneric(name = "createModelEqClass",
           def = function(theObject, theModel) {
            standardGeneric("createModelEqClass")
           }
)

setMethod(f = "createModelEqClass",
          signature = "odeEquations",
          definition = function(theObject, theModel) {
            theObject@modelStr <- deparse(theModel, width.cutoff = 500)
            tempList <- getEquations(theModel)
            theObject@origEq <- tempList$strM
            theObject@parameters <- tempList$strP
            theObject@cond <- tempList$cond
            return(theObject)
          }

)

setGeneric(name = "setCostFunc",
           def = function(theObject, costFunction) {
            standardGeneric("setCostFunc")
           }
)

setMethod(f = "setCostFunc",
          signature = "odeEquations",
          definition = function(theObject, costFunction) {
            theObject@costFunction <- getEquations(costFunction)$strM
            theObject@dynamicElasticNet <- FALSE

            return(theObject)
          }
)

setGeneric(name = "setMeassureFunc",
           def = function(theObject, meassureFunc) {
            standardGeneric("setMeassureFunc")
           }
)

setMethod(f = "setMeassureFunc",
          signature = "odeEquations",
          definition = function(theObject, meassureFunc) {
            theObject@measureStr <- deparse(meassureFunc, width.cutoff = 500)
            theObject@measureFunction <- getEquations(meassureFunc)$strM
            return(theObject)
          }
)

setGeneric(name = "isDynElaNet",
           def = function(theObject) {
            standardGeneric("isDynElaNet")
           }
)

setMethod(f = "isDynElaNet",
          signature = "odeEquations",
          definition = function(theObject) {
            theObject@dynamicElasticNet <- TRUE
            return(theObject)
          }
)



