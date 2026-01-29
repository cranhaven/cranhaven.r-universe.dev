#Class implementing an Associative Classification Algorithm
  #Implements the CPAR-C of KEEL

CPAR_C <- function(train, test, delta=0.05, min_gain=0.7, alpha=0.66,
                   rules_prediction=5){
  alg <- RKEEL::R6_CPAR_C$new()
  alg$setParameters(train, test, delta, min_gain, alpha, rules_prediction)
  return (alg)
}

R6_CPAR_C <- R6::R6Class("R6_CPAR_C",

  inherit = AssociativeClassificationAlgorithm,

  public = list(

    #Public properties

    #Number of rules combining for every example (delta)
    delta = 0.05,

    #Minimum gain
    min_gain = 0.7,

    #Weight decay factor (alfa)
    alpha = 0.66,

    #Number of rules used in prediction
    rules_prediction = 5,


    #Public functions

    #Initialize function
    setParameters = function(train, test, delta=0.05, min_gain=0.7, alpha=0.66,
                             rules_prediction=5){

		  super$setParameters(train, test)

      #Check for constraints
      stopText <- ""

      if((hasMissingValues(train)) || (hasMissingValues(test))){
        stopText <- paste0(stopText, "Dataset has missing values and the algorithm does not accept it.\n")
      }

      if((hasContinuousData(train)) || (hasContinuousData(test))){
        stopText <- paste0(stopText, "Dataset has continuous data and the algorithm does not accept it.\n")
      }

      if(stopText != ""){
        stop(stopText)
      }

      self$delta <- delta
      self$min_gain <- min_gain
      self$alpha <- alpha
      self$rules_prediction <- rules_prediction

    }

  ),

  private = list(

    #Private properties

    #jar Filename
    jarName = "Clas-CPAR.jar",

    #algorithm name
    algorithmName = "CPAR-C",

    #String with algorithm name
    algorithmString = "Classification based on Predictive Association Rules (CPAR)",


    #Private functions

    #Get the text with the parameters for the config file
    getParametersText = function(){

      text <- ""
      text <- paste0(text, "Number of rules combining for every example (delta) = ", self$delta, "\n")
      text <- paste0(text, "Minimum Gain = ", self$min_gain, "\n")
      text <- paste0(text, "Weight decay factor (alfa) = ", self$alpha, "\n")
      text <- paste0(text, "Number of rules used in prediction = ", self$rules_prediction, "\n")

      return(text)

    }

  )
)
