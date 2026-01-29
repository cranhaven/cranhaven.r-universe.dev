#Class implementing an Associative Classification Algorithm
  #Implements the CMAR-C of KEEL

CMAR_C <- function(train, test, min_confidence=0.5, min_support=0.01,
                   databaseCoverage=4){
  alg <- RKEEL::R6_CMAR_C$new()
  alg$setParameters(train, test, min_confidence, min_support, databaseCoverage)
  return (alg)
}

R6_CMAR_C <- R6::R6Class("R6_CMAR_C",

  inherit = AssociativeClassificationAlgorithm,

  public = list(

    #Public properties

    #Minimum Confidence
    min_confidence = 0.5,

    #Minimum Support
    min_support = 0.01,

    #Database coverage Threshold (delta)
    databaseCoverage = 4,

    #Public functions

    #Initialize function
    setParameters = function(train, test, min_confidence=0.5, min_support=0.01,
                             databaseCoverage=4){

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

      self$min_confidence <- min_confidence
		  self$min_support <- min_support
      self$databaseCoverage <- databaseCoverage

    }

  ),

  private = list(

    #Private properties

    #jar Filename
    jarName = "Clas-CMAR.jar",

    #algorithm name
    algorithmName = "CMAR-C",

    #String with algorithm name
    algorithmString = "Accurate and Efficient Classification Based on Multiple Class Association Rules (CMAR)",


    #Private functions

    #Get the text with the parameters for the config file
    getParametersText = function(){

      text <- ""
      text <- paste0(text, "Minimum Confidence = ", self$min_confidence, "\n")
      text <- paste0(text, "Minimum Support = ", self$min_support, "\n")
      text <- paste0(text, "Database Coverage Threshold (delta) = ", self$databaseCoverage, "\n")

      return(text)

    }

  )
)
