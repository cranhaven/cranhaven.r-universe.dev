#Class implementing an Associative Classification Algorithm
  #Implements the CBA-C of KEEL

CBA_C <- function(train, test, min_support=0.01, min_confidence=0.5,
                  pruning=TRUE, maxCandidates=80000){
  alg <- RKEEL::R6_CBA_C$new()
  alg$setParameters(train, test, min_support, min_confidence, pruning,
                    maxCandidates)
  return (alg)
}

R6_CBA_C <- R6::R6Class("R6_CBA_C",

  inherit = AssociativeClassificationAlgorithm,

  public = list(

    #Public properties

    #Minimum Support
    min_support = 0.01,

    #Minimum Confidence
    min_confidence = 0.5,

    #Pruning rules?
    pruning = TRUE,

    #Maximum candidate rules (if 0, not limited)
    maxCandidates = 80000,


    #Public functions

    #Initialize function
    setParameters = function(train, test, min_support=0.01, min_confidence=0.5,
                             pruning=TRUE, maxCandidates=80000){

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

      self$min_support <- min_support
      self$min_confidence <- min_confidence
      self$pruning <- pruning
      self$maxCandidates <- maxCandidates

    }

  ),

  private = list(

    #Private properties

    #jar Filename
    jarName = "Clas-CBA.jar",

    #algorithm name
    algorithmName = "CBA-C",

    #String with algorithm name
    algorithmString = "Classification Based on Associations (CBA)",


    #Private functions

    #Get the text with the parameters for the config file
    getParametersText = function(){

      text <- ""
      text <- paste0(text, "Minimum support = ", self$min_support, "\n")
      text <- paste0(text, "Minimum confidence = ", self$min_confidence, "\n")
      if(self$pruning){
        text <- paste0(text, "Wether pruning rules or not? (1:yes, 0:no) = 1", "\n")
      }
      else{
        text <- paste0(text, "Wether pruning rules or not? (1:yes, 0:no) = 0", "\n")
      }

      text <- paste0(text, "Maximum candidate rules (0: no limited) = ", self$maxCandidates, "\n")

      return(text)

    }

  )
)
