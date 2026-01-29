#Class implementing an Association Rules Algorithm
  #Implements the Eclat_A KEEL association rules algorithm
  #Author: Oliver Sanchez

Eclat_A <- function(dat, NumberofPartitionsforNumericAttributes = 4,MinimumSupport = 0.1,MinimumConfidence = 0.8){
  alg <- RKEEL::R6_Eclat_A$new()
  alg$setParameters(dat,NumberofPartitionsforNumericAttributes,MinimumSupport,MinimumConfidence)
  return (alg)
}

R6_Eclat_A <- R6::R6Class("R6_Eclat_A",

  inherit = AssociationRulesAlgorithm,

  public = list(

    #Public properties

    #pruned
    #pruned = TRUE,

    #confidence
    #confidence = 0.25,

    #instances per leaf
    #instancesPerLeaf = 2,

    NumberofPartitionsforNumericAttributes = 4,
    MinimumSupport = 0.1,
    MinimumConfidence = 0.8,


    #Public functions

    #Initialize function
    setParameters = function(dat, NumberofPartitionsforNumericAttributes = 4,MinimumSupport = 0.1,MinimumConfidence = 0.8){

      super$setParameters(dat)

      self$NumberofPartitionsforNumericAttributes <- NumberofPartitionsforNumericAttributes
      self$MinimumSupport <- MinimumSupport
      self$MinimumConfidence <- MinimumConfidence

    }

  ),

  private = list(

    #Private properties

    #jar Filename
    jarName = "Eclat.jar",

    #algorithm name
    algorithmName = "Eclat_A",

    #String with algorithm name
    algorithmString = "Eclat_A",

    algorithmOutputNumTxt = 1,


    #Private functions

    #Get the text with the parameters for the config file
    getParametersText = function(){

      text <- ""
      text <- paste0(text, "Number of Partitions for Numeric Attributes = ", self$NumberofPartitionsforNumericAttributes, "\n")
      text <- paste0(text, "Minimum Support = ", self$MinimumSupport, "\n")
      text <- paste0(text, "Minimum Confidence = ", self$MinimumConfidence, "\n")

      return(text)

    }
  )
)


