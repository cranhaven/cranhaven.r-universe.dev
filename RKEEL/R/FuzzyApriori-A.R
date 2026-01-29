#Class implementing an Association Rules Algorithm
  #Implements the FuzzyApriori_A KEEL association rules algorithm
  #Author: Oliver Sanchez

FuzzyApriori_A <- function(dat, NumberofPartitionsforNumericAttributes=4,UseMaxOperatorfor1FrequentItemsets="false",MinimumSupport=0.1,MinimumConfidence=0.8){
  alg <- RKEEL::R6_FuzzyApriori_A$new()
  alg$setParameters(dat,NumberofPartitionsforNumericAttributes,UseMaxOperatorfor1FrequentItemsets,MinimumSupport,MinimumConfidence)
  return (alg)
}

R6_FuzzyApriori_A <- R6::R6Class("R6_FuzzyApriori_A",

  inherit = AssociationRulesAlgorithm,

  public = list(

    #Public properties

    NumberofPartitionsforNumericAttributes=4,
    UseMaxOperatorfor1FrequentItemsets="false",
    MinimumSupport=0.1,
    MinimumConfidence=0.8,


    #Public functions

    #Initialize function
    setParameters = function(dat, NumberofPartitionsforNumericAttributes=4,UseMaxOperatorfor1FrequentItemsets="false",MinimumSupport=0.1,MinimumConfidence=0.8){

      super$setParameters(dat)

      self$NumberofPartitionsforNumericAttributes <- NumberofPartitionsforNumericAttributes
      self$UseMaxOperatorfor1FrequentItemsets <- UseMaxOperatorfor1FrequentItemsets
      self$MinimumSupport <- MinimumSupport
      self$MinimumConfidence <- MinimumConfidence

    }

  ),

  private = list(

    #Private properties

    #jar Filename
    jarName = "FuzzyApriori.jar",

    #algorithm name
    algorithmName = "FuzzyApriori_A",

    #String with algorithm name
    algorithmString = "FuzzyApriori_A",

    algorithmOutputNumTxt = 1,


    #Private functions

    #Get the text with the parameters for the config file
    getParametersText = function(){

      text <- ""
      text <- paste0(text, "Number of Partitions for Numeric Attributes = ", self$NumberofPartitionsforNumericAttributes, "\n")
      text <- paste0(text, "Use Max Operator for 1-Frequent Itemsets = ", self$NumberofPartitionsforNumericAttributes, "\n")
      text <- paste0(text, "Minimum Support = ", self$MinimumSupport, "\n")
      text <- paste0(text, "Minimum Confidence = ", self$MinimumConfidence, "\n")


      return(text)

    }
  )
)


