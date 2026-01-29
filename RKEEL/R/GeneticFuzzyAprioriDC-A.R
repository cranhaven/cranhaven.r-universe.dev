#Class implementing an Association Rules Algorithm
  #Implements the GeneticFuzzyAprioriDC_A KEEL association rules algorithm
  #Author: Oliver Sanchez

GeneticFuzzyAprioriDC_A <- function(dat, seed=1286082570,NumberofEvaluations=10000,PopulationSize=50,ProbabilityofMutation=0.01,ProbabilityofCrossover=0.8,ParameterdforMMACrossover=0.35,NumberofFuzzyRegionsforNumericAttributes=3,UseMaxOperatorfor1FrequentItemsets="false",MinimumSupport=0.1,MinimumConfidence=0.8){
  alg <- RKEEL::R6_GeneticFuzzyAprioriDC_A$new()
  alg$setParameters(dat,seed,NumberofEvaluations,PopulationSize,ProbabilityofMutation,ProbabilityofCrossover,ParameterdforMMACrossover,NumberofFuzzyRegionsforNumericAttributes,UseMaxOperatorfor1FrequentItemsets,MinimumSupport,MinimumConfidence)
  return (alg)
}

R6_GeneticFuzzyAprioriDC_A <- R6::R6Class("R6_GeneticFuzzyAprioriDC_A",

  inherit = AssociationRulesAlgorithm,

  public = list(

    #Public properties

    #pruned
    #pruned = TRUE,

    #confidence
    #confidence = 0.25,

    #instances per leaf
    #instancesPerLeaf = 2,

    seed=1286082570,
    NumberofEvaluations=10000,
    PopulationSize=50,
    ProbabilityofMutation=0.01,
    ProbabilityofCrossover=0.8,
    ParameterdforMMACrossover=0.35,
    NumberofFuzzyRegionsforNumericAttributes=3,
    UseMaxOperatorfor1FrequentItemsets="false",
    MinimumSupport=0.1,
    MinimumConfidence=0.8,


   #Public functions

    #Initialize function
    setParameters = function(dat, seed=1286082570,NumberofEvaluations=10000,PopulationSize=50,ProbabilityofMutation=0.01,ProbabilityofCrossover=0.8,ParameterdforMMACrossover=0.35,NumberofFuzzyRegionsforNumericAttributes=3,UseMaxOperatorfor1FrequentItemsets="false",MinimumSupport=0.1,MinimumConfidence=0.8){

      super$setParameters(dat)

      self$seed <- seed
      self$NumberofEvaluations <- NumberofEvaluations
      self$PopulationSize <- PopulationSize
      self$ProbabilityofMutation <- ProbabilityofMutation
      self$ProbabilityofCrossover <- ProbabilityofCrossover
      self$ParameterdforMMACrossover <- ParameterdforMMACrossover
      self$NumberofFuzzyRegionsforNumericAttributes <- NumberofFuzzyRegionsforNumericAttributes
      self$UseMaxOperatorfor1FrequentItemsets <- UseMaxOperatorfor1FrequentItemsets
      self$MinimumSupport <- MinimumSupport
      self$MinimumConfidence <- MinimumConfidence

    }

  ),

  private = list(

    #Private properties

    #jar Filename
    jarName = "GeneticFuzzyAprioriDC.jar",

    #algorithm name
    algorithmName = "GeneticFuzzyAprioriDC_A",

    #String with algorithm name
    algorithmString = "GeneticFuzzyAprioriDC_A",

    algorithmOutputNumTxt = 2,


    #Private functions

    #Get the text with the parameters for the config file
    getParametersText = function(){

      text <- ""
      text <- paste0(text, "seed = ", self$seed, "\n")
      text <- paste0(text, "Number of Evaluations = ", self$NumberofEvaluations, "\n")
      text <- paste0(text, "Population Size = ", self$PopulationSize, "\n")
      text <- paste0(text, "Probability of Mutation = ", self$ProbabilityofMutation, "\n")
      text <- paste0(text, "Probability of Crossover = ", self$ProbabilityofCrossover, "\n")
      text <- paste0(text, "Parameter d for MMA Crossover = ", self$ParameterdforMMACrossover, "\n")
      text <- paste0(text, "Number of Fuzzy Regions for Numeric Attributes = ", self$NumberofFuzzyRegionsforNumericAttributes, "\n")
      text <- paste0(text, "Use Max Operator for 1-Frequent Itemsets = ", self$UseMaxOperatorfor1FrequentItemsets, "\n")
      text <- paste0(text, "Minimum Support = ", self$MinimumSupport, "\n")
      text <- paste0(text, "Minimum Confidence = ", self$MinimumConfidence, "\n")

      return(text)

    }
  )
)


