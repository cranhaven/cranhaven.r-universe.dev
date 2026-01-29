#Class implementing an Association Rules Algorithm
  #Implements the Alcalaetal_A KEEL association rules algorithm
  #Author: Oliver Sanchez

Alcalaetal_A <- function(dat, seed=1286082570,NumberofEvaluations=10000,PopulationSize=50,NumberofBitsperGene=30,DecreasingFactorofLthresholdNOTUSED=0.1,FactorforParentCentricBLXCrossover=1.0,NumberofFuzzyRegionsforNumericAttributes=3,UseMaxOperatorfor1FrequentItemsets="false",MinimumSupport=0.1,MinimumConfidence=0.8){
  alg <- RKEEL::R6_Alcalaetal_A$new()
  alg$setParameters(dat,seed,NumberofEvaluations,PopulationSize,NumberofBitsperGene,DecreasingFactorofLthresholdNOTUSED,FactorforParentCentricBLXCrossover,NumberofFuzzyRegionsforNumericAttributes,UseMaxOperatorfor1FrequentItemsets,MinimumSupport,MinimumConfidence)
  return (alg)
}

R6_Alcalaetal_A <- R6::R6Class("R6_Alcalaetal_A",

  inherit = AssociationRulesAlgorithm,

  public = list(

    #Public properties

    seed=1286082570,
    NumberofEvaluations=10000,
    PopulationSize=50,
    NumberofBitsperGene=30,
    DecreasingFactorofLthresholdNOTUSED=0.1,
    FactorforParentCentricBLXCrossover=1.0,
    NumberofFuzzyRegionsforNumericAttributes=3,
    UseMaxOperatorfor1FrequentItemsets="false",
    MinimumSupport=0.1,
    MinimumConfidence=0.8,


    #Public functions

    #Initialize function
    setParameters = function(dat,seed=1286082570,NumberofEvaluations=10000,PopulationSize=50,NumberofBitsperGene=30,DecreasingFactorofLthresholdNOTUSED=0.1,FactorforParentCentricBLXCrossover=1.0,NumberofFuzzyRegionsforNumericAttributes=3,UseMaxOperatorfor1FrequentItemsets="false",MinimumSupport=0.1,MinimumConfidence=0.8){

      super$setParameters(dat)

      self$seed <- seed
      self$NumberofEvaluations <- NumberofEvaluations
      self$PopulationSize <- PopulationSize
      self$NumberofBitsperGene <- NumberofBitsperGene
      self$DecreasingFactorofLthresholdNOTUSED <- DecreasingFactorofLthresholdNOTUSED
      self$FactorforParentCentricBLXCrossover <- FactorforParentCentricBLXCrossover
      self$NumberofFuzzyRegionsforNumericAttributes <- NumberofFuzzyRegionsforNumericAttributes
      self$UseMaxOperatorfor1FrequentItemsets <- UseMaxOperatorfor1FrequentItemsets
      self$MinimumSupport <- MinimumSupport
      self$MinimumConfidence <- MinimumConfidence

    }

  ),

  private = list(

    #Private properties

    #jar Filename
    jarName = "Alcalaetal.jar",

    #algorithm name
    algorithmName = "Alcalaetal_A",

    #String with algorithm name
    algorithmString = "Alcalaetal_A",

    algorithmOutputNumTxt = 3,


    #Private functions

    #Get the text with the parameters for the config file
    getParametersText = function(){

      text <- ""
      text <- paste0(text, "seed = ", self$seed, "\n")
      text <- paste0(text, "Number of Evaluations = ", self$NumberofEvaluations, "\n")
      text <- paste0(text, "Population Size = ", self$PopulationSize, "\n")
      text <- paste0(text, "Number of Bits per Gene = ", self$NumberofBitsperGene, "\n")
      text <- paste0(text, "Decreasing Factor of L threshold (NOT USED) = ", self$DecreasingFactorofLthresholdNOTUSED, "\n")
      text <- paste0(text, "Factor for Parent Centric BLX Crossover = ", self$FactorforParentCentricBLXCrossover, "\n")
      text <- paste0(text, "Number of Fuzzy Regions for Numeric Attributes = ", self$NumberofFuzzyRegionsforNumericAttributes, "\n")
      text <- paste0(text, "Use Max Operator for 1-Frequent Itemsets = ", self$UseMaxOperatorfor1FrequentItemsets, "\n")
      text <- paste0(text, "Minimum Support = ", self$MinimumSupport, "\n")
      text <- paste0(text, "Minimum Confidence = ", self$MinimumConfidence, "\n")
      

      return(text)

    }
  )
)


