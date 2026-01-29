#Class implementing an Association Rules Algorithm
  #Implements the EARMGA_A KEEL association rules algorithm
  #Author: Oliver Sanchez

EARMGA_A <- function(dat, seed=1286082570,FixedLengthofAssociationRules=2,PopulationSize=100,TotalNumberofEvaluations=50000,DifferenceBoundaryNOTUSED=0.01,ProbabilityofSelection=0.75,ProbabilityofCrossover=0.7,ProbabilityofMutation=0.1,NumberofPartitionsforNumericAttributes=4){
  alg <- RKEEL::R6_EARMGA_A$new()
  alg$setParameters(dat,seed,FixedLengthofAssociationRules,PopulationSize,TotalNumberofEvaluations,DifferenceBoundaryNOTUSED,ProbabilityofSelection,ProbabilityofCrossover,ProbabilityofMutation,NumberofPartitionsforNumericAttributes)
  return (alg)
}

R6_EARMGA_A <- R6::R6Class("R6_EARMGA_A",

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
    FixedLengthofAssociationRules=2,
    PopulationSize=100,
    TotalNumberofEvaluations=50000,
    DifferenceBoundaryNOTUSED=0.01,
    ProbabilityofSelection=0.75,
    ProbabilityofCrossover=0.7,
    ProbabilityofMutation=0.1,
    NumberofPartitionsforNumericAttributes=4,


    #Public functions

    #Initialize function
    setParameters = function(dat, seed=1286082570,FixedLengthofAssociationRules=2,PopulationSize=100,TotalNumberofEvaluations=50000,DifferenceBoundaryNOTUSED=0.01,ProbabilityofSelection=0.75,ProbabilityofCrossover=0.7,ProbabilityofMutation=0.1,NumberofPartitionsforNumericAttributes=4){

      super$setParameters(dat)

      self$seed <- seed
      self$FixedLengthofAssociationRules <- FixedLengthofAssociationRules
      self$PopulationSize <- PopulationSize
      self$TotalNumberofEvaluations <- TotalNumberofEvaluations
      self$DifferenceBoundaryNOTUSED <- DifferenceBoundaryNOTUSED
      self$ProbabilityofSelection <- ProbabilityofSelection
      self$ProbabilityofCrossover <- ProbabilityofCrossover
      self$ProbabilityofMutation <- ProbabilityofMutation
      self$NumberofPartitionsforNumericAttributes <- NumberofPartitionsforNumericAttributes

    }

  ),

  private = list(

    #Private properties

    #jar Filename
    jarName = "EARMGA.jar",

    #algorithm name
    algorithmName = "EARMGA_A",

    #String with algorithm name
    algorithmString = "EARMGA_A",

    algorithmOutputNumTxt = 1,


    #Private functions

    #Get the text with the parameters for the config file
    getParametersText = function(){

      text <- ""
      text <- paste0(text, "seed = ", self$seed, "\n")
      text <- paste0(text, "Fixed Length of Association Rules = ", self$FixedLengthofAssociationRules, "\n")
      text <- paste0(text, "Population Size = ", self$PopulationSize, "\n")
      text <- paste0(text, "Total Number of Evaluations = ", self$TotalNumberofEvaluations, "\n")
      text <- paste0(text, "Difference Boundary (NOT USED) = ", self$DifferenceBoundaryNOTUSED, "\n")
      text <- paste0(text, "Probability of Selection = ", self$ProbabilityofSelection, "\n")
      text <- paste0(text, "Probability of Crossover = ", self$ProbabilityofCrossover, "\n")
      text <- paste0(text, "Probability of Mutation = ", self$ProbabilityofMutation, "\n")
      text <- paste0(text, "Number of Partitions for Numeric Attributes = ", self$NumberofPartitionsforNumericAttributes, "\n")
      

      return(text)

    }
  )
)


