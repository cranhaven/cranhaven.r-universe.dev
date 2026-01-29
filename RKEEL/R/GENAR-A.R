#Class implementing an Association Rules Algorithm
  #Implements the GENAR_A KEEL association rules algorithm
  #Author: Oliver Sanchez

GENAR_A <- function(dat,seed=1286082570,NumberofAssociationRules=30,TotalNumberofEvaluations=50000,PopulationSize=100,ProbabilityofSelection=0.25,ProbabilityofMutation=0.1,PenalizationFactor=0.7,AmplitudeFactor=2.0){
  alg <- RKEEL::R6_GENAR_A$new()
  alg$setParameters(dat,seed,NumberofAssociationRules,TotalNumberofEvaluations,PopulationSize,ProbabilityofSelection,ProbabilityofMutation,PenalizationFactor,AmplitudeFactor)
  return (alg)
}

R6_GENAR_A <- R6::R6Class("R6_GENAR_A",

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
    NumberofAssociationRules=30,
    TotalNumberofEvaluations=50000,
    PopulationSize=100,
    ProbabilityofSelection=0.25,
    ProbabilityofMutation=0.1,
    PenalizationFactor=0.7,
    AmplitudeFactor=2.0,


    #Public functions

    #Initialize function
    setParameters = function(dat,seed=1286082570,NumberofAssociationRules=30,TotalNumberofEvaluations=50000,PopulationSize=100,ProbabilityofSelection=0.25,ProbabilityofMutation=0.1,PenalizationFactor=0.7,AmplitudeFactor=2.0){

      super$setParameters(dat)

      self$seed <- seed
      self$NumberofAssociationRules <- NumberofAssociationRules
      self$TotalNumberofEvaluations <- TotalNumberofEvaluations
      self$PopulationSize <- PopulationSize
      self$ProbabilityofSelection <- ProbabilityofSelection
      self$ProbabilityofMutation <- ProbabilityofMutation
      self$PenalizationFactor <- PenalizationFactor
      self$AmplitudeFactor <- AmplitudeFactor
      

    }

  ),

  private = list(

    #Private properties

    #jar Filename
    jarName = "GENAR.jar",

    #algorithm name
    algorithmName = "GENAR_A",

    #String with algorithm name
    algorithmString = "GENAR_A",

    algorithmOutputNumTxt = 1,


    #Private functions

    #Get the text with the parameters for the config file
    getParametersText = function(){

      text <- ""
      text <- paste0(text, "seed = ", self$seed, "\n")
      text <- paste0(text, "Number of Association Rules = ", self$NumberofAssociationRules, "\n")
      text <- paste0(text, "Total Number of Evaluations = ", self$TotalNumberofEvaluations, "\n")
      text <- paste0(text, "Population Size = ", self$PopulationSize, "\n")
      text <- paste0(text, "Probability of Selection = ", self$ProbabilityofSelection, "\n")
      text <- paste0(text, "Probability of Mutation = ", self$ProbabilityofMutation, "\n")
      text <- paste0(text, "Penalization Factor = ", self$PenalizationFactor, "\n")
      text <- paste0(text, "Amplitude Factor = ", self$AmplitudeFactor, "\n")
      

      return(text)

    }
  )
)


