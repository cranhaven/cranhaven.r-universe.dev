#Class implementing an Association Rules Algorithm
  #Implements the QAR_CIP_NSGAII_A KEEL association rules algorithm
  #Author: Oliver Sanchez

QAR_CIP_NSGAII_A <- function(dat, seed=1286082570,NumberofObjetives=3,NumberofEvaluations=50000,PopulationSize=100,ProbabilityofMutation=0.1,Thefactorofamplitudeforeachattributeofthedataset=2.0,Differencethreshold=5.0){
  alg <- RKEEL::R6_QAR_CIP_NSGAII_A$new()
  alg$setParameters(dat,seed,NumberofObjetives,NumberofEvaluations,PopulationSize,ProbabilityofMutation,Thefactorofamplitudeforeachattributeofthedataset,Differencethreshold)
  return (alg)
}

R6_QAR_CIP_NSGAII_A <- R6::R6Class("R6_QAR_CIP_NSGAII_A",

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
    NumberofObjetives=3,
    NumberofEvaluations=50000,
    PopulationSize=100,
    ProbabilityofMutation=0.1,
    Thefactorofamplitudeforeachattributeofthedataset=2.0,
    Differencethreshold=5.0,


    #Public functions

    #Initialize function
    setParameters = function(dat, seed=1286082570,NumberofObjetives=3,NumberofEvaluations=50000,PopulationSize=100,ProbabilityofMutation=0.1,Thefactorofamplitudeforeachattributeofthedataset=2.0,Differencethreshold=5.0){

      super$setParameters(dat)

      self$seed <- seed
      self$NumberofObjetives <- NumberofObjetives
      self$NumberofEvaluations <- NumberofEvaluations
      self$PopulationSize <- PopulationSize
      self$ProbabilityofMutation <- ProbabilityofMutation
      self$Thefactorofamplitudeforeachattributeofthedataset <- Thefactorofamplitudeforeachattributeofthedataset
      self$Differencethreshold <- Differencethreshold
      

    }

  ),

  private = list(

    #Private properties

    #jar Filename
    jarName = "QAR_CIP_NSGAII.jar",

    #algorithm name
    algorithmName = "QAR_CIP_NSGAII_A",

    #String with algorithm name
    algorithmString = "QAR_CIP_NSGAII_A",

    algorithmOutputNumTxt = 1,


    #Private functions

    #Get the text with the parameters for the config file
    getParametersText = function(){

      text <- ""
      text <- paste0(text, "seed = ", self$seed, "\n")
      text <- paste0(text, "Number of Objetives = ", self$NumberofObjetives, "\n")
      text <- paste0(text, "Number of Evaluations = ", self$NumberofEvaluations, "\n")
      text <- paste0(text, "Population Size = ", self$PopulationSize, "\n")
      text <- paste0(text, "Probability of Mutation = ", self$ProbabilityofMutation, "\n")
      text <- paste0(text, "The factor of amplitude for each attribute of the dataset = ", self$Thefactorofamplitudeforeachattributeofthedataset, "\n")
      text <- paste0(text, "Difference threshold = ", self$Differencethreshold, "\n")

      return(text)

    }
  )
)


