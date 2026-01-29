#Class implementing an Association Rules Algorithm
  #Implements the MOEA_Ghosh_A KEEL association rules algorithm
  #Author: Oliver Sanchez

MOEA_Ghosh_A <- function(dat, seed=1286082570,NumberofObjetives=3,NumberofEvaluations=50000,PopulationSize=100,PointCrossover=2,ProbabilityofCrossover=0.8,ProbabilityofMutation=0.02,Thefactorofamplitudeforeachattributeofthedataset=2.0){
  alg <- RKEEL::R6_MOEA_Ghosh_A$new()
  alg$setParameters(dat,seed,NumberofObjetives,NumberofEvaluations,PopulationSize,PointCrossover,ProbabilityofCrossover,ProbabilityofMutation,Thefactorofamplitudeforeachattributeofthedataset)
  return (alg)
}

R6_MOEA_Ghosh_A <- R6::R6Class("R6_MOEA_Ghosh_A",

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
    PointCrossover=2,
    ProbabilityofCrossover=0.8,
    ProbabilityofMutation=0.02,
    Thefactorofamplitudeforeachattributeofthedataset=2.0,


    #Public functions

    #Initialize function
    setParameters = function(dat, seed=1286082570,NumberofObjetives=3,NumberofEvaluations=50000,PopulationSize=100,PointCrossover=2,ProbabilityofCrossover=0.8,ProbabilityofMutation=0.02,Thefactorofamplitudeforeachattributeofthedataset=2.0){

      super$setParameters(dat)

      self$seed <- seed
      self$NumberofObjetives <- NumberofObjetives
      self$NumberofEvaluations <- NumberofEvaluations
      self$PopulationSize <- PopulationSize
      self$PointCrossover <- PointCrossover
      self$ProbabilityofCrossover <- ProbabilityofCrossover
      self$ProbabilityofMutation <- ProbabilityofMutation
      self$Thefactorofamplitudeforeachattributeofthedataset <- Thefactorofamplitudeforeachattributeofthedataset

    }

  ),

  private = list(

    #Private properties

    #jar Filename
    jarName = "MOEA_Ghosh.jar",

    #algorithm name
    algorithmName = "MOEA_Ghosh_A",

    #String with algorithm name
    algorithmString = "MOEA_Ghosh_A",

    algorithmOutputNumTxt = 1,


    #Private functions

    #Get the text with the parameters for the config file
    getParametersText = function(){

      text <- ""
      text <- paste0(text, "seed = ", self$seed, "\n")
      text <- paste0(text, "Number of Objetives = ", self$NumberofObjetives, "\n")
      text <- paste0(text, "Number of Evaluations = ", self$NumberofEvaluations, "\n")
      text <- paste0(text, "Population Size = ", self$PopulationSize, "\n")
      text <- paste0(text, "Point Crossover = ", self$PointCrossover, "\n")
      text <- paste0(text, "Probability of Crossover = ", self$ProbabilityofCrossover, "\n")
      text <- paste0(text, "Probability of Mutation = ", self$ProbabilityofMutation, "\n")
      text <- paste0(text, "The factor of amplitude for each attribute of the dataset = ", self$Thefactorofamplitudeforeachattributeofthedataset, "\n")

      return(text)

    }
  )
)


