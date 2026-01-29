#Class implementing an Association Rules Algorithm
  #Implements the NICGAR_A KEEL association rules algorithm
  #Author: Oliver Sanchez

NICGAR_A <- function(dat,seed=1286082570,NumberofEvaluations=100000,PopulationSize=100,ProbabilityofMutation=0.1,Thefactorofamplitudeforeachattributeofthedataset=3.0,NichingThreshold=0.5,QualityThreshold=0.85,PercentUpdate=5.0){
  alg <- RKEEL::R6_NICGAR_A$new()
  alg$setParameters(dat,seed,NumberofEvaluations,PopulationSize,ProbabilityofMutation,Thefactorofamplitudeforeachattributeofthedataset,NichingThreshold,QualityThreshold,PercentUpdate)
  return (alg)
}

R6_NICGAR_A <- R6::R6Class("R6_NICGAR_A",

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
    NumberofEvaluations=100000,
    PopulationSize=100,
    ProbabilityofMutation=0.1,
    Thefactorofamplitudeforeachattributeofthedataset=3.0,
    NichingThreshold=0.5,
    QualityThreshold=0.85,
    PercentUpdate=5.0,


    #Public functions

    #Initialize function
    setParameters = function(dat,seed=1286082570,NumberofEvaluations=100000,PopulationSize=100,ProbabilityofMutation=0.1,Thefactorofamplitudeforeachattributeofthedataset=3.0,NichingThreshold=0.5,QualityThreshold=0.85,PercentUpdate=5.0){

      super$setParameters(dat)

      self$seed <- seed
      self$NumberofEvaluations <- NumberofEvaluations
      self$PopulationSize <- PopulationSize
      self$ProbabilityofMutation <- ProbabilityofMutation
      self$Thefactorofamplitudeforeachattributeofthedataset <- Thefactorofamplitudeforeachattributeofthedataset
      self$NichingThreshold <- NichingThreshold
      self$QualityThreshold <- QualityThreshold
      self$PercentUpdate <- PercentUpdate

    }

  ),

  private = list(

    #Private properties

    #jar Filename
    jarName = "NICGAR.jar",

    #algorithm name
    algorithmName = "NICGAR_A",

    #String with algorithm name
    algorithmString = "NICGAR_A",

    algorithmOutputNumTxt = 1,


    #Private functions

    #Get the text with the parameters for the config file
    getParametersText = function(){

      text <- ""
      text <- paste0(text, "seed = ", self$seed, "\n")
      text <- paste0(text, "Number of Evaluations = ", self$NumberofEvaluations, "\n")
      text <- paste0(text, "Population Size = ", self$PopulationSize, "\n")
      text <- paste0(text, "Probability of Mutation = ", self$ProbabilityofMutation, "\n")
      text <- paste0(text, "The factor of amplitude for each attribute of the dataset = ", self$Thefactorofamplitudeforeachattributeofthedataset, "\n")
      text <- paste0(text, "Niching Threshold = ", self$NichingThreshold, "\n")
      text <- paste0(text, "Quality Threshold = ", self$QualityThreshold, "\n")
      text <- paste0(text, "Percent Update = ", self$PercentUpdate, "\n")
      

      return(text)

    }
  )
)


