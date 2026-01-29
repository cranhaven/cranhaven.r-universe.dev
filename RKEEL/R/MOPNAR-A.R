#Class implementing an Association Rules Algorithm
  #Implements the MOPNAR_A KEEL association rules algorithm
  #Author: Oliver Sanchez

MOPNAR_A <- function(dat, seed = 1286082570,objetives = 3,evaluations = 50000,parameter = 13,weightNeighborhood = 10,wrobabilitySolutionsNeighborhood = 0.9,maxSolutions = 2,probabilityMutation = 0.1,amplitude = 2.0,threshold = 5.0){
  alg <- RKEEL::R6_MOPNAR_A$new()
  alg$setParameters(dat,seed,objetives,evaluations,parameter,weightNeighborhood,wrobabilitySolutionsNeighborhood,maxSolutions,probabilityMutation,amplitude,threshold)
  return (alg)
}

R6_MOPNAR_A <- R6::R6Class("R6_MOPNAR_A",

  inherit = AssociationRulesAlgorithm,

  public = list(

    #Public properties

    #pruned
    #pruned = TRUE,

    #confidence
    #confidence = 0.25,

    #instances per leaf
    #instancesPerLeaf = 2,

    seed = 1286082570,
    objetives = 3,
    evaluations = 50000,
    parameter = 13,
    weightNeighborhood = 10,
    wrobabilitySolutionsNeighborhood = 0.9,
    maxSolutions = 2,
    probabilityMutation = 0.1,
    amplitude = 2.0,
    threshold = 5.0,


    #Public functions

    #Initialize function
    setParameters = function(dat, seed = 1286082570,objetives = 3,evaluations = 50000,parameter = 13,weightNeighborhood = 10,wrobabilitySolutionsNeighborhood = 0.9,maxSolutions = 2,probabilityMutation = 0.1,amplitude = 2.0,threshold = 5.0){

      super$setParameters(dat)

      self$seed <- seed
      self$objetives <- objetives
      self$evaluations <- evaluations
      self$parameter <- parameter
      self$weightNeighborhood <- weightNeighborhood
      self$wrobabilitySolutionsNeighborhood <- wrobabilitySolutionsNeighborhood
      self$maxSolutions <- maxSolutions
      self$probabilityMutation <- probabilityMutation
      self$amplitude <- amplitude
      self$threshold <- threshold

    }

  ),

  private = list(

    #Private properties

    #jar Filename
    jarName = "MOPNAR.jar",

    #algorithm name
    algorithmName = "MOPNAR_A",

    #String with algorithm name
    algorithmString = "MOPNAR_A",

    algorithmOutputNumTxt = 1,


    #Private functions

    #Get the text with the parameters for the config file
    getParametersText = function(){

      text <- ""
      text <- paste0(text, "seed = ", self$seed, "\n")
      text <- paste0(text, "Number of Objetives = ", self$objetives, "\n")
      text <- paste0(text, "Number of Evaluations = ", self$evaluations, "\n")
      text <- paste0(text, "H control parameter = ", self$parameter, "\n")
      text <- paste0(text, "Weight vectors in neighborhood = ", self$weightNeighborhood, "\n")
      text <- paste0(text, "Probability parent solutions are selected from the neighborhood = ", self$wrobabilitySolutionsNeighborhood, "\n")
      text <- paste0(text, "Max solutions replaced by each child solution = ", self$maxSolutions, "\n")
      text <- paste0(text, "Probability of Mutation = ", self$probabilityMutation, "\n")
      text <- paste0(text, "Factor amplitude = ", self$amplitude, "\n")
      text <- paste0(text, "Difference threshold = ", self$threshold, "\n")

      return(text)

    }
  )
)


