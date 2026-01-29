#Class implementing an Association Rules Algorithm
  #Implements the GAR_A KEEL association rules algorithm
  #Author: Oliver Sanchez

GAR_A <- function(dat,seed=1286082570,NumberofItemsets=100,TotalNumberofEvaluations=50000,PopulationSize=100,ProbabilityofSelection=0.25,ProbabilityofCrossover=0.7,ProbabilityofMutation=0.1,ImportanceofNumberofRecordsAlreadyCovered=0.4,ImportanceofIntervalsAmplitude=0.7,ImportanceofNumberofInvolvedAttributes=0.5,AmplitudeFactor=2.0,MinimumSupport=0.1,MinimumConfidence=0.8){
  alg <- RKEEL::R6_GAR_A$new()
  alg$setParameters(dat,seed,NumberofItemsets,TotalNumberofEvaluations,PopulationSize,ProbabilityofSelection,ProbabilityofCrossover,ProbabilityofMutation,ImportanceofNumberofRecordsAlreadyCovered,ImportanceofIntervalsAmplitude,ImportanceofNumberofInvolvedAttributes,AmplitudeFactor,MinimumSupport,MinimumConfidence)
  return (alg)
}

R6_GAR_A <- R6::R6Class("R6_GAR_A",

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
    NumberofItemsets=100,
    TotalNumberofEvaluations=50000,
    PopulationSize=100,
    ProbabilityofSelection=0.25,
    ProbabilityofCrossover=0.7,
    ProbabilityofMutation=0.1,
    ImportanceofNumberofRecordsAlreadyCovered=0.4,
    ImportanceofIntervalsAmplitude=0.7,
    ImportanceofNumberofInvolvedAttributes=0.5,
    AmplitudeFactor=2.0,
    MinimumSupport=0.1,
    MinimumConfidence=0.8,


    #Public functions

    #Initialize function
    setParameters = function(dat,seed=1286082570,NumberofItemsets=100,TotalNumberofEvaluations=50000,PopulationSize=100,ProbabilityofSelection=0.25,ProbabilityofCrossover=0.7,ProbabilityofMutation=0.1,ImportanceofNumberofRecordsAlreadyCovered=0.4,ImportanceofIntervalsAmplitude=0.7,ImportanceofNumberofInvolvedAttributes=0.5,AmplitudeFactor=2.0,MinimumSupport=0.1,MinimumConfidence=0.8){

      super$setParameters(dat)

      self$seed <- seed
      self$NumberofItemsets <- NumberofItemsets
      self$TotalNumberofEvaluations <- TotalNumberofEvaluations
      self$PopulationSize <- PopulationSize
      self$ProbabilityofSelection <- ProbabilityofSelection
      self$ProbabilityofCrossover <- ProbabilityofCrossover
      self$ProbabilityofMutation <- ProbabilityofMutation
      self$ImportanceofNumberofRecordsAlreadyCovered <- ImportanceofNumberofRecordsAlreadyCovered
      self$ImportanceofIntervalsAmplitude <- ImportanceofIntervalsAmplitude
      self$ImportanceofNumberofInvolvedAttributes <- ImportanceofNumberofInvolvedAttributes
      self$AmplitudeFactor <- AmplitudeFactor
      self$MinimumSupport <- MinimumSupport
      self$MinimumConfidence <- MinimumConfidence

    }

  ),

  private = list(

    #Private properties

    #jar Filename
    jarName = "GAR.jar",

    #algorithm name
    algorithmName = "GAR_A",

    #String with algorithm name
    algorithmString = "GAR_A",

    algorithmOutputNumTxt = 1,


    #Private functions

    #Get the text with the parameters for the config file
    getParametersText = function(){

      text <- ""
      text <- paste0(text, "seed = ", self$seed, "\n")
      text <- paste0(text, "Number of Itemsets = ", self$NumberofItemsets, "\n")
      text <- paste0(text, "Total Number of Evaluations = ", self$TotalNumberofEvaluations, "\n")
      text <- paste0(text, "Population Size = ", self$PopulationSize, "\n")
      text <- paste0(text, "Probability of Selection = ", self$ProbabilityofSelection, "\n")
      text <- paste0(text, "Probability of Crossover = ", self$ProbabilityofCrossover, "\n")
      text <- paste0(text, "Probability of Mutation = ", self$ProbabilityofMutation, "\n")
      text <- paste0(text, "Importance of Number of Records Already Covered = ", self$ImportanceofNumberofRecordsAlreadyCovered, "\n")
      text <- paste0(text, "Importance of Intervals Amplitude = ", self$ImportanceofIntervalsAmplitude, "\n")
      text <- paste0(text, "Importance of Number of Involved Attributes = ", self$ImportanceofNumberofInvolvedAttributes, "\n")
      text <- paste0(text, "Amplitude Factor = ", self$AmplitudeFactor, "\n")
      text <- paste0(text, "Minimum Support = ", self$MinimumSupport, "\n")
      text <- paste0(text, "Minimum Confidence = ", self$MinimumConfidence, "\n")
      

      return(text)

    }
  )
)


