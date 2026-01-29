#Class implementing an Association Rules Algorithm
  #Implements the Alatasetal_A KEEL association rules algorithm
  #Author: Oliver Sanchez

Alatasetal_A <- function(dat, seed=1286082570,NumberofEvaluations=50000,InitialRandomChromosomes=12,rDividingPoints=3,TournamentSize=10,ProbabilityofCrossover=0.7,MinimumProbabilityofMutation=0.05,MaximumProbabilityofMutation=0.9,ImportanceofRulesSupport=5,ImportanceofRulesConfidence=20,ImportanceofNumberofInvolvedAttributes=0.05,ImportanceofIntervalsAmplitude=0.02,ImportanceofNumberofRecordsAlreadyCovered=0.01,AmplitudeFactor=2.0){
  alg <- RKEEL::R6_Alatasetal_A$new()
  alg$setParameters(dat,seed,NumberofEvaluations,InitialRandomChromosomes,rDividingPoints,TournamentSize,ProbabilityofCrossover,MinimumProbabilityofMutation,MaximumProbabilityofMutation,ImportanceofRulesSupport,ImportanceofRulesConfidence,ImportanceofNumberofInvolvedAttributes,ImportanceofIntervalsAmplitude,ImportanceofNumberofRecordsAlreadyCovered,AmplitudeFactor)
  return (alg)
}

R6_Alatasetal_A <- R6::R6Class("R6_Alatasetal_A",

  inherit = AssociationRulesAlgorithm,

  public = list(

    #Public properties

    seed = 1286082570,
    NumberofEvaluations=50000,
    InitialRandomChromosomes=12,
    rDividingPoints=3,
    TournamentSize=10,
    ProbabilityofCrossover=0.7,
    MinimumProbabilityofMutation=0.05,
    MaximumProbabilityofMutation=0.9,
    ImportanceofRulesSupport=5,
    ImportanceofRulesConfidence=20,
    ImportanceofNumberofInvolvedAttributes=0.05,
    ImportanceofIntervalsAmplitude=0.02,
    ImportanceofNumberofRecordsAlreadyCovered=0.01,
    AmplitudeFactor=2.0,


    #Public functions

    #Initialize function
    setParameters = function(dat, seed=1286082570,NumberofEvaluations=50000,InitialRandomChromosomes=12,rDividingPoints=3,TournamentSize=10,ProbabilityofCrossover=0.7,MinimumProbabilityofMutation=0.05,MaximumProbabilityofMutation=0.9,ImportanceofRulesSupport=5,ImportanceofRulesConfidence=20,ImportanceofNumberofInvolvedAttributes=0.05,ImportanceofIntervalsAmplitude=0.02,ImportanceofNumberofRecordsAlreadyCovered=0.01,AmplitudeFactor=2.0){

      super$setParameters(dat)

      self$seed <- seed
      self$NumberofEvaluations <- NumberofEvaluations
      self$InitialRandomChromosomes <- InitialRandomChromosomes
      self$rDividingPoints <- rDividingPoints
      self$TournamentSize <- TournamentSize
      self$ProbabilityofCrossover <- ProbabilityofCrossover
      self$MinimumProbabilityofMutation <- MinimumProbabilityofMutation
      self$MaximumProbabilityofMutation <- MaximumProbabilityofMutation
      self$ImportanceofRulesSupport <- ImportanceofRulesSupport
      self$ImportanceofRulesConfidence <- ImportanceofRulesConfidence
      self$ImportanceofNumberofInvolvedAttributes <- ImportanceofNumberofInvolvedAttributes
      self$ImportanceofIntervalsAmplitude <- ImportanceofIntervalsAmplitude
      self$ImportanceofNumberofRecordsAlreadyCovered <- ImportanceofNumberofRecordsAlreadyCovered
      self$AmplitudeFactor <- AmplitudeFactor

    }

  ),

  private = list(

    #Private properties

    #jar Filename
    jarName = "Alatasetal.jar",

    #algorithm name
    algorithmName = "Alatasetal_A",

    #String with algorithm name
    algorithmString = "Alatasetal_A",

    algorithmOutputNumTxt = 1,


    #Private functions

    #Get the text with the parameters for the config file
    getParametersText = function(){

      text <- ""
      text <- paste0(text, "seed = ", self$seed, "\n")
      text <- paste0(text, "Number of Evaluations = ", self$NumberofEvaluations, "\n")
      text <- paste0(text, "Initial Random Chromosomes = ", self$InitialRandomChromosomes, "\n")
      text <- paste0(text, "r-Dividing Points = ", self$rDividingPoints, "\n")
      text <- paste0(text, "Tournament Size = ", self$TournamentSize, "\n")
      text <- paste0(text, "Probability of Crossover = ", self$ProbabilityofCrossover, "\n")
      text <- paste0(text, "Minimum Probability of Mutation = ", self$MinimumProbabilityofMutation, "\n")
      text <- paste0(text, "Maximum Probability of Mutation = ", self$MaximumProbabilityofMutation, "\n")
      text <- paste0(text, "Importance of Rules Support = ", self$ImportanceofRulesSupport, "\n")
      text <- paste0(text, "Importance of Rules Confidence = ", self$ImportanceofRulesConfidence, "\n")
      text <- paste0(text, "Importance of Number of Involved Attributes = ", self$ImportanceofNumberofInvolvedAttributes, "\n")
      text <- paste0(text, "Importance of Intervals Amplitude = ", self$ImportanceofIntervalsAmplitude, "\n")
      text <- paste0(text, "Importance of Number of Records Already Covered = ", self$ImportanceofNumberofRecordsAlreadyCovered, "\n")
      text <- paste0(text, "Amplitude Factor = ", self$AmplitudeFactor, "\n")
      

      return(text)

    }
  )
)


