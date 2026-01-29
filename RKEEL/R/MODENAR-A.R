#Class implementing an Association Rules Algorithm
  #Implements the MODENAR_A KEEL association rules algorithm
  #Author: Oliver Sanchez

MODENAR_A <- function(dat, seed=1286082570,PopulationSize=100,NumberofEvaluations=50000,CrossoverrateCR=0.3,Thresholdforthenumberofnondominatedsolutions=60,Thefactorofamplitudeforeachattributeofthedataset=2,WeightforSupport=0.8,WeightforConfidence=0.2,WeightforComprehensibility=0.1,WeightforAmplitudeoftheIntervals=0.4){
  alg <- RKEEL::R6_MODENAR_A$new()
  alg$setParameters(dat,seed,PopulationSize,NumberofEvaluations,CrossoverrateCR,Thresholdforthenumberofnondominatedsolutions,Thefactorofamplitudeforeachattributeofthedataset,WeightforSupport,WeightforConfidence,WeightforComprehensibility,WeightforAmplitudeoftheIntervals)
  return (alg)
}

R6_MODENAR_A <- R6::R6Class("R6_MODENAR_A",

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
    PopulationSize=100,
    NumberofEvaluations=50000,
    CrossoverrateCR=0.3,
    Thresholdforthenumberofnondominatedsolutions=60,
    Thefactorofamplitudeforeachattributeofthedataset=2,
    WeightforSupport=0.8,
    WeightforConfidence=0.2,
    WeightforComprehensibility=0.1,
    WeightforAmplitudeoftheIntervals=0.4,


    #Public functions

    #Initialize function
    setParameters = function(dat, seed=1286082570,PopulationSize=100,NumberofEvaluations=50000,CrossoverrateCR=0.3,Thresholdforthenumberofnondominatedsolutions=60,Thefactorofamplitudeforeachattributeofthedataset=2,WeightforSupport=0.8,WeightforConfidence=0.2,WeightforComprehensibility=0.1,WeightforAmplitudeoftheIntervals=0.4){

      super$setParameters(dat)

      self$seed <- seed
      self$PopulationSize <- PopulationSize
      self$NumberofEvaluations <- NumberofEvaluations
      self$CrossoverrateCR <- CrossoverrateCR
      self$Thresholdforthenumberofnondominatedsolutions <- Thresholdforthenumberofnondominatedsolutions
      self$Thefactorofamplitudeforeachattributeofthedataset <- Thefactorofamplitudeforeachattributeofthedataset
      self$WeightforSupport <- WeightforSupport
      self$WeightforConfidence <- WeightforConfidence
      self$WeightforComprehensibility <- WeightforComprehensibility
      self$WeightforAmplitudeoftheIntervals <- WeightforAmplitudeoftheIntervals

    }

  ),

  private = list(

    #Private properties

    #jar Filename
    jarName = "MODENAR.jar",

    #algorithm name
    algorithmName = "MODENAR_A",

    #String with algorithm name
    algorithmString = "MODENAR_A",

    algorithmOutputNumTxt = 1,


    #Private functions

    #Get the text with the parameters for the config file
    getParametersText = function(){

      text <- ""
      text <- paste0(text, "seed = ", self$seed, "\n")
      text <- paste0(text, "Population Size = ", self$PopulationSize, "\n")
      text <- paste0(text, "Number of Evaluations = ", self$NumberofEvaluations, "\n")
      text <- paste0(text, "Crossover rate (CR) = ", self$CrossoverrateCR, "\n")
      text <- paste0(text, "Threshold for the number of non-dominated solutions = ", self$Thresholdforthenumberofnondominatedsolutions, "\n")
      text <- paste0(text, "The factor of amplitude for each attribute of the dataset = ", self$Thefactorofamplitudeforeachattributeofthedataset, "\n")
      text <- paste0(text, "Weight for Support = ", self$WeightforSupport, "\n")
      text <- paste0(text, "Weight for Confidence = ", self$WeightforConfidence, "\n")
      text <- paste0(text, "Weight for Comprehensibility = ", self$WeightforComprehensibility, "\n")
      text <- paste0(text, "Weight for Amplitude of the Intervals = ", self$WeightforAmplitudeoftheIntervals, "\n")

      return(text)

    }
  )
)


