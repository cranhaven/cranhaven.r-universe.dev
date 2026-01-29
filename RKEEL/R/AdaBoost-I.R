#Class implementing a Classification Algorithm
  #Implements the AdaBoost.NC-C KEEL classification algorithm

AdaBoost_I <- function(train, test, pruned=TRUE, confidence=0.25, instancesPerLeaf=2, numClassifiers=10, algorithm="ADABOOST.NC", trainMethod="NORESAMPLING", seed=-1){
  alg <- RKEEL::R6_AdaBoost_I$new()
  alg$setParameters(train, test, pruned, confidence, instancesPerLeaf, numClassifiers, algorithm, trainMethod, seed)
  return (alg)
}

R6_AdaBoost_I <- R6::R6Class("R6_AdaBoost_I",

  inherit = ClassificationAlgorithm,

  public = list(

    #Public properties

    #pruned
    pruned = TRUE,

    #confidence
    confidence = 0.25,

    #instances per leaf
    instancesPerLeaf = 2,

    #Number of Classifiers
    numClassifiers = 10,

    #Algorithm
    algorithm = "ADABOOST.NC",

    #Train method
    trainMethod = "NORESAMPLING",

    #seed
    seed = -1,


    #Public functions

    #Initialize function
    setParameters = function(train, test, pruned=TRUE, confidence=0.25,
                          instancesPerLeaf=2, numClassifiers=10,
                          algorithm="ADABOOST", trainMethod="NORESAMPLING",
                          seed=-1){

      super$setParameters(train, test)

      self$pruned <- pruned

      self$confidence <- confidence

      self$instancesPerLeaf <- 2

      self$numClassifiers <- numClassifiers

      if((tolower(algorithm) == "adaboost")){
        self$algorithm <- algorithm
      }
      else{
        #Default value of algorithm
        self$algorithm <- "ADABOOST"
      }

      if((tolower(trainMethod) == "noresampling") || (tolower(trainMethod) == "resampling")){
        self$trainMethod <- toupper(trainMethod)
      }
      else{
        #Default value of training method
        self$trainMethod <- "NORESAMPLING"
      }

      if(seed == -1) {
        self$seed <- sample(1:1000000, 1)
      }
      else {
        self$seed <- seed
      }

    }

  ),

  private = list(

    #Private properties

    #jar Filename
    jarName = "Ensembles-I.jar",

    #algorithm name
    algorithmName = "AdaBoost-I",

    #String with algorithm name
    algorithmString = "AdaBoost algorithm",


    #Private functions

    #Get the text with the parameters for the config file
    getParametersText = function(){

      text <- ""
      text <- paste0(text, "seed = ", self$seed, "\n")
      text <- paste0(text, "pruned = ", self$pruned, "\n")
      text <- paste0(text, "confidence = ", self$confidence, "\n")
      text <- paste0(text, "isntancesPerLeaf = ", self$instancesPerLeaf, "\n")
      text <- paste0(text, "Number of Classifiers = ", self$numClassifiers, "\n")
      text <- paste0(text, "Algorithm = ", self$algorithm, "\n")
      text <- paste0(text, "Train Method = ", self$trainMethod, "\n")

      return(text)

    }
  )
)
