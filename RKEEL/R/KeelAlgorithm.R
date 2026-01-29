#Class that defines a KEEL Algorithm
  #Implements the common functions of a KEEL algorithm
require(rJava)


KeelAlgorithm <- R6::R6Class("KeelAlgorithm",

  public = list(

    #Public properties


    #Public functions

    #Initialize
    initialize = function(){

      rJava::.jinit()
      jv <-  rJava::.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
      if(substr(jv, 1L, 2L) == "1.") {
        jvn <- as.numeric(paste0(strsplit(jv, "[.]")[[1L]][1:2], collapse = "."))
        if(jvn < 1.8) stop("Java >= 8 is needed for this package but not available")
      }

      rJava::.jinit()
      javaPath <- paste0(rJava::.jcall('java/lang/System', 'S', 'getProperty', 'java.home'), "\\bin\\")

      private$jarPath <- system.file("exe", "", package="RKEEL")
      private$exePath <- system.file("exe", "", package="RKEEL")

      if(substr(private$jarPath, nchar(private$jarPath), nchar(private$jarPath)) != "/"){
        private$jarPath <- paste0(private$jarPath, "/")
      }
      if(substr(private$exePath, nchar(private$exePath), nchar(private$exePath)) != "/"){
        private$exePath <- paste0(private$exePath, "/")
      }
      if(substr(private$dataPath, nchar(private$dataPath), nchar(private$dataPath)) != "/"){
        private$dataPath <- paste0(private$dataPath, "/")
      }

      #Test paths
      if(! file.exists(system.file("exe", "RunKeel.jar", package = "RKEEL"))){
        stop("RunKeel.jar doesn't exist under the defined path. Installation error.")
      }

    },

    #Execute algorithm
    run = function(folderPath, expUniqueName, javaOptions){
      #Manage expPath
      expPath <- ""
      if(missing(folderPath)){
        expPath <- gsub("\\\\", "/", tempdir())
      }
      else{
        expPath <- folderPath
      }

      if(substr(expPath, nchar(expPath), nchar(expPath)) != "/"){
        expPath <- paste0(expPath, "/")
      }

      #Manage expUniqueName
      if(missing(expUniqueName)){
        private$mainPath <- paste0(expPath, "experiment_", gsub(" ", "_", gsub(":", "-", toString(Sys.time()))), sample(1:10000, 1))
      }
      else{
        private$mainPath <- paste0(expPath, expUniqueName)
      }

      if(dir.exists(private$mainPath)){
        stop(paste0("The current experiment folder ",  private$mainPath, " already exists. Please select an unique experiment folder name.", sep="\n"))
      }

      private$generateExperimentDir(private$mainPath)

      #Manage options to java command line
      if(missing(javaOptions)){
        private$javaOpt <- ""
      }
      else{
        private$javaOpt <- javaOptions
      }

      #Continue implementing in each algorithm type class


    },

    #Print object
    print = function(...) {
      cat("Keel Algorithm Object", sep="\n")
      cat("Need to have an object of an algorithm implementing this class")
    }

  ),

  private = list(

    #Private properties

    #RunKeel.jar path
    jarPath = "", # system.file("exe", "", package="RKEEL"),
    
    #exes path
    exePath = "", # system.file("exe", "", package="RKEEL"),
    
    #dataset path
    dataPath = getDataPath(),

    #path for experiments
    mainPath = NULL,

    #java bin path
    javaPath = "",

    #java command line options
    javaOpt = "",

    #Private Functions

    #Create XML function
    writeKeelXML = function(mainPath){
      #Implement in each algorithm class
    },

    #Create config files function
    writeKeelConfig = function(mainPath){
      #Implement in each algorithm class
    },

    #Generate experiment directory
    generateExperimentDir = function(mainPath){
      dir.create(mainPath)
      dir.create(paste0(mainPath, "/datasets"))
      dir.create(paste0(mainPath, "/exe"))
      dir.create(paste0(mainPath, "/results"))
      dir.create(paste0(mainPath, "/scripts"))
      file.copy(system.file("exe", "RunKeel.jar", package = "RKEEL"), paste0(mainPath, "/scripts/RunKeel.jar"))
    }

  )
)
