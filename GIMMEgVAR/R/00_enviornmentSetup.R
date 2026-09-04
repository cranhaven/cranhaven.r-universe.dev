#' GIMMEgVAR: Environment Setup
#'
#' Loads needed packages and creates directories
#' where logfiles and output from 'GIMMEgVAR'
#' will be stored
#'
#' @param outputPath The user specified path to the directory where results files should be stored.

environmentSetup <- function(outputPath){

  #attach required packages
   requiredPackages <- c("graphicalVAR",
                         "qgraph",
                         "here",
                         "png")

  invisible(lapply(requiredPackages, library, character.only = TRUE))

  #create needed output directory if it doesn't exist
  if (dir.exists(outputPath)==FALSE){
    dir.create(paste0(file.path(outputPath)))
  }

  #create logFile directory
  dir.create(here::here(outputPath,"logFiles"))

  #create gvar output directory
  dir.create(here::here(outputPath,"gvarFiles"))

  #create gimmegvar output directory
  dir.create(here::here(outputPath,"gimmegvarFiles"))
}

