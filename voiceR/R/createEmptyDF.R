#' Create an Empty data.frame
#'
#' Internal function which creates an empty data.frame in which the different audio files represent rows and the extracted measures represent columns. Several options can be configured such as joining dimensions or separating conditions.
#'
#' @param path An optional character string indicating the path to the folder containing the audio files. Default corresponds to current working directory.
#' @param audioList Optional list with already loaded Wave objects to analyze.
#' @param fileType Character string indicating the file format (wav or mp3) of the audio files. Default corresponds to wav.
#' @param fileNamePattern A character string indicating the naming format, such as "ID-Condition-Dimension", "Condition_ID_Dimension" or "ID". Default corresponds to "ID_Condition_Dimension".
#' @param sep A non alpha-numeric character that acts as separator between the different naming components. Default corresponds to underscore. This field can be ignored if the audio file names only contain an ID component.
#' @param measures A character vector of measures that should appear in the data frame columns.
#' @param jointDimensions Logical value indicating whether dimensions should be joint into a single or not. Default corresponds to FALSE.
#' @param separateConditions Logical value indicating whether conditions should be separated or not. Default corresponds to TRUE.
#' @param filter Optional character vector indicating IDs, Conditions, Dimensions or other name patterns. Default corresponds to NA.
#' @param recursive Logical value indicating whether subdirectories should be included when searching for audio files. Default corresponds to FALSE.
#' @return An empty data.frame in which ID's represent rows and dimensions/measures represent columns.
#'
#' @importFrom stringr str_remove_all

createEmptyDF <-
  function(path = ".", audioList = list(), fileType = "wav", fileNamePattern = "ID_Condition_Dimension", sep = "_",
           measures = c(), jointDimensions = FALSE, separateConditions = TRUE, filter = NA, recursive = FALSE) {

    if(!is.character(path) || !file.exists(path)) stop("Invalid path!")
    if(!is.list(audioList)) stop("audioList must be a list of Wave objects!")
    if(!is.character(fileType)) stop("fileType must be a string!")
    if(!is.character(fileNamePattern)) stop("fileNamePattern must be a string!")
    if (!grepl("[^[:alnum:]]", sep)) stop("Error: sep should be a non-alphanumeric character")
    if(!is.logical(recursive)) stop("recursive must be a boolean!")
    if(!is.logical(separateConditions)) stop("separateConditions must be a boolean!")
    if(!is.logical(jointDimensions)) stop("separateConditions must be a boolean!")

    if(length(audioList) == 0){
      if(length(filter) == 1){
        if(is.na(filter)){
          files <- list.files(path = path, pattern = fileType, recursive=recursive, full.names = FALSE)
        }
        else{
          files <- list.files(path = path, pattern = filter, recursive=recursive, full.names = FALSE)
          files <- files[grep(fileType, files)]
        }
      }
      else{
        if(length(filter) > 1)
          files <- list.files(path = path, pattern = paste(as.character(filter),collapse="|"), recursive = recursive, full.names = FALSE)
        else
          files <- list.files(path = path, pattern = as.character(filter), recursive = recursive, full.names = FALSE)
        files <- files[grep(fileType, files)]
      }

      processedNames <-
        str_remove_all(files, pattern = paste0("\\.", fileType))
    }
    else{
      processedNames <- names(audioList)
    }




    componentsAndPositions <- getComponents(processedNames, fileNamePattern, sep)

    userIds <-
      unique(componentsAndPositions[["Components"]][,componentsAndPositions[["Positions"]][["ID"]]])
    conditions <- unique(componentsAndPositions[["Components"]][,componentsAndPositions[["Positions"]][["Condition"]]])
    dimensions <- unique(componentsAndPositions[["Components"]][,componentsAndPositions[["Positions"]][["Dimension"]]])

    if(separateConditions & length(as.vector(conditions)) > 0){
      if(jointDimensions | length(as.vector(dimensions)) == 0){
        rowNumber <- length(userIds) * max(length(as.vector(conditions)), 1)
      }
      else{
        rowNumber <- length(userIds) * length(dimensions) * max(length(as.vector(conditions)), 1)

      }
    }
    else{

      if(jointDimensions | length(as.vector(dimensions)) == 0){
        rowNumber <- length(userIds)
      }
      else{
        rowNumber <- length(userIds) * length(dimensions)

      }

    }

    MeasuresDimensionsDF <-
      (data.frame(matrix(
        nrow = rowNumber,
        ncol = length(measures)
      )))
    colnames(MeasuresDimensionsDF) <-  measures


    if(!jointDimensions & separateConditions){
      tempData <- tempData <- expand.grid(userIds, conditions, dimensions)
      colnames(tempData) <- c("ID", "Condition", "Dimension")
    } else if(!jointDimensions){
      tempData <- expand.grid(userIds, dimensions)
      colnames(tempData) <- c("ID", "Dimension")
    } else if(separateConditions) {
      tempData <- expand.grid(userIds, conditions)
      colnames(tempData) <- c("ID", "Condition")
    } else{
      tempData <- as.data.frame(userIds)
      colnames(tempData) <- "ID"
    }

    MeasuresDimensionsDF <- cbind(MeasuresDimensionsDF, tempData)


    return(MeasuresDimensionsDF)


  }
