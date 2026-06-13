#' Get Components Name and Positions
#'
#' Indicates the presence and order in which components are retrieved from the file name of each recording.
#'
#' @param fileNames A character vector of audio file names.
#' @param fileNamePattern A character string indicating the naming format of the audio files, such as "ID-Condition-Dimension", "Condition_ID_Dimension" or "ID". Default corresponds to "ID_Condition_Dimension".
#' @param sep A non alpha-numeric that acts as separator between the different naming components. Default corresponds to underscore.
#' @return A list, containing a vector of positions for each component and a data.frame containing the values for each component of the audio files.
#' @examples
#' getComponents(names(testAudioList), fileNamePattern = "ID_Condition_", sep = "_")
#'
#' @importFrom stringr str_split
#' @export

getComponents <- function(fileNames, fileNamePattern = "ID_Condition_Dimension", sep = "_"){
  if(length(fileNames) == 0){
    stop("File names are empty")
  }
  if(!is.character(fileNamePattern)) stop("fileNamePattern must be a string!")
  if (!grepl("[^[:alnum:]]", sep)) stop("Error: sep should be a non-alphanumeric character")



  #Split string using the provided separator
  fileNamePattern1 <- str_split(fileNamePattern, sep)
  #convert text to lower case
  fileNamePattern1 <- as.vector(sapply(fileNamePattern1, tolower))
  #Check for the position of the ID field
  idPosition <- which(fileNamePattern1 %in% "id")
  #Check for the position of the Condition field
  conditionPosition <- which(fileNamePattern1 %in% "condition")
  #Check for the position of the dimension field
  dimensionPosition <- which(fileNamePattern1 %in% "dimension")

  #Get the original names but separated
  processedNames <- str_split(fileNames, sep, simplify = TRUE)


  #Check if some component is missing and set the value of 99 if it is
  if(length(conditionPosition) == 0){
    conditionPosition <- 99
  }
  else if(conditionPosition > ncol(processedNames)){
    conditionPosition <- 99
    warning("Attention: the pattern you defined includes conditions.
          But the names do not have as many divisions (ignoring conditions component)")
  }
  else{
    conditions <- unique(processedNames[,conditionPosition])
  }
  if(length(dimensionPosition) == 0){
    dimensionPosition <- 99
  }
  else if(dimensionPosition > ncol(processedNames)){
    dimensionPosition <- 99
    warning("Attention: the pattern you defined includes dimensions.
          But the names do not have as many divisions (ignoring dimensions component)")
  }
  else{
    dimensions <- unique(processedNames[,dimensionPosition])
  }

  #Create a data frame containing the component names and their positions
  positions <- data.frame(ID = idPosition, Condition = conditionPosition, Dimension = dimensionPosition)
  processedNames <- as.data.frame(processedNames)
  colnames(processedNames)[positions[positions < 99]] <- colnames(positions)[positions < 99]
  processedNames <- processedNames[,!is.na(colnames(processedNames)), drop=FALSE]
  cols_to_keep <- c("ID", "Condition", "Dimension")
  processedNames <- processedNames[,colnames(processedNames) %in% cols_to_keep, drop=FALSE]
  finalList <- list(Positions = positions, Components =  processedNames)
  return(finalList)
}
