#' Get Conditions
#'
#' Retrieves the experimental conditions from the file name following a naming pattern in which the various components (IDs, conditions, and dimensions) are separated by a non-alphanumeric character.
#'
#' @param path A character string indicating the path to the folder containing the audio files. Default corresponds to the current working directory.
#' @param audioList Optional list with Wave objects to analyze.
#' @param fileType Character string indicating the file format (wav or mp3) of the audio files. Default corresponds to wav.
#' @param fileNamePattern Character string indicating the naming format of the audio files, such as "ID-Condition-Dimension", "Condition_ID_Dimension" or "ID". Default corresponds to "ID_Condition_Dimension".
#' @param sep A non alpha-numeric that acts as separator between the different naming components. Default corresponds to underscore.
#' @param filter Optional character vector used to filter for specific audio files. Default corresponds to NULL.
#' @param recursive A logical value indicating whether subdirectories should be included when searching for voice files. Default corresponds to FALSE.
#' @return Character vector, which contains all the unique conditions of the voice files extracted from the name pattern of the audio files.
#' @examples
#' getConditions(audioList = testAudioList,
#' fileNamePattern = "ID_Condition_Dimension", sep = "_")
#'
#' @importFrom stringr str_extract_all str_replace_all str_split
#' @importFrom xfun file_ext
#' @export


getConditions <- function(path = ".",  audioList = list(), fileType = "wav", fileNamePattern = "ID_Condition_Dimension", sep = "_", filter = NULL, recursive=FALSE) {
  if(is.list(path)){
    stop("Error file path is a list. If you meant to use a Wave list object, specify the audioList parameter")
  }
  if(!is.character(path) || !file.exists(path)) stop("Invalid path!")
  if(!is.list(audioList)) stop("audioList must be a list of Wave objects!")
  if(!is.character(fileType)) stop("fileType must be a string!")
  if(!is.character(fileNamePattern)) stop("fileNamePattern must be a string!")
  if (!grepl("[^[:alnum:]]", sep)) stop("Error: sep should be a non-alphanumeric character")
  if(!is.logical(recursive)) stop("recursive must be a boolean!")



  path <- file.path(path)


  # Read files from path if audioList is NULL
  if (is.null(audioList)) {
    # Find files matching filter and file type in path
    files <- list.files(path, recursive = recursive, full.names = TRUE, include.dirs = FALSE)
    if (!is.null(filter) && length(filter) > 0) {
      files <- files[grep(paste(filter, collapse = "|"), files)]
    }
    if (fileType != "") {
      files <- files[grep(paste0(".", fileType, "$"), files)]
    }
    if (length(files) == 0) {
      stop("Error, no files found in the specified directory")
    }
    processedNames <- str_extract_all(files, "(?<=/|^)[^/]+(?=\\.[^/]+$)")
  } else {
    if(!is.null(filter) && length(filter) > 0) audioList <- audioList[grep(filter, names(audioList))]
    processedNames <- names(audioList)
  }

  # Split file names into components and determine position of dimension component
  components <- str_split(processedNames, sep, simplify = TRUE)
  fileNamePattern <- tolower(fileNamePattern)
  cond_pos <- match("condition", str_split(fileNamePattern, sep, simplify = TRUE)[1, ])
  if (is.na(cond_pos)) {
    conditions <- character()
  } else {
    conditions <- unique(components[, cond_pos])
  }

  return(conditions)
}
