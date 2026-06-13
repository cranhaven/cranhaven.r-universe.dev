#' What IDs have missing dimensions?
#'
#' Indicates whether and which dimensions are missing for each ID.
#'
#' @param path Character string indicating the path to the folder containing the audio files. Default corresponds to the current working directory.
#' @param audioList Optional list with Wave objects to analyze.
#' @param ids Character vector indicating the IDs of the files.
#' @param fileType Character string indicating the file format (wav or mp3) of the audio files. Default corresponds to wav.
#' @param fileNamePattern Character string indicating the naming format of the audio files, such as "ID-Condition-Dimension", "Condition_ID_Dimension" or "ID". Default corresponds to "ID_Condition_Dimension".
#' @param sep A non alpha-numeric that acts as separator between the different naming components. Default corresponds to underscore.
#' @param recursive A logical value indicating whether subdirectories should be included when searching for voice files. Default corresponds to FALSE.
#' @return A data.frame, in which rows represent IDs and columns represent missing vs. present Dimensions.
#' @export
#' @examples
#' MissDimPerId(audioList = testAudioList)

MissDimPerId <- function(path = ".", audioList = NULL, ids = c(), fileType = "wav", fileNamePattern = "ID_Condition_Dimension", sep = "_", recursive=FALSE){
  # Validate parameters
  if (!is.character(path) || length(path) != 1) stop("path must be a single character string")
  if (!is.null(audioList) && !is.list(audioList)) stop("audioList must be a list")
  if (length(ids) == 0) {
    warning("No IDs provided, using all IDs")
    ids <- getIds(path = path, audioList = audioList, fileType = fileType, fileNamePattern = fileNamePattern, sep = sep, recursive = recursive)
  }
  if (!is.character(fileType) || length(fileType) != 1) stop("fileType must be a single character string")
  if (!is.character(fileNamePattern) || length(fileNamePattern) != 1) stop("fileNamePattern must be a single character string")
  if (!is.character(sep) || length(sep) != 1) stop("sep must be a single character string")
  if (!is.logical(recursive) || length(recursive) != 1) stop("recursive must be a single logical value")

  # Get all dimensions
  dimensions <- getDimensions(path = path, audioList = audioList, fileType = fileType, fileNamePattern = fileNamePattern, sep = sep, recursive = recursive)

  # Create an empty data frame with the desired IDs and dimensions
  id_vec <- unique(ids)
  df <- data.frame(ID = id_vec, stringsAsFactors = FALSE)
  df[,dimensions] <- NA

  #Check which dimensions are missing for each ID
  for(id in id_vec){

    dimensionsId <- getDimensions(path = path, audioList = audioList, fileType = fileType, sep = sep, fileNamePattern = fileNamePattern, filter = id, recursive=recursive)

    df[df$ID == id,dimensions] <- dimensions %in% dimensionsId

  }



  return(df)
}
