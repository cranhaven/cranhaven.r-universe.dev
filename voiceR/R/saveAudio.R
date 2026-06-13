#' Save Audio Files
#'
#' Save all objects in a list of \code{Wave} objects as a .wav or .mp3 file in the specified path.
#'
#' @param audioList A list of Wave objects.
#' @param path A character string indicating the full path to the folder containing the audio files. Default corresponds to the current working directory.
#' @param fileType Character string indicating the file format (wav or mp3) of the audio files. Default corresponds to wav.
#' @return Save objects of a \code{Wave} list as .mp3 or .wav files.
#' @examples
#' \donttest{
#' saveAudio(testAudioList, fileType = "wav")
#' }
#' @importFrom tuneR writeWave
#' @export
saveAudio <- function(audioList, path = "./Preprocessed/", fileType = "wav"){
  if(!is.character(path) || !file.exists(path)) stop("Invalid path!")
  if(!is.list(audioList)) stop("audioList must be a list of Wave objects!")
  if(!is.character(fileType)) stop("fileType must be a string!")

  if(substr(path, nchar(path), nchar(path)) != "/")
    path <- paste0(path, "/")
  if(substr(fileType, 1, 1) != ".")
    fileType <- paste0(".", fileType)

  #create directory if it does not exist
  ifelse(!dir.exists(file.path(path)), dir.create(file.path(path)), FALSE)

  #save files using tuner
  for (i in 1:length(audioList)) {
    writeWave(object = audioList[[i]], filename = paste0(path, names(audioList[i]),  fileType))
  }
}
