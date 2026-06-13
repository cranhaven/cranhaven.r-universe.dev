#' Read Audio Files
#'
#' Loads all audio files in the specified directory and provides the option to filter via ID, conditions, and/or dimensions.
#'
#' @param path Character string indicating the full path to the folder containing the audio files. Default corresponds to the current working directory.
#' @param filter Optional character vector, containing patterns, such as IDs or conditions of each audio file.
#' @param fileType Character string indicating the file format (wav or mp3) of the audio files. Default corresponds to wav.
#' @param recursive A logical value indicating whether subdirectories should be included when searching for voice files. Default corresponds to FALSE.
#' @return Returns a list of Wave objects.
#' @examples
#' readAudio(system.file("Audios", package = "voiceR"),
#' fileType = "wav", recursive = TRUE)
#'
#' @importFrom tuneR readWave readMP3
#' @importFrom stringr str_remove_all str_sub str_length
#' @export

readAudio <-
  function(path = ".", filter = c(),
           fileType = "wav",
           recursive = FALSE
           ) {

    if(!is.character(path) || !file.exists(path)) stop("Invalid path!")
    if(!is.logical(recursive)) stop("recursive must be a boolean!")


    #Check path to prevent errors
    if(str_sub(path, -1) == "/"){
      path <- str_sub(path, 1, str_length(path)-1)
    }
    #If filter is NA convert it to an empty vector. Otherwise concatenate different filter words using the or operator
    if(all(is.na(filter)) || length(filter) == 0){
      filter <- c()
    } else {
      filter = paste(filter,collapse="|")
    }

    #list files in the specified folDer, matching the specified patterns and the specified file type
    files <- list.files(path = path, pattern = filter, recursive = recursive, full.names = TRUE)
    fileType <- tolower(fileType)

    if(length(files) == 0){
      stop("No audio files found", call. = FALSE)
    }

    audioList <- vector("list",length(files))

    i <- 1
    for (file in files) {
      if(fileType == "wav"){
        audioList[[i]] <- readWave(file)
        names(audioList)[i] <- basename(str_remove_all(file, pattern = paste0("\\.", fileType)))
      }
      else{
        audioList[[i]] <- readMP3(file)
        names(audioList)[i] <- basename(str_remove_all(file, pattern = paste0("\\.", fileType)))
      }
      i <- i + 1
    }


    return(audioList)

  }
