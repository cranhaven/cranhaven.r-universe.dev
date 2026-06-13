#' Automatically analyze audio files
#'
#' Automatically analyzes audio files and outputs a data.frame with their main extracted audio features.
#'
#' @details The voiceR package requires the audio file names to follow a specific pattern, in which the different components are separated by a non alphanumeric character (e.g., “_”). File name components refer to:
#' \describe{
#'   \item{ID}{Unique identifier of the speaker or recording.}
#'   \item{Condition}{Experimental condition or other grouping variable.}
#'   \item{Dimension}{Additional survey or experiment information (e.g., additional conditions).}
#' }
#' Order and presence of the different components is not important, as long as at least one of the aforementioned components is present.
#' Furthermore, non-relevant components can be skipped by specifying “Null” in its position such as: ID_Null_Condition. Valid name patterns are, for example, 876h Interior (ID Condition), Exterior-3543h (Condition-ID), 983b-Exterior-q1 (ID-Condition-Dimension) or 455k (ID). All voice files within one session need to follow the same file naming pattern.
#' Note: the non-alpha numeric separator should also be specified as sep.
#'
#' @param path An optional character string indicating the path to the folder containing the audio files. Default corresponds to the current working directory. (You should only define it if the audios you wish to analyze are not already read in R. Otherwise define the audioList parameter).
#' @param audioList An optional list of Wave objects to analyze.
#' @param filter An optional character vector indicating IDs, Conditions, Dimensions, or other patterns used to filter for specific audio files. Default corresponds to NA.
#' @param fileType A character string indicating the audio file format (wav or mp3). Default corresponds to wav.
#' @param fileNamePattern A character string indicating the naming format of the audio files, such as "ID-Condition-Dimension", "Condition_ID_Dimension" or just "ID". Default corresponds to "ID".
#' @param sep A non alpha-numeric character that acts as separator between the different naming components. Default corresponds to an underscore. This field can be ignored if the audio file name only contains an ID component.
#' @param parallel Logical value indicating whether to use parallelism to extract the different audio characteristics to enhance computational performance. Default corresponds to FALSE.
#' @param recursive Logical value indicating whether subdirectories in the specified directory should be included when searching for voice files. Default corresponds to FALSE.
#' @param preprocess Logical value indicating whether to preprocess (normalize amplitude and remove background noise) the audio files before extraction and analysis. Default corresponds to FALSE.
#' @param extended Logical value indicating whether all features extracted by the soundgen package should be inputted. Default corresponds to FALSE.
#' @param ... Other options used to control preprocessing behavior.
#' @return A data.frame is created with the following audio features:
#' \describe{
#'   \item{duration}{Total duration in seconds.}
#'   \item{voice_breaks_percent}{Proportion of unvoiced frames.}
#'   \item{RMS_env}{Root mean square of the amplitude envelope.}
#'   \item{mean_loudness}{Average subjective loudness in sone.}
#'   \item{mean_F0}{Average fundamental frequency in Hertz.}
#'   \item{sd_F0}{Standard deviation of the fundamental frequency in Hertz.}
#'   \item{mean_entropy}{Average Wiener entropy. A value of 0 indicates a pure tone, while a value of 1 indicates white noise.}
#'   \item{mean_HNR}{Average Harmonics-to-Noise Ratio.}
#'   \item{ID}{ID component of the audio file.}
#'   \item{Condition}{If fileNamePattern and audio names include a Condition, an additional column with the Condition component of the audio file is included.}
#'   \item{Dimension}{If fileNamePattern and audio names include a Dimension, an additional column with the Dimension component of the audio file is included.}
#' }
#' @seealso [soundgen::analyze()], [seewave::duration()], [seewave::rms()], [seewave::env()]
#' @examples
#' \donttest{
#' audioData <- autoExtract(audioList = testAudioList, filter = c("5b438f516066ad470d3be72c52005251"))
#' }
#'
#' @importFrom stringr str_sub str_length str_split
#' @importFrom parallel makeCluster detectCores stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom seewave duration rms env zapsilw
#' @importFrom soundgen analyze
#' @importFrom foreach foreach
#' @export

autoExtract <- function(path = ".", audioList = list(), filter = NA, fileType = "wav", fileNamePattern = "ID_Condition_Dimension", sep = "_", parallel = FALSE, recursive = FALSE, preprocess = FALSE, extended = FALSE, ...){
  if(is.list(path)) stop("Did you pass a list of Wave objetcs as a path? If so, specify audioList parameter")
  if(!is.list(audioList)) stop("audioList must be a list of Wave objects!")
  if(!is.character(path) || !file.exists(path)) stop("Invalid path!")
  if(!is.character(fileType)) stop("fileType must be a string!")
  if(!is.character(fileNamePattern)) stop("fileNamePattern must be a string!")
  if (!grepl("[^[:alnum:]]", sep)) stop("Error: sep should be a non-alphanumeric character")
  if(!is.logical(parallel)) stop("parallel must be a boolean!")
  if(!is.logical(recursive)) stop("recursive must be a boolean!")
  if(!is.logical(preprocess)) stop("preprocess must be a boolean!")

  if(all(is.na(filter)) || length(filter) == 0){
    filter <- c()
  } else {
    filter = paste(filter,collapse="|")
  }

  #Measures to compute
  measures <- c("duration", "voice_breaks_percent", "RMS_env", "mean_loudness", "mean_F0", "sd_F0", "mean_entropy", "mean_HNR")
  initialMeasures <- measures

  if(length(audioList) == 0){
    #Check path to prevent errors
    if(str_sub(path, -1) == "/"){
      path <- str_sub(path, 1, str_length(path)-1)
    }
    audioList <- readAudio(path = path, filter = filter, fileType = fileType, recursive = recursive)
  }
  else{
    if(length(filter) != 0) audioList <- audioList[grep(filter, names(audioList))]
  }


  #Check if pattern is correct
  checkPattern <- str_split(fileNamePattern, sep, simplify = TRUE)
  checkPattern <- tolower(checkPattern)
  patternCorrectness <- checkPattern %in% c("id", "condition", "dimension", "null")



  if(length(checkPattern[!patternCorrectness]) >= 1){
    stop(paste("Incorrect Components:", checkPattern[!patternCorrectness]))
  }





  #Extract the different components and their respective positions from the file names
  componentsAndPositions <- getComponents(names(audioList), fileNamePattern, sep)
  ids <- unique(componentsAndPositions[["Components"]][,"ID"])

  #Check for missing components
  if(componentsAndPositions$Positions["Condition"] == 99){
    conditions <- ""
    conditionPresence <- FALSE
  }else{
    conditions <- unique(componentsAndPositions[["Components"]][,"Condition"])
    conditionPresence <- TRUE
  }

  if(componentsAndPositions$Positions["Dimension"] == 99){
    dimensions <- ""
    dimensionPresence <- FALSE
  }else{
    dimensions <- unique(componentsAndPositions[["Components"]][,"Dimension"])
    dimensionPresence <- TRUE
  }



  # Create a list with all the audios in tuneR format and normalize them if required

    if(preprocess){
      audioList <- preprocess(audioList, ...)
    }

  #create an empty data.frame with all the files and measures
  measures <- c("duration", "voice_breaks_percent", "RMS_env", "mean_loudness", "mean_F0", "sd_F0", "mean_entropy", "mean_HNR")
  audioData <- as.data.frame(matrix(nrow = length(audioList), ncol = length(measures)))
  row.names(audioData) <- names(audioList)
  colnames(audioData) <- measures


  components <- getComponents(rownames(audioData), fileNamePattern, sep)

  if(dimensionPresence){
    audioData <- cbind("Dimension" = components[["Components"]][,"Dimension"], audioData)
  }
  if(conditionPresence){
    audioData <- cbind("Condition" = components[["Components"]][,"Condition"], audioData)
  }


  audioData <- cbind("ID" = components[["Components"]][,"ID"], audioData)



  #Extract the different features and fill them into the previous data.frame (use parallel processing if parallel is True)
  if(parallel == TRUE){

    i <- 1
    cl <- makeCluster(detectCores() - 2)
    registerDoParallel(cl)

    audioData <- foreach::`%dopar%`(
      foreach(i = 1:length(audioList), .combine=rbind, .export = c("getComponents"), .errorhandling = 'remove'), {

          audioName <- names(audioList)[i]
          componentsAndPositions <- getComponents(audioName, fileNamePattern, sep)
          id <- componentsAndPositions[["Components"]][,"ID"]
          tempData <- as.data.frame(matrix(nrow = 1, ncol = length(measures)))
          colnames(tempData) <- measures
          if(conditionPresence){
            condition <- componentsAndPositions[["Components"]][,"Condition"]
            tempData[,"Condition"] <- condition
          }
          if(dimensionPresence){
            dimension <- componentsAndPositions[["Components"]][,"Dimension"]
            tempData[,"Dimension"] <- dimension
          }



          tempData[,"ID"] <- id

          sound_orig = as.numeric(scale(audioList[[audioName]]@left))
          samplingRate = audioList[[audioName]]@samp.rate
          #    savewav(audioList[[audioName]], filename = paste0(newFolder, "/",audioName, ".", "wav"))





          analyzeData <- analyze(audioList[[audioName]]@left, samplingRate = audioList[[audioName]]@samp.rate, plot = FALSE, osc = FALSE, summaryFun = c("mean", "sd"), ...)
          analyzeData <- analyzeData$summary



          if(extended){
            tempData[,colnames(analyzeData)] <- analyzeData

          }
          else{
            tempData[, measures[1]] <- duration(audioList[[audioName]])
            tempData[, measures[2]] <- 1 - analyzeData$voiced
            tempData[, measures[3]] <- rms(env(zapsilw(audioList[[audioName]], plot = FALSE),f=audioList[[audioName]]@samp.rate, plot = FALSE))
            tempData[, measures[4]] <- analyzeData$loudness_mean
            tempData[, measures[5]] <- analyzeData$pitch_mean
            tempData[, measures[6]] <- analyzeData$pitch_sd
            tempData[, measures[7]] <- analyzeData$entropy_mean
            tempData[, measures[8]] <- analyzeData$HNR_mean

          }

          tempData



      })
    stopCluster(cl)
  }
  else{

      for(i in 1:length(audioList)) {
        tryCatch({
          audioName <- names(audioList)[i]
          componentsAndPositions <- getComponents(audioName, fileNamePattern, sep)
          id <- componentsAndPositions[["Components"]][,"ID"]
          sound_orig = as.numeric(scale(audioList[[audioName]]@left))
          samplingRate = audioList[[audioName]]@samp.rate
          #    savewav(audioList[[audioName]], filename = paste0(newFolder, "/",audioName, ".", "wav"))

          if(conditionPresence & dimensionPresence){
            dimension <- componentsAndPositions[["Components"]][,"Dimension"]
            Condition <- componentsAndPositions[["Components"]][,"Condition"]
            rowsFilter <- audioData$ID == id & audioData$Dimension == dimension & audioData$Condition == Condition
          }
          else if(conditionPresence){
            Condition <- componentsAndPositions[["Components"]][,"Condition"]

          }
          else if(dimensionPresence){
            dimension <- componentsAndPositions[["Components"]][,"Dimension"]

          }

          rowsFilter <- which(rownames(audioData) %in% names(audioList)[i])


          analyzeData <- analyze(audioList[[audioName]]@left, samplingRate = audioList[[audioName]]@samp.rate, plot = FALSE, osc = FALSE, summaryFun = c("mean", "sd"))
          analyzeData <- analyzeData$summary

          if(extended){
            audioData[rowsFilter,colnames(analyzeData)] <- analyzeData

          }
          else{
            audioData[rowsFilter, measures[1]] <- duration(audioList[[audioName]])
            audioData[rowsFilter, measures[2]] <- 1 - analyzeData$voiced
            audioData[rowsFilter, measures[3]] <- rms(env(zapsilw(audioList[[audioName]], plot = FALSE),f=audioList[[audioName]]@samp.rate, plot = FALSE))
            audioData[rowsFilter, measures[4]] <- analyzeData$loudness_mean
            audioData[rowsFilter, measures[5]] <- analyzeData$pitch_mean
            audioData[rowsFilter, measures[6]] <- analyzeData$pitch_sd
            audioData[rowsFilter, measures[7]] <- analyzeData$entropy_mean
            audioData[rowsFilter, measures[8]] <- analyzeData$HNR_mean
          }



        }, error = function(e) {
          warning(paste(audioName, "excluded from the analysis. File corrupted or with no sound."))
        })
      }

  }
    #Postprocess check: Remove empty audios
    audioData <- audioData[!is.na(audioData$duration),]
    audioData <- audioData[,!is.na(colnames(audioData))]
    rownames(audioData) <- c()
    if(extended){
      audioData <- audioData[,!colnames(audioData) %in% initialMeasures[-1]]
    }
    #Return the filled data.frame with all the vocal measurements
    return(audioData)

}
