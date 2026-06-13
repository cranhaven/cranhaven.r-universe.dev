#' Preprocess list of Audio objects
#'
#' Automatically preprocesses a list of Wave objects by normalizing their amplitude and removing background noise.
#'
#' @param audioList A list of Wave objects.
#' @param normalizeAmplitude A logical value indicating whether to normalize amplitude.
#' @param removeNoise A logical value indicating whether to remove background noise.
#' @param ... Other options used to control preprocessing behavior.
#' @return A list of (processed) Wave objects.
#' @examples
#' preprocess(testAudioList)
#'
#' @importFrom seewave rmnoise
#' @export
preprocess <- function(audioList, normalizeAmplitude = TRUE, removeNoise = TRUE, ...){

  if(!is.list(audioList)) stop("audioList must be a list of Wave objects!")
  if(!is.logical(normalizeAmplitude)) stop("normalizeAmplitude must be a boolean!")
  if(!is.logical(removeNoise)) stop("removeNoise must be a boolean!")

  #Separate arguments for normalizeAmplitude and removeNoise
  normArgs_string <- c("type", "maxAmp", "summaryFun", "windowLength", "step", "overlap", "killDC", "windowDC", "verbose", "progress")
  extra_args <- list(...)
  normArgs <- extra_args[names(extra_args) %in% normArgs_string]
  normArgs[["audioList"]] = audioList
  rmnoiseArgs <- extra_args[!names(extra_args) %in% normArgs_string]

  rmnoiseArgs[["output"]] = "Wave"

  #if normalize amplitude, then normalize amplitude
  if(normalizeAmplitude){
    audioList <- do.call(normAmplitude, normArgs)
  }
  #Remove noise
  if(removeNoise){
    for (i in 1:length(audioList)) {
      audioList[[i]]  <-  do.call(rmnoise, c("wave" = audioList[[i]], "f" = audioList[[i]]@samp.rate, rmnoiseArgs))
    }
  }
  return(audioList)

}
