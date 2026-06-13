#' Normalize Amplitude
#'
#' Internal function adapted from the soundgen package, in order to normalize the amplitude of all the objects in a list of Wave objects.
#'
#' @param audioList A list of Wave objects.
#' @param type normalize so the output files has the same peak amplitude ('peak'), root mean square amplitude ('rms'), or subjective loudness in sone ('loudness').
#' @param maxAmp maximum amplitude in dB (0 = max possible, -10 = 10 dB below max possible, etc.).
#' @param summaryFun should the output files have the same mean / median / max etc rms amplitude or loudness? (summaryFun has no effect if type = 'peak').
#' @param windowLength length of FFT window, ms
#' @param step you can override overlap by specifying FFT step, ms (NB: because digital audio is sampled at discrete time intervals of 1/samplingRate, the actual step and thus the time stamps of STFT frames may be slightly different, eg 24.98866 instead of 25.0 ms).
#' @param overlap	overlap between successive FFT frames, \%.
#' @param killDC if TRUE, removed DC offset (see also flatEnv).
#' @param windowDC the window for calculating DC offset, ms.
#' @param verbose a logical value for printing messages.
#' @param progress a logical value to report the progress (only for the Shiny app).
#' @return list of normalized Wave objects.
#'
#' @source This function is adapted from `soundgen::normalizeFolder()`.
#' @importFrom tuneR normalize
#' @importFrom shiny incProgress
#' @importFrom soundgen getRMS getLoudness
#' @noRd
normAmplitude = function(audioList,
                           type = c('peak', 'rms', 'loudness')[1],
                           maxAmp = 0,
                           summaryFun = 'mean',
                           windowLength = 50,
                           step = NULL,
                           overlap = 70,
                           killDC = FALSE,
                           windowDC = 200,
                           verbose = TRUE, progress=FALSE) {
  files <- audioList
  n <- length(audioList)

  if(n == 0){
    stop("No audio Files provided")
  }

  ## process all the files
  if (verbose) message('Processing...')
  # for either peak or RMS normalization, start by peak normalization to maxAmp dB
  level = 10 ^ (maxAmp / 20)
  for (i in 1:n) {
    files[[i]] = normalize(files[[i]],
                                  unit = as.character(files[[i]]@bit),
                                  rescale = TRUE, level = level)
    if(progress)
      incProgress(1/(length(files)))
  }

  # for RMS- or loudness-normalization, perform additional steps
  if (type %in% c('rms', 'loudness')) {
    perSound = vector('list', n)
    if (type == 'rms') {
      for (i in 1:n) {
        # calculate the RMS amplitude of each file
        perSound[[i]] = getRMS(files[[i]]@left,
                               samplingRate = files[[i]]@samp.rate,
                               windowLength = windowLength,
                               step = step,
                               overlap = overlap,
                               scale = 2^(files[[i]]@bit - 1),
                               killDC = killDC,
                               windowDC = windowDC,
                               plot = FALSE)

      }
    } else if (type == 'loudness') {
      for (i in 1:n) {
        # estimate subjective loudness of each file
        perSound[[i]] = getLoudness(as.numeric(files[[i]]@left),
                                    samplingRate = files[[i]]@samp.rate,
                                    scale = 2^(files[[i]]@bit - 1),
                                    windowLength = windowLength,
                                    step = step,
                                    plot = FALSE)$loudness
      }
    }

    # summary measure per file
    summaryPerSound = unlist(lapply(perSound, summaryFun))
    names(summaryPerSound) = names(audioList)

    # find the quietest file
    ref = which.min(summaryPerSound)

    # the quietest file is untouched, but all others are rescaled to have the
    # same RMS/loudness as the quietest one
    for (i in 1:n) {
      if (i != ref) {
        if (type == 'rms') {
          rescale = summaryPerSound[ref] / summaryPerSound[i]
        } else if (type == 'loudness') {
          rescale = (summaryPerSound[ref] / summaryPerSound[i]) ^ (5 / 3)
        }
        files[[i]]@left = as.integer(round(files[[i]]@left * rescale))
        if(progress)
          incProgress(1/(length(files)))
      }
    }
  }

  # return the rescaled files
  return(files)
}
