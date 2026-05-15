#' IBI analysis version 2 
#'
#' This function is an updated approach for the analysis of (IBI) data.
#' Here is a link to the old version \code{\link{ibi_analysis_old}}.
#' It was updated to handle specific errors encountered in the BuildNIHR or InterpolateNIHR functions
#' that resulted from files that were almost empty on IBI data. 
#' 
#' This function analyzes IBI data, focusing on time and frequency domain measures.
#'  Note that the function imports functions from the RHRV package for HRV (Heart Rate Variability)
#'  analysis. Also note that measurements on the wrist are now more often referred to as PRV (Pulse
#'  Rate Variability), see for instance
#'   https://jphysiolanthropol.biomedcentral.com/articles/10.1186/s40101-020-00233-x.
#' 
#' @description The `ibi_analysis` function is an update to the previous `ibi_analysis_old` (version 0.8.1) 
#' and includes improvements to handle Empatica E4 data more effectively. The updated version 
#' tries to mitigate issues with empty IBI files and reduces the likelihood of errors during HRV analysis.
#' 
#' @param IBI IBI data, a dataframe with columns including DateTime and seconds since the start
#' of the recording. This data can be read with read with \code{\link{read_e4}}
#' 
#' @return A list containing the results of the time analysis, frequency analysis (if available), 
#' and a summary of HRV measures. The summary includes time domain measures (SDNN, pNN50, etc.), 
#' frequency domain measures (HF, LF, LFHF, etc.), and counts of original and accepted beats.
#'
#' @details
#' The function performs several steps in processing IBI data:
#' 1. Initializes HRV data structure using RHRV.
#' 2. Handles potential data discrepancies due to the nature of wrist-worn devices.
#' 3. Implements error handling for BuildNIHR and InterpolateNIHR functions.
#' 4. Processes the data through time and frequency domain analyses.
#' 5. Returns a comprehensive summary of HRV measures.
#' 
#' @examples
#' \dontrun{
#' zip_path <- system.file("extdata", "1635148245_A00204.zip", package = "wearables")
#' Assuming "IBI_data" is your interbeat interval data
#' result <- read_e4("path to your file") 
#' print(result$IBI)
#' }
#' 
#' @export
#' @importFrom RHRV CreateHRVData SetVerbose BuildNIHR FilterNIHR InterpolateNIHR CreateTimeAnalysis CreateFreqAnalysis CalculatePowerBand
ibi_analysis <- function(IBI){
  # Select the heart beat positions in time. Use the amount of seconds since the start
  e4_hrv_data <- RHRV::CreateHRVData()
  e4_hrv_data <- RHRV::SetVerbose(e4_hrv_data, TRUE)
  e4_hrv_data$datetime <- as.POSIXlt(IBI$DateTime)[1]
  
  # There is no 0 added to the Empatica E4 seconds column, therefore, slight deviations
  # with RHRV are possible. To match RHRV outcome, add 0 to the dataframe.
  # Reason for not adding the 0 is that Empatica does not contain a valid first
  # RR interval from the start of the study.
  e4_hrv_data$Beat <- data.frame(Time = IBI$seconds)
  
  n_beats_original <- nrow(e4_hrv_data$Beat)
  
  # Integration of new version 2
  # Define the error handling function
  handle_hrv_error <- function(e) {
    warning("Error in BuildNIHR or InterpolateNIHR: ", e$message, "-> all hrv values set to NA")
    list(time = create_empty_time_list(), 
         freq = create_empty_freq_list(),
         n_beats_accepted = 0)
  }
  
  # Main processing with error handling
  result_hrv_processing <- tryCatch({
  
  # Then build the non interpolated heart rate series
  e4_hrv_data <- RHRV::BuildNIHR(e4_hrv_data)
  
  # Pay attention that we don't need the inter-beat-intervals as RHRV does not know how to handle these
  # as there are so much missing values in there.
  
  # Remove too short RR intervals or missed beats
  # This also provides the number of accepted beats
  
  e4_hrv_data <- RHRV::FilterNIHR(e4_hrv_data)
  
  n_beats_accepted <- nrow(e4_hrv_data$Beat)
  
  # Note that it is not necessary to specify freqhr since it matches with
  # the default value: 4 Hz
  # suppressWarnings({
  e4_hrv_data <- RHRV::InterpolateNIHR(e4_hrv_data, freqhr = 4)
  
  e4_hrv_data <- RHRV::CreateTimeAnalysis(e4_hrv_data,
                                          size = 300,
                                          interval = 7.8125
  )
  # })
  
  # We typically have a lot of missing beats with wristbands, so frequency analysis is difficult.
  e4_hrv_data <- RHRV::CreateFreqAnalysis(e4_hrv_data)
  
  e4_hrv_data <- RHRV::CalculatePowerBand(e4_hrv_data,
                                          indexFreqAnalysis = 1,
                                          size = 300, shift = 30, type = "fourier",
                                          ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05,
                                          LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4
  )
  
  list(time = e4_hrv_data$TimeAnalysis[[1]],
       freq = e4_hrv_data$FreqAnalysis[[1]],
       n_beats_accepted = n_beats_accepted)
  }, error = handle_hrv_error) 
  
  
  # Assign time, freq and n_beats_accepted
  time <- result_hrv_processing$time
  freq <- result_hrv_processing$freq
  n_beats_accepted <- result_hrv_processing$n_beats_accepted
  
  list(
    time_analysis = time,
    # freq_analysis = freq,
    summary = list(
      time = list(
        SDNN = time$SDNN,
        pNN50 = time$pNN50,
        SDSD = time$SDSD,
        rMSSD = time$rMSSD,
        HRVi = time$HRVi,
        SDANN = time$SDANN,
        TINN = time$TINN
      ),
      frequency = list(
        HF = mean(freq$HF),
        LF = mean(freq$LF),
        LFHF = mean(freq$LFHF),
        VLF = mean(freq$VLF),
        ULF = mean(freq$ULF)
      ),
      beats = list(
        beats_original = n_beats_original,
        beats_accepted = n_beats_accepted
      )
    )
  )
}