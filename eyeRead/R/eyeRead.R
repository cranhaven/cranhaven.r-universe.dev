#' eyeRead: A Package to Prepare/Analyse Eye Tracking Data for Reading
#' 
#' This package contains some functions to prepare and analyse eye 
#' tracking data of reading exercises. Essentially it identifies first pass and 
#' second pass fixations and their respective total durations.
#' 
#' @section eyeRead functions:  
#' \itemize{
#'  \item \link{compileAOI} Compiles the information on AOI's in separate variables to one variable
#'  \item \link{AOItransitions} Calculates transitions between AOI's
#'  \item \link{codePasses} Codes fixations as first pass and second pass with or without rereading
#'  \item \link{convert} Converts  between visual degrees and centimeters, inches or pixels
#'  \item \link{fixDur} Calculates the fixation durations for the passes or the AOI's
#' }
#' 
#' @docType package
#' @name eyeRead

NULL