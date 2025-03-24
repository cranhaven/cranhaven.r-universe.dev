#' Data of PLoS ONE paper 
#' 
#' Dataset of a subject and modeling data for an auditory word identification task.
#' 
#' @name word_classification_data
#' @docType data
#' @usage data(word_classification_data)
#' 
#'	
#' @format Data from the four experiments and model estimates
#'  \describe{
#'    \item{\code{ExperimentNumber}}{Experiment identifier}
#'    \item{\code{PresentationMethod}}{Method of presentation in the experiment: loudspeaker, headphones 3. Trial: Trial number in the experimental list}
#'    \item{\code{TrialScaled}}{scaled Trial}
#'    \item{\code{Subject}}{anonymized subject identifier}
#'    \item{\code{Item}}{word identifier -german umlaute and special character coded as 'ae' 'oe' 'ue' and 'ss'}
#'    \item{\code{Activation}}{NDL activation}
#'    \item{\code{LogActivation}}{log(activation+epsilon)}
#'    \item{\code{L1norm}}{L1-norm (lexicality)}
#'    \item{\code{LogL1norm}}{log of L1-norm}
#'    \item{\code{RecognitionDecision}}{recognition decision (yes/no)}
#'    \item{\code{RecognitionRT}}{latency for recognition decision}
#'    \item{\code{LogRecognitionRT}}{log recognition RT}
#'    \item{\code{DictationAccuracy}}{dictation accuracy (TRUE: correct word reported, FALSE otherwise) 15. DictationRT: response latency to typing onset}
#'}
#' 
#' @references 
#' 
#' Denis Arnold, Fabian Tomaschek, Konstantin Sering, Florence Lopez, and R. Harald Baayen (2017).
#' Words from spontaneous conversational speech can be recognized with human-like accuracy by 
#' an error-driven learning algorithm that discriminates between meanings straight from smart 
#' acoustic features, bypassing the phoneme as recognition unit  PLoS ONE 12(4):e0174623. 
#' https://doi.org/10.1371/journal.pone.0174623
#' @keywords data
NULL
