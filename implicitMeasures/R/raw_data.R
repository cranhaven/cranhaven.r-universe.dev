#' Dataset with one IAT and two SC-IATs
#'
#' A dataset containing the data from 152 participants who completed
#'  one IAT and two SC-IATs. The object of both the implicit measures
#'  was chocolate, either Milk or Dark chocolate:
#'
#' @docType data
#' @keywords datasets
#' @name raw_data
#' @usage data(raw_data)
#' @format A dataframe with 6 variables, as follows:
#'   \itemize{
#'    \item Participant. Participants ID.
#'    \item latency. Latency of the response times in millisecond.
#'    \item correct. Response accuracy (0--correct, 1--error).
#'    \item trialcode. Factor with 32 levels identifying the trial for each
#'            response, both for the implicit measures and the demographic
#'            questionnaire. It contains also the trials that have to be
#'            eliminated, defined as follows:
#'            \itemize{
#'               \item alert. Defines the SC-IAT trials beyond the response time
#'                            window.
#'               \item Reminder, Reminder1. Identify the instruction page.
#'             }
#'    \item blockcode. Factor with 13 levels as follow:
#'                      \itemize{
#'                        \item practice.iat.Milkbad. IAT practice blocks,
#'                                                     Mapping A.
#'                        \item practice.iat.Milkbad. IAT practice blocks,
#'                                                     Mapping B.
#'                        \item practice.sc_dark.Darkbad. Dark SC-IAT practice
#'                                                         blocks, Mapping A.
#'                        \item practice.sc_dark.Darkbad. Dark SC-IAT practice
#'                                                          blocks, Mapping B.
#'                        \item practice.sc_milk.Milkbad. Milk SC-IAT practice
#'                                                          blocks, Mapping A.
#'                        \item practice.sc_milk.Milkgood. Milk SC-IAT practice
#'                                                           blocks, Mapping B.
#'                        \item test.iat.Milkbad. IAT test blocks, Mapping A.
#'                        \item test.iat.Milkgood. IAT test blocks, Mapping B.
#'                        \item test.sc_dark.Darkbad. Dark SC_IAT test blocks,
#'                                                     Mapping A.
#'                        \item test.sc_dark.Darkbad. Dark SC-IAT test blocks,
#'                                                     Mapping B.
#'                        \item test.sc_milk.Milkbad. Milk SC-IAT test blocks,
#'                                                     Mapping A.
#'                        \item test.sc_milk.Milkgood. Milk SC-IAT test blocks,
#'                                                      Mapping B.
#'                        \item demo. Demographic questionnaire.
#'                       }
#'    \item response. Character registering the type of response for the
#'                     demographic .
#'  }
"raw_data"
