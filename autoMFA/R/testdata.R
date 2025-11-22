#' Test dataset for the MFA model
#'
#' A 720 x 3 test dataset generated from a MFA model with 3 components, 1 factor for 
#' each component. Uneven point distribution with large separation between 
#' clusters relative to the component variance matrices. 
#'
#' @docType data
#'
#' @usage MFA_testdata
#'
#' @format Data matrix with 720 observations of 3 variables.
#'
#' @keywords datasets
#' @examples
#' data(MFA_testdata)
#' plot(MFA_testdata[,1], MFA_testdata[,2])
"MFA_testdata"