#' A function which removes missing data from the DC
#'
#' Currently the psc package works only on complete-case datasets.  This
#' function removes rows with missing data and returns a warning to inform the
#' user. This acts as a sub-function to the pscData.R function.
#'
#' @param DC a data cohort to be 'cleaned'
#' @return a dataset with missing data removed
pscData_miss <- function(DC){
  miss.id <- which(is.na(DC),arr.ind=T)[,1];miss.id
  if(length(miss.id)>0) {
    DC <- DC[-miss.id,]
    warning(paste(length(miss.id),"rows removed due to missing data in dataset"))
  }
  DC
}
