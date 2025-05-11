# This function rather clumsily extracts the covariate data from an nls object.
get_nls_data <- function(m){
  elements <- as.list(m[["m"]][["getEnv"]]())
  elements[["(weights)"]] <- NULL
  elements <- elements[unlist(lapply(elements, function(x) if(length(x) == 1) FALSE else TRUE))]
  return(as.data.frame(elements))
}
