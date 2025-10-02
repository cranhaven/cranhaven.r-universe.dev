#' Function to select the desired methods in the study
#'
#' @param object as output of 'prediction_errors()' function
#' @return Returns error comparison for selected forecasting methods
#' @export
#' @examples
#' \dontrun{
#' a <- prediction_errors(data = nottem)
#' choose_(object = a)
#' }

choose_ <- function(object)
{
  a <- object
  df <- data.frame(1:length(a@parameters$MethodName), a@parameters$MethodName)
  names(df) <- c("Indices", "Methods")
  df <- t(df)
  df
  writeLines(c("Following are the methods attached with the object:"))
  print(df)
  var <- readline(prompt="Enter the indices of methods to remove:")
  var <- as.numeric(unlist(strsplit(var, ",")))

  a@output$Error_Parameters <- a@output$Error_Parameters[-var,]
  #a@output$DIRREC_ERROR_PARAMETERS <- a@output$DIRREC_ERROR_PARAMETERS[-var,]
  a@output$Predicted_Values <- a@output$Predicted_Values[-(var+1),]
  #a@output$DIRREC_Predicted_Values <- a@output$DIRREC_Predicted_Values[-(var+1),]

  a@parameters$Method <- a@parameters$Method[-var]
  a@parameters$MethodName <- a@parameters$MethodName[-var]
  return(a)
}
