
#' Centering on grand-means
#'
#' This function allows you to center on grand-means.
#' @param var name of variable to be centered
#' @return A column in your dataframe (with grand-mean centered data)
#' @keywords centering
#' @export
#' @examples
#' \dontrun{data$centeredVAR<-gcenter(data$var)}


gcenter<-function(var){
  centered<- var-(mean(var, na.rm=T))
  return(centered)
}



