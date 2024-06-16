
#' Centering on person-means
#'
#' This function allows you to center on person-means (also called "centering within clusters")
#' @param ID name of ID variable
#' @param var name of variable to be centered
#' @return A column in your dataframe (with person-centered data)
#' @keywords centering
#' @export
#' @examples
#' \dontrun{data$centeredVAR<-pcenter(data$ID,data$var)}


pcenter<-function(ID,var){
  centered<- var-ave(var, ID,FUN=function(x) mean(x, na.rm=T))
  return(centered)
}



