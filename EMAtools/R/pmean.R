
#' Centering on person-means
#'
#' This function allows you calculate person-level means. This will create a level-2 variable that can be used in tandem with person-centered means. This is useful if you are interested in both the within-person and between-person effects.
#' @param ID name of ID variable
#' @param var name of variable to be centered
#' @return A column in your dataframe (with person-level means)
#' @keywords centering
#' @export
#' @examples
#' \dontrun{data$centeredVAR<-pmean(data$ID,data$var)}


pmean<-function(ID,var){
  centered<- ave(var, ID,FUN=function(x) mean(x, na.rm=T))
  return(centered)
}



