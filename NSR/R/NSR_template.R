#'Make a template for an NSR query
#'
#'NSR_template builds a template that can be populated to submit an NSR query.
#' @param nrow The number of rows to include in the template
#' @return Template data.frame that can be populated and then used in NSR queries.
#' @export
#' @examples \dontrun{
#' 
#' template<-NSR_template(nrow = 2)
#' template$genus<-"Acer"
#' template$species<-c("Acer rubrum", "Acer saccharum")
#' template$country<-"Canada"
#' template$user_id<-1:2
#' results <- NSR(occurrence_dataframe = template)
#' 
#' }
NSR_template <- function(nrow=1){
  
  template<-matrix(nrow = nrow, ncol= 5)
  template<-as.data.frame(template)
  colnames(template)<-c("species","country","state_province","county_parish","user_id")      
  return(template)  
  
}


