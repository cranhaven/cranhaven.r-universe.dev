#'Check the native status for plant species in a political region
#'
#'NSR_simple returns information on native status for species within a political region.
#' @param species A single species or a vector of species, with genus and specific epithet separated by a space.
#' @param country A single country or a vector of countries.  If a vector, length must equal length of species vector.
#' @param state_province A single state/province or a vector of states.  If a vector, length must equal length of species vector.
#' @param county_parish A single county/parish or a vector of counties.  If a vector, length must equal length of species vector.
#' @param ... Additional arguments passed to internal functions.
#' @return Dataframe containing NSR results.
#' @export
#' @examples \dontrun{
#' 
#' results <- NSR_simple(species = "Acer rubrum",
#'            country = "Canada",state_province = "Ontario")
#' 
#' results <- NSR_simple(species = c("Acer rubrum", "Aspen tremuloides") , 
#'            country = c("Canada","Canada"),state_province = c("Ontario","Ontario"))
#' 
#' }
NSR_simple <- function(species=NULL, country=NULL, state_province=NULL,county_parish=NULL,...){
  
  #Check input for odd stuff
  
  if(length(species) != length(country)){stop("Country and species vectors should be the same length.")}
  
  if(!is.null(state_province)){
    if(length(species) != length(state_province)){stop("State and species vectors should be the same length.")}  
    
  }
  
  if(!is.null(county_parish)){
    if(length(species) != length(county_parish)){stop("County and species vectors should be the same length.")}  
    
  }

  #Make template
  template <- NSR_template(nrow = length(species))
  
  
  #Populate fields as needed
  
  #Species
  template$species <- species

    #Country
      if(!is.null(country)){
        template$country <- country  
      }
  
  #State
      if(!is.null(state_province)){
          template$state_province <- state_province  
      }
  
  #County
  if(!is.null(county_parish)){
    template$county_parish <- county_parish  
  }
  
  
  return(NSR(template,...))
  
  
}


