#' Construct canonical names
#'
#' Construct canonical names using Genus, Species and Subspecies fields. At 
#' times due to spaces or NAs in the data fields, it makes it tricky to generate
#' canonical names.
#'
#' @param dat data frame containing taxonomic list
#' @param canonical field name for canonical names. Default 'canonical'
#' @param genus field name for Genus field
#' @param species field name for Species field
#' @param subspecies field name for Subspecies field
#' @param verbose verbose output, Default: FALSE
#' @family Name functions
#' @return a data frame containing Canonical names field added or repopulated using
#'     filed names for Genus, Species and Subspecies specified in parameters
#' @examples
#' \donttest{
#' mylist <- data.frame("genus" = c("Acodon", "Akodon", "Abrothrix", "Abeomelomys"),
#'                      "species" = c("jelskii","longipilis","longipilis", "sevia"),
#'                      "subspecies" = c("pyrrhotis","castaneus","", NA))
#' cast_canonical(mylist,"canonical","genus","species","subspecies")
#' }
#' @export
cast_canonical <- function(dat,canonical="canonical",genus="",
                           species="",subspecies="",verbose=FALSE){
  if(is.null(dat) | nrow(dat)==0 ){
    return(NULL)  
  }
  newdat <- as.data.frame(dat)
  newdat$canonical_ <- NA
  if(is.empty(canonical)){
    warning("Canonical Empty")
    canonical <- "canonical"
  }
  if(genus==""){
    warning("genus field not specified")
    return(NULL)
  } else {
    newdat <- rename_column(newdat,genus,"genus_")
  }
  if(species==""){
    warning("species field not specified")
    return(NULL)
  } else {
    newdat <- rename_column(newdat,species,"species_")
  }
  if(subspecies!=""){
    newdat <- rename_column(newdat,subspecies,"subspecies_")
  } else {
    warning("subspecies field not specified. Assuming empty")
    newdat$subspecies_ <- NA
  }
  if(verbose){pb = txtProgressBar(min = 0, max = nrow(newdat), initial = 0)}
  for(i in 1:nrow(newdat)){
    cano <- NA
    if(!is.empty(newdat$genus_[i])){
      cano <- newdat$genus_[i]
    }
    if(!is.empty(newdat$species_[i]) &
       !is.empty(cano)){
      cano <- paste(cano,newdat$species_[i])
    }
    if(subspecies!=""){
      if(!is.empty(newdat$subspecies_[i])&
         !is.empty(cano)){
        cano <- paste(cano,newdat$subspecies_[i])
      }
    }
    newdat$canonical_[i] <- toproper(cano)
    if(verbose){setTxtProgressBar(pb,i)}
  }
  if(verbose){cat("\n")}
  newdat <- rename_column(newdat,"genus_",genus)
  newdat <- rename_column(newdat,"species_",species)
  if(subspecies!=""){
    newdat <- rename_column(newdat,"subspecies_",subspecies)
  } else {
    newdat <- newdat[ , !(names(newdat) %in% c("subspecies_"))]
  }
  if((canonical == "canonical") & ("canonical" %in% names(newdat))){
    newdat$canonical <- newdat$canonical_
    newdat <- newdat[ , !(names(newdat) %in% c("canonical_"))]
  } else {
    if(canonical %in% names(newdat)){
      newdat <- newdat[ , !(names(newdat) %in% canonical)]
    }
    newdat <- rename_column(newdat,"canonical_",canonical)
  }
  return(newdat)
}
