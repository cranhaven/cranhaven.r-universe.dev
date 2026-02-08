#' @title Cast scientific name using taxonomic fields
#' @description Combine scientific names using Genus, Species, Subspecies,
#'  Author etc.
#' @param dat data frame containing taxonomic data
#' @param sciname column name for scientific names, Default: 'scientificname'
#' @param genus column name for genus, Default: 'genus'
#' @param subgenus column name for subgenus, Default: ''
#' @param species column name for species, Default: 'species'
#' @param subspecies column name for subspecies, Default: 'subspecies'
#' @param author column name for author, Default: 'author'
#' @param verbose verbose output, Default: FALSE
#' @return data frame with additional columns for taxonomic fields
#' @details Helpful function to break down Scientific names into Genus, Subgenus,
#' species, Subspecies, Author so that the names can be constructed into canonical
#'  names for matching
#' @family Name functions
#' @rdname cast_scientificname
#' @examples
#' \donttest{
#' mylist <- data.frame("id" = c(11,12,13,14,15,16,17,18,19),
#'                      "genus" = c("Hypochlorosis","Hypochlorosis","Hypochlorosis",
#'                                  "Myrina","Hypochlorosis","Hypochlorosis",
#'                                  "Hypochlorosis","Seuku","Sithon"),
#'                      "subgenus" = c("","","","","","","(Pseudonotis)","",""),
#'                      "species" = c("ancharia","ancharia","ancharia",
#'                                    "lorquinii","ancharia","ancharia",
#'                                    "metilia","emlongi","lorquinii"),
#'                      "subspecies" = c("","ancharia","humboldti",
#'                                       "","tenebrosa"," tenebrosa",
#'                                       "","",""),
#'                      "author" = c("(Hewitson, 1869)","(Hewitson, 1869)","Druce, 1894",
#'                                   "C. & R. Felder, 1865","Rothschild, 1915",
#'                                   "Rothschild, 1915","Fruhstorfer, 1908",
#'                                   "(Domning et al., 1986)",""),
#'                      stringsAsFactors = FALSE)
#' 
#' cast_scientificname(mylist,genus = "genus", subgenus = "subgenus",
#'                     species = "species", subspecies = "subspecies",
#'                     author = "author")
#' }
#' @export
cast_scientificname <- function(dat=NULL,sciname="scientificname", genus="", 
                                subgenus="", species="",
                                subspecies="", author="", 
                                verbose=FALSE){
  if(is.null(dat) ){
    warning("No data supplied to process. Nothing to do...")
    return(NULL)  
  }
  newdat <- as.data.frame(dat)
  newdat$sciname_ <- NA
  if(is.empty(sciname)){
    warning("sciname not provided. Using default scientificname")
    sciname <- "scientificname"
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
  if(subgenus!=""){
    newdat <- rename_column(newdat,subgenus,"subgenus_")
  } else {
    warning("subgenus field not specified. Assuming empty")
    newdat$subgenus_ <- NA
  }
  if(subspecies!=""){
    newdat <- rename_column(newdat,subspecies,"subspecies_")
  } else {
    warning("subspecies field not specified. Assuming empty")
    newdat$subspecies_ <- NA
  }
  if(author!=""){
    newdat <- rename_column(newdat,author,"author_")
  } else {
    warning("author field not specified. Assuming empty")
    newdat$author_ <- NA
  }
  if(verbose){pb = txtProgressBar(min = 0, max = nrow(newdat), initial = 0)}
  for(i in 1:nrow(newdat)){
    if(!is.empty(newdat$genus_[i])){
      scn <- toproper(newdat$genus_[i])
    }
    if(!is.empty(newdat$subgenus_[i])){
      scn <- paste(scn," (",toproper(newdat$genus_[i]),") ",sep = "")
    }
    if(!is.empty(newdat$species_[i])){
      scn <- paste(scn,newdat$species_[i])
    }
    if(!is.empty(newdat$subspecies_[i])){
      scn <- paste(scn,newdat$subspecies_[i])
    }
    if(!is.empty(newdat$author_[i])){
      scn <- paste(scn,trimws(newdat$author_[i]))
    }
    newdat$sciname_[i] <- scn
    if(verbose){setTxtProgressBar(pb,i)}
  }
  if(verbose){cat("\n")}
  newdat <- rename_column(newdat,"genus_",genus)
  newdat <- rename_column(newdat,"species_",species)
  if(subgenus!=""){
    newdat <- rename_column(newdat,"subgenus_",subgenus)
  } else {
    newdat <- newdat[ , !(names(newdat) %in% c("subgenus_"))]
  }
  if(subspecies!=""){
    newdat <- rename_column(newdat,"subspecies_",subspecies)
  } else {
    newdat <- newdat[ , !(names(newdat) %in% c("subspecies_"))]
  }
  if(author!=""){
    newdat <- rename_column(newdat,"author_",author)
  } else {
    newdat <- newdat[ , !(names(newdat) %in% c("author_"))]
  }
  if((sciname == "scientificname") & ("scientificname" %in% names(newdat))){
    newdat$sciname <- newdat$sciname_
    newdat <- newdat[ , !(names(newdat) %in% c("sciname_"))]
  } else {
    if(sciname %in% names(newdat)){
      newdat <- newdat[ , !(names(newdat) %in% sciname)]
    }
    newdat <- rename_column(newdat,"sciname_",sciname)
  }
  return(newdat)
}
