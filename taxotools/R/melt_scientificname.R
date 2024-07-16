#' @title Melt scientific name into fields
#' @description Parse scientific names into Genus, Species, Subspecies,
#'  Author etc.
#' @param dat data frame containing scientific names
#' @param sciname column name for scientific names, Default: ''
#' @param genus column name for genus, Default: 'genus'
#' @param subgenus column name for subgenus, Default: 'subgenus'
#' @param species column name for species, Default: 'species'
#' @param subspecies column name for subspecies, Default: 'subspecies'
#' @param author column name for author, Default: 'author'
#' @param verbose verbose output, Default: FALSE
#' @return data frame with additional columns for taxonomic fields
#' @details Helpful function to break down Scientific names into Genus, Subgenus,
#' species, Subspecies, Author so that the names can be constructed into canonical
#'  names for matching
#' @family Name functions
#' @examples
#' \donttest{
#' mylist <- data.frame("id"= c(11,12,13,14,15,16,17,18,19),
#'                      "scname" = c("Hypochlorosis ancharia (Hewitson, 1869)",
#'                                   "Hypochlorosis ancharia ssp. ancharia (Hewitson, 1869)",
#'                                   "Hypochlorosis ancharia ssp. humboldti Druce, 1894",
#'                                   "Myrina lorquinii C. & R. Felder, 1865",
#'                                   "Hypochlorosis ancharia tenebrosa Rothschild, 1915",
#'                                   "Hypochlorosis ancharia  tenebrosa Rothschild, 1915",
#'                                   "Hypochlorosis (Pseudonotis) metilia Fruhstorfer, 1908",
#'                                   "Seuku emlongi (Domning et al., 1986)",
#'                                   "Sithon lorquinii"),
#'                      stringsAsFactors = FALSE)
#' 
#' melt_scientificname(mylist, sciname="scname", genus="genus",
#'                     subgenus="subgenus", species="species",subspecies="subspecies",
#'                     author="author")
#' }
#' @rdname melt_scientificname
#' @export
melt_scientificname <- function(dat,sciname="",genus="genus", 
                                subgenus="subgenus", species="species",
                                subspecies="subspecies", author="author",
                                verbose=FALSE){
  if(is.null(dat) | nrow(dat)==0){
    return(NULL)
  }
  spvar <- c("sp","sp.","species", "verified")
  authvar <- c("ex","ex.","van", "de", "von", "et", "et.", "al", "al.",
               "da", "du", "la", "del", "der", "den", "y", "al.,")
  newdat <- as.data.frame(dat)
  if(genus==""){
    return(NULL)
  } else {
    newdat <- rename_column(newdat,genus,"genus_",silent=TRUE)
    newdat$genus_ <- NA
  }
  if(subgenus==""){
    return(NULL)
  } else {
    newdat <- rename_column(newdat,subgenus,"subgenus_",silent=TRUE)
    newdat$subgenus_ <- NA
  }
  if(species==""){
    return(NULL)
  } else {
    newdat <- rename_column(newdat,species,"species_",silent=TRUE)
    newdat$species_ <- NA
  }
  if(subspecies!=""){
    newdat <- rename_column(newdat,subspecies,"subspecies_",silent=TRUE)
    newdat$subspecies_ <- NA
  }
  if(sciname!=""){
    newdat <- rename_column(newdat,sciname,"sciname")
    newdat$sciname <- as.character(newdat$sciname)
  }
  if(author!=""){
    newdat <- rename_column(newdat,author,"author_",silent=TRUE)
    newdat$author_ <- ""
  }
  pb = txtProgressBar(min = 0, max = nrow(newdat), initial = 0)
  for(i in 1:nrow(newdat)){
    if(!is.empty(newdat$sciname[i])){
      words <- strsplit(newdat$sciname[i]," ")[[1]]
      cwd <- 0
      while (length(words)>0) {
        cwd <- cwd + 1
        # Genus
        if(cwd==1){
          if(isproper(words[1])){
            newdat$genus_[i] <- words[1]
            words <- words[-1]
            next
          } else {
            cat(paste("\n Name: ",newdat$sciname[i]," is not well formated"))
            words <- words[-1]
            next
          }
        }
        # Subgenus in ()
        if(startsWith(words[1],"(")){
          if(cwd==2){
            newdat$subgenus_[i] <- words[1]
            words <- words[-1]
            cwd <- cwd - 1
            next
          } else {
            words[1] <- substr(words[1],2,nchar(words[1]))
            cwd <- cwd - 1
            next
          }
        }
        if(endsWith(words[1],")")){
          if(cwd>2) {
            words[1] <- substr(words[1],1,nchar(words[1])-1)
            cwd <- cwd - 1
            next
          }
        }

        # species
        if(!isproper(words[1]) & cwd==2){
          if(words[1] %!in% spvar){
            newdat$species_[i] <- words[1]
            words <- words[-1]
            next
          }
        }
        # subspecies
        if(!isproper(words[1]) & cwd>2){
          if(words[1] %in% authvar){
            newdat$author_[i] <- paste(newdat$author_[i],words[1])
            words <- words[-1]
            next
          }
          if(words[1] %!in% spvar){
            if(islower(words[1])){
              newdat$subspecies_[i] <- words[1]
              words <- words[-1]
              next
            }
          }
        }
        # author
        if(isproper(words[1])){
          newdat$author_[i] <- paste(newdat$author_[i],words[1])
          words <- words[-1]
          next
        } else {
          words <- words[-1]
          next
        }
      }
    }
    setTxtProgressBar(pb,i)
  }
  cat("\n")
  newdat <- rename_column(newdat,"genus_",genus)
  newdat <- rename_column(newdat,"species_",species)
  if(subspecies!=""){
    newdat <- rename_column(newdat,"subspecies_",subspecies)
  }
  if(subgenus!=""){
    newdat <- rename_column(newdat,"subgenus_",subgenus)
  }
  if(author!=""){
    newdat$author_ <- trimws(newdat$author_)
    newdat <- rename_column(newdat,"author_",author)
  }
  newdat <- rename_column(newdat,"sciname",sciname)
  return(newdat)
}
