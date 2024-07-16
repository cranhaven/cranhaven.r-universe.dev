#' @title Darwin Core to Taxolist format
#' @description Converts a Darwin Core name list to taxolist format
#' @param namelist names list in Darwin Core format
#' @param statuslist vector listing taxonomicStatus to be considered in
#' the namelist. If Default value is NA, automatically uses list of
#' \itemize{\item{Accepted} \item{Synonym} \item{Valid}
#' \item{heterotypic Synonym} \item{homotypic Synonym} \item{doubtful},
#' \item{proparte synonym}}
#' @param source source of the namelist i.e. Global Biodiversity Information
#'  Facility 'GBIF' or Integrated Taxonomic Information System 'ITIS'. 
#'  Default NA
#' @return names list is taxolist format
#' @details The name lists downloaded from 'GBIF' or 'ITIS' website in Darwin
#'  Core (DwC) format has all the required fields for taxolist. The list just
#'  needs to be converted to taxolist by renaming column names and and quality
#'  checked in terms of missing synonym to accepted name linkages at times.
#' @family List functions
#' @importFrom plyr rename
#' @importFrom stringr word str_remove
#' @examples
#' \donttest{
#' dwclist <- data.frame("taxonKey" = c("5129025","6224429","1896957"),
#'                       "scientificName" =  c("Charaxes solon Fabricius, 1793",
#'                                             "Papilio jason Linnaeus, 1767",
#'                                             "Charaxes jasius (Linnaeus, 1767)"),
#'                      "acceptedTaxonKey" = c("5129025","1896957","1896957"),
#'                      "acceptedScientificName" = c("Charaxes solon Fabricius, 1793",
#'                                                   "Charaxes jasius (Linnaeus, 1767)",
#'                                                   "Charaxes jasius (Linnaeus, 1767)"),
#'                      "taxonRank" = c("SPECIES","SPECIES","SPECIES"),
#'                      "taxonomicStatus" = c("ACCEPTED","SYNONYM","ACCEPTED"),
#'                      "family" = c("Nymphalidae","Nymphalidae","Nymphalidae"),
#'                      "order" = c("Lepidoptera","Lepidoptera","Lepidoptera"),
#'                      stringsAsFactors = FALSE)
#'                      
#' mytaxo <- DwC2taxo(dwclist)
#' }
#' @rdname DwC2taxo
#' @export
DwC2taxo <- function(namelist,
                     statuslist=NA,
                     source=NA){
  if(is.na(statuslist)){
    statuslist <- c("Accepted", "Synonym", "Valid", "heterotypicSynonym",
                    "homotypicSynonym", "doubtful", "proparte synonym")
  }
  statuslist <- toupper(statuslist)
  if("taxonRank" %in% names((namelist))){
    namelist <- namelist[which(toupper(namelist$taxonRank) == "SPECIES" |
                                 toupper(namelist$taxonRank) == "SUBSPECIES"),]
  } else {
    warning("taxonRank not found.")
    return(NULL)
  }
  if("taxonomicStatus" %in% names((namelist))){
    namelist <- namelist[which(toupper(namelist$taxonomicStatus) %in%
                                 statuslist),]
  }
  if(nrow(namelist)==0){
    message("taxonomicStatus does not have usable values")
    return(NULL)
  }
  if("taxonID" %in% names(namelist)){
    namelist <- plyr::rename(namelist,
                             replace = c("taxonID" = "id",
                                         "acceptedNameUsageID" = "accid",
                                         "specificEpithet" = "species",
                                         "infraspecificEpithet" = "subspecies",
                                         "scientificNameAuthorship" = "author",
                                         "taxonRank" = "taxonlevel"))
    namelist <- cast_canonical(namelist,
                               "canonical",
                               "genus",
                               "species",
                               "subspecies")
    namelist$accid[is.na(namelist$accid)] <- 0
    namelist1 <- namelist[which(namelist$accid!=0 & namelist$accid %!in%
                                  namelist$id),c("id", "order",
                                                 "family", "genus",
                                                 "species", "subspecies",
                                                 "author", "taxonlevel",
                                                 "accid", "canonical")]
    namelist <- namelist[which(namelist$accid==0 | namelist$accid %in%
                                 namelist$id),c("id", "order",
                                                "family", "genus",
                                                "species", "subspecies",
                                                "author", "taxonlevel",
                                                "accid", "canonical")]
    names(namelist) <- c("id", "order", "family", "genus", "species",
                         "subspecies", "author","taxonlevel", "accid",
                         "canonical")
  }
  if("taxonKey" %in% names(namelist)){
    namelist <- rename(namelist,
                       replace = c("taxonKey" = "id",
                                   "acceptedTaxonKey" = "accid",
                                   "taxonRank" = "taxonlevel"))
    namelist <- melt_scientificname(namelist,
                                    sciname = "acceptedScientificName",
                                    genus = "genus_a",
                                    species = "species_a",
                                    subspecies = "subspecies_a",
                                    author = "author_a")
    namelist <- cast_canonical(namelist,
                               "accepted",
                               "genus_a",
                               "species_a",
                               "subspecies_a")
    namelist$author_a <- trimws(str_remove(namelist$acceptedScientificName,
                                           namelist$accepted))
    
    namelist1 <- namelist[,c("accid", "order","family", "genus_a",
                             "species_a", "subspecies_a", "author_a",
                             "taxonlevel", "id", "accepted")]
    names(namelist1) <- c("id", "order", "family", "genus", "species",
                          "subspecies", "author", "taxonlevel", "accid",
                          "canonical")
    namelist1$accid <- 0
    namelist1 <- namelist1[!duplicated(namelist1$id),]
    namelist$accid[which(namelist$accid==namelist$id)] <- 0
    if("speciesKey" %in% names(namelist)){
      namelist2 <- namelist[which(namelist$accid!=0 &
                                    !is.na(namelist$speciesKey)),]
    } else {
      namelist2 <- namelist[which(namelist$accid!=0),]
    }
    if(nrow(namelist2)>0){
      namelist2 <- melt_scientificname(namelist2,
                                       sciname = "scientificName",
                                       genus = "genus_s",
                                       species = "species_s",
                                       subspecies = "subspecies_s",
                                       author = "author_s")
      namelist2 <- cast_canonical(namelist2,"synonym","genus_s",
                                  "species_s", "subspecies_s")
      namelist2$author_s <- trimws(str_remove(namelist2$scientificName,
                                              namelist2$synonym))
      
      namelist2 <- namelist2[which(namelist2$accid %in%
                                     namelist1$id),c("id", "order",
                                                     "family", "genus_s",
                                                     "species_s",
                                                     "subspecies_s", "author_s",
                                                     "taxonlevel", "accid",
                                                     "synonym")]
    } else {
      namelist2 <- NULL
    }
    if(!is.null(namelist2)){
      names(namelist2) <- c("id", "order", "family", "genus", "species",
                            "subspecies", "author", "taxonlevel", "accid",
                            "canonical")
      namelist <- rbind(namelist1,namelist2)
    } else {
      namelist <- namelist1
    }
  }
  namelist$source <- source
  return(namelist)
}