#' @title Taxolist to Darwin Core (DwC)
#' @description Converts a taxolist to Darwin Core format
#' @param taxolist taxolist
#' @param verbose verbose output, Default: TRUE
#' @return returns a taxonomic list in DwC format
#' @details Converts a taxolist to Darwin Core format
#' @family List functions
#' @examples
#' \donttest{
#'mytaxo <- data.frame("id" = c(1,2,3,4,5,6,7),
#'                     "canonical" = c("Hypochlorosis ancharia",
#'                                     "Hypochlorosis tenebrosa",
#'                                     "Pseudonotis humboldti",
#'                                     "Myrina ancharia",
#'                                     "Hypochlorosis ancharia tenebrosa",
#'                                     "Hypochlorosis ancharia obiana",
#'                                     "Hypochlorosis lorquinii"),
#'                      "family" = c("Lycaenidae", "Lycaenidae", "Lycaenidae",
#'                                   "Lycaenidae", "Lycaenidae", "Lycaenidae",
#'                                   "Lycaenidae"),
#'                     "accid" = c(0,1,1,1,0,0,0),
#'                     "source" = c("itis","itis","wiki","wiki","itis",
#'                                  "itis","itis"),
#'                     stringsAsFactors = FALSE)
#' mysynlst <- taxo2DwC(mytaxo)
#'}
#' @rdname taxo2DwC
#' @export
#' 
taxo2DwC <- function(taxolist,
                     verbose=TRUE){
  if(is.null(taxolist)){
    return(NULL)
  }
  if(nrow(taxolist)<1){
    return(NULL)
  }
  if("canonical" %!in% names(taxolist)){
    if("genus" %!in% names(taxolist) | "species" %!in% names(taxolist)){
      stop("Column names are not in standard format or missing some columns")
    } else {
      if(verbose){cat(paste("\nUpdating canonical names...\n"))}
      taxolist <- cast_canonical(taxolist,
                                 canonical = "canonical",
                                 genus = "genus",
                                 species = "species",
                                 subspecies = "subspecies",
                                 verbose=verbose)
    }
  }
  if("genus" %!in% names(taxolist)){
    if("canonical" %!in% names(taxolist)){
      stop("Column names are not in standard format or missing some columns")
    } else {
      if(verbose){cat(paste("\nUpdating genus, species, subspecies...\n"))}
      taxolist <- melt_canonical(taxolist,
                                 canonical = "canonical",
                                 genus = "genus",
                                 species = "species",
                                 subspecies = "subspecies",
                                 verbose=verbose)
    }
  }
  if("taxonlevel" %!in% names(taxolist)){
    taxolist$taxonlevel <- as.character(lapply(taxolist$canonical, 
                                               guess_taxo_rank))
  }
  if("id" %!in% names(taxolist)){
    taxolist$id <- seq(1:nrow(taxolist))
    warning("Column 'id' was missing. Assigning ids to each name")
  }
  if("accid" %!in% names(taxolist)){
    taxolist$accid <- 0L
    warning("Column 'accid' was missing. Assuming all names are accepted names")
  }
  taxolist <- rename_column(taxolist,"id","taxonKey")
  taxolist <- rename_column(taxolist,"canonical","scientificName")
  taxolist <- rename_column(taxolist,"taxonlevel","taxonRank")
  taxolist <- rename_column(taxolist,"species","specificEpithet")
  taxolist <- rename_column(taxolist,"subspecies",
                            "infraspecificEpithetProperty")
  # Accepted names
  taxo_ac <- taxolist[which(taxolist$accid==0),]
  taxo_ac$acceptedTaxonKey <- taxo_ac$taxonKey
  taxo_ac$acceptedScientificName <- taxo_ac$scientificName
  taxo_ac$taxonomicStatus <- "Valid"
  # Synonyms
  taxo_syn <- taxolist[which(taxolist$accid!=0),]
  if(nrow(taxo_syn)>0){
    taxo_syn$acceptedTaxonKey <- taxo_syn$accid
    taxo_syn$taxonomicStatus <- "Synonym"
    taxo_syn$acceptedScientificName <- taxo_ac$scientificName[match(taxo_syn$acceptedTaxonKey,taxo_ac$taxonKey)]
    taxo_ac <- rbind(taxo_ac,taxo_syn)
  }
  taxo_ac <- taxo_ac[,!names(taxo_ac) %in% c("accid")]
  return(taxo_ac)
}

