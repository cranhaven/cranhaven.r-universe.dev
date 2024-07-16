#' @title Taxolist to Synonym list
#' @description Converts a taxolist to Synonym list with Accepted Names and 
#'  Synonym columns format
#' @param taxolist taxolist
#' @param canonical names column name, Default: 'canonical'
#' @param synonym Synonym column name to be created, Default: 'synonym'
#' @param duplicate If true, duplicate entries are allowed in secondary field
#' @param sepchar Character separator between the data items. Default is comma
#' @return returns a synonym list  all the names in same column and
#' accepted names linked to synonyms with id and accid fields
#' @details Converts a taxolist to synonyms list 
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
#' mysynlst <- taxo2syn(mytaxo)
#'}
#' @rdname taxo2syn
#' @export
#' 
taxo2syn <- function(taxolist,
                     canonical = "canonical",
                     synonym = "synonym",
                     duplicate = FALSE,
                     sepchar = ","){
  if(is.null(taxolist)){
    return(NULL)
  }
  if(nrow(taxolist)<1){
    return(NULL)
  }
  synlist <- taxolist[which(taxolist$accid==0),]
  synlist$synonym <- NA
  synlist1 <- taxolist[which(taxolist$accid!=0),]
  if(nrow(synlist1)>0){
    synlist1$name__ <- synlist$canonical[match(synlist1$accid,synlist$id)]
    synlist1 <- rename_column(synlist1,"canonical","synonym")
    synlist1 <- rename_column(synlist1,"name__","canonical")
    synlist <- rbind(synlist,synlist1)
    synlist <- cast_cs_field(synlist,"canonical","synonym",
                             duplicate=duplicate,sepchar=sepchar)
  }
  return(synlist)
}
