#' @title Synonym list to taxolist
#' @description Converts a Synonym list with Accepted Names and Synonym columns
#'  to taxolist format
#' @param synlist Synonym list with Accepted name (canonical) and Synonym columns
#' @param canonical Accepted names column name, Default: 'canonical'
#' @param synonym Synonym column name , Default: 'synonym'
#' @return returns a data frame in taxolist format with all the names in 
#'  canonical column and accepted names linked to synonyms using id and accid
#'  fields. Order, family and (guessed) taxonlevel are added if missing. Genus,
#'  species and subspecies fields are added by melting the canonical names.
#' @details Converts a synonyms list to taxolist format. If order and family
#'  fields are present, then they are carried forward else NAs are populated.
#'  Duplicate synomyms with same source are removed but with different sources 
#'  are retained.
#' @family List functions
#' @examples
#' \donttest{
#'  synlist <- data.frame("id" = c(1,2,3),
#'                        "canonical" = c("Hypochlorosis ancharia",
#'                                        "Hypochlorosis ancharia",
#'                                        "Hypochlorosis ancharia"),
#'                        "synonym" = c( "Hypochlorosis tenebrosa",
#'                                       "Pseudonotis humboldti",
#'                                       "Myrina ancharia"),
#'                        "family" = c("Lycaenidae", "Lycaenidae", 
#'                                     "Lycaenidae"),
#'                        "source" = c("itis","wiki","wiki"),
#'                        stringsAsFactors = FALSE)
#'                        
#'  mytaxo <- syn2taxo(synlist)
#' }
#' @rdname syn2taxo
#' @export
syn2taxo <- function(synlist,
                     canonical="canonical",
                     synonym="synonym"){
  synlist <- rename_column(synlist,canonical,"Name_")
  synlist <- rename_column(synlist,synonym,"Syn_")
  # Accepeted Names
  synlist <- as.data.frame(synlist)
  acclist <- synlist[!duplicated(synlist$Name_),]
  canonical <- acclist$Name_
  source <- acclist$source
  Id <- seq(1:length(canonical))
  AccId <- 0
  order <- NA
  family <- NA
  taxo <- as.data.frame(cbind(Id,canonical,AccId,order,family,
                              source),stringsAsFactors = F)
  for(i in 1:nrow(taxo)){
    taxo$taxonlevel[i] <-  guess_taxo_rank(taxo$canonical[i])
  }
  # Synonyms
  id <- length(canonical) + 1
  pb = txtProgressBar(min = 0, max = nrow(synlist), initial = 0)
  if("source" %in% names(synlist)){
    synlist <- synlist[!duplicated(cbind(synlist$Name_,synlist$Syn_,
                                         synlist$source)),]
  } else {
    synlist <- synlist[!duplicated(cbind(synlist$Name_,synlist$Syn_)),]
  }
  for(i in 1:nrow(synlist)){
    canonical <- synlist$Syn_[i]
    if("source" %in% names(synlist)){
      source <- synlist$source[i]
    }
    if("order" %in% names(synlist)){
      order <- synlist$order[i]
    }
    if("family" %in% names(synlist)){
      family <- synlist$family[i]
    }
    Id <-  id
    id <- id + 1
    AccId <- taxo[which(taxo$canonical==as.character(synlist$Name_[i])),c("Id")]
    taxolevel <- guess_taxo_rank(as.character(synlist$Syn_[i]))
    if(taxolevel=="Species" | taxolevel=="Subspecies"){
      rec <- data.frame("Id"=Id,"canonical"=canonical,
                        "AccId"=AccId,"order"=order,
                        "family"=family,"source"=source,
                        "taxonlevel"=taxolevel)
      taxo <- rbind(taxo,rec)
    }
    setTxtProgressBar(pb,i)
  }
  cat("\n")
  taxo <- melt_canonical(taxo,"canonical","genus","species","subspecies")
  taxo$Id <- as.numeric(taxo$Id)
  taxo$AccId <- as.numeric(taxo$AccId)
  names(taxo) <- tolower(names(taxo))
  return(taxo)
}