#' @title Wikipedia list to taxo
#' @description Converts the output of \link{list_wiki_syn} function to
#' taxolist format of \link{taxotools} package
#' @param wikisyn Wikipedia synonyms list
#' @return taxolist
#' @details Output of \link{list_wiki_syn} function has different format
#' than taxolist. This function converts it making sure to add additional
#' fields and maintain the synonym linkages.
#' @family List functions
#' @examples
#' \donttest{
#'  wikilist <- list_wiki_syn("Abrothrix illutea")
#'  wiki2taxo(wikilist)
#' }
#' @rdname wiki2taxo
#' @export
wiki2taxo <- function(wikisyn){
  # Accepeted Names
  wikisyn <- as.data.frame(wikisyn)
  wikisyn <- wikisyn[which(wikisyn$Syn %!in% wikisyn$Name),]
  wikisyn <- wikisyn[!duplicated(paste(wikisyn$WikiName,wikisyn$Syn)),]
  canonical <- unique(wikisyn$Name)
  Id <- seq(1:length(canonical))
  AccId <- 0
  source <- "wiki"
  taxo <- as.data.frame(cbind(Id,canonical,AccId,source),stringsAsFactors = F)
  for(i in 1:nrow(taxo)){
    taxo$taxonlevel[i] <-  guess_taxo_rank(taxo$canonical[i])
  }
  # Synonyms
  id <- length(canonical) + 1
  for(i in 1:nrow(wikisyn)){
    canonical <- wikisyn$Syn[i]
    Id <-  id
    id <- id + 1
    AccId <- taxo[which(taxo$canonical==as.character(wikisyn$Name[i])),c("Id")]
    taxolevel <- guess_taxo_rank(as.character(wikisyn$Syn[i]))
    if(taxolevel=="Species" | taxolevel=="Subspecies"){
      rec <- data.frame("Id"=Id,"canonical"=canonical,
                        "AccId"=AccId,"source"=source,
                        "taxonlevel"=taxolevel)
      taxo <- rbind(taxo,rec)
    }
  }
  taxo$order <- ""
  taxo$family <- ""
  taxo <- melt_canonical(taxo,"canonical","genus","species","subspecies")
  names(taxo) <- tolower(names(taxo))
  return(taxo)
}