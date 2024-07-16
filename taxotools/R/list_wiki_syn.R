#' Get Wikipedia Synonyms for list of names
#'
#' Fetch Synonyms from Wikipedia and clean them for use
#'
#' @param namelist list of scientific names
#' @param verbose status output. Default TRUE
#' @family Wiki functions
#' @return a data frame containing names, synonyms and Canonical synonyms matched
#'    with is scientific name backbone taxonomy  
#'    \describe{ \item{Name}{: Scientific name}
#'    \item{WikiName}{: Wikipedia page name}
#'    \item{OrigSyn}{: Original synonym returned by Wikipedia}
#'  \item{Syn}{: Synonym in canonical form, matched with GBIF}}
#' @importFrom wikitaxa wt_wikipedia
#' @importFrom taxize gbif_parse
#' @examples
#' \donttest{
#'  list_wiki_syn("Abrothrix illutea")
#'  list_wiki_syn(c("Abditomys latidens", "Abeomelomys sevia",
#'                  "Abrocoma schistacea"))
#' }
#'
#' @export
list_wiki_syn <- function(namelist,verbose = TRUE){
  res <- NULL
  for(i in 1:length(namelist)){
    accname <- namelist[i]
    if(verbose){cat(paste("\n",i,accname," "))}
    wikisyn <- wikitaxa::wt_wikipedia(accname)$synonyms
    wikiacn <- wikitaxa::wt_wikipedia(accname)$classification[which(wt_wikipedia(namelist[i])$classification$rank=="binomial"),]$name
    if(!is.null(wikiacn) & !identical(wikiacn, character(0)) ){
      if(accname!= wikiacn){
        if(length(wikisyn)>0){
          wikisyn <- c(wikisyn,wikiacn)
        } else {
          wikisyn <- wikiacn
        }
      }
    }
    if(length(wikisyn)>0){
      wikisyn <- expand_name(accname,wikisyn)
      synlst <- NULL
      for(j in 1:length(wikisyn)){
        synrec <- data.frame("accname"=NA,"wikiacn"=NA,"syn_orig"=NA,"syn"=NA)
        rec <- taxize::gbif_parse(wikisyn[j])
        synrec$syn <- check_scientific(rec$canonicalname)
        if(is.null(synrec$syn)| is.na(synrec$syn)){
          synrec$syn <- NA
        }
        synrec$syn_orig <- as.character(wikisyn[j])
        if(is.null(wikiacn) | identical(wikiacn,character(0))){
          synrec$wikiacn <- NA
        } else {
          synrec$wikiacn <- wikiacn
        }
        synrec$accname <- accname
        synlst <- rbind(synlst,synrec)
        if(verbose){cat("+")}
      }
      recs <- synlst
    } else {
      recs <- NULL
    }
    res <- rbind(res,recs)
  }
  if(!is.null(res)){
    res <- as.data.frame(res)
    names(res) <- c("Name","WikiName","OrigSyn","Syn")
  }
  if(verbose){cat("\n")}
  return(res)
}