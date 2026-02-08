#' @title QID from DOI
#' @description simple converter from DOIs to QIDs (for items in wikidata)
#' @param DOI digital object identifiers submitted as strings
#' @return vector of QIDs corresponding to DOIs submitted
#' @export
qid_from_DOI <- function(DOI = '10.15347/WJM/2019.001'){
  article.qid <- qid_from_identifier(property = 'P356',value = DOI)
  return(article.qid)
}

#' @title QID from label name
#' @description simple converter from label names to QIDs (for items in wikidata).
#' Essentially a simplification of \code{find_item}
#' @param name name labels submitted as strings
#' @param limit if multiple QIDs match each submitted name, how many to return
#' @param format output format ('vector' to return a simple vector, or 'list' to return a nested list)
#' @return vector of QIDs corresponding to names submitted. Note: some names may return multiple QIDs.
#' @export
qid_from_name <- function(name   = "Thomas Shafee",
                          limit  = 100,
                          format = "vector"){
  qid_from_name_nest1 <- function(x){lapply(x,"[[","id")}
  item.qs  <- lapply(name,find_item, limit=limit)
  item.qid <- lapply(item.qs,qid_from_name_nest1)
  names(item.qid) <- name
  if(format=="vector"){item.qid <- unlist(item.qid)}
  if(format=="list")  {item.qid <- item.qid}
  return(item.qid)
}

#' @title QID from ORCID
#' @description simple converter from ORCIDs to QIDs (for items in wikidata)
#' @param ORCID digital object identifiers submitted as strings
#' @return vector of QIDs corresponding to ORCIDs submitted
#' @export
qid_from_ORCID <- function(ORCID = '0000-0002-2298-7593'){
  author.qid   <- qid_from_identifier(property = 'P496',value = ORCID)
  return(author.qid)
}
