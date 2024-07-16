#' @title Convert all subspecies into synonyms of the species
#' @description used in generating master lists
#' @param master List of names with a field named canonical
#' @param return_unmatched If the return values should be unmatched (orphan)
#'   subspecies records. Default: FALSE
#' @param verbose display process messages, Default: TRUE
#' @return Same list of names with id and accid fields added (or data updated
#' the fields exists) with all subspecies linked to the species names as
#' synonyms
#' @details While dealing with taxonomic names only at species level,
#' to take advantage of sub-specific names already available in the lists
#' are sometimes treated as synonyms of the names at species rank. To
#' convert all the subspecies names as synonyms this function is very handy.
#' This function will add id, accid and taxonrank columns ro return data if
#' missing from original data.
#' @family List functions
#' @examples
#' \donttest{
#' master <- data.frame("id" = c(1,2,3,4,5,6,7),
#'                     "canonical" = c("Hypochlorosis ancharia",
#'                                      "Hypochlorosis tenebrosa",
#'                                      "Pseudonotis humboldti",
#'                                      "Myrina ancharia ancharia",
#'                                      "Hypochlorosis ancharia tenebrosa",
#'                                      "Hypochlorosis ancharia obiana",
#'                                      "Hypochlorosis lorquinii"),
#'                      "family" = c("Lycaenidae", "Lycaenidae", "Lycaenidae",
#'                                   "Lycaenidae", "Lycaenidae", "Lycaenidae",
#'                                   "Lycaenidae"),
#'                      "accid" = c(0,1,1,0,0,0,0),
#'                      "source" = c("itis","itis","wiki","wiki","itis",
#'                                   "itis","itis"),
#'                      stringsAsFactors = FALSE)
#' 
#' synonymize_subspecies(master)
#' synonymize_subspecies(master,return_unmatched = TRUE)
#' }
#' @rdname synonymize_subspecies
#' @export
synonymize_subspecies <- function(master,
                                  return_unmatched=FALSE,
                                  verbose=TRUE){
  names(master) <- tolower(names(master))
  if("id" %!in% names(master)){
    master$id <- seq.int(nrow(master))
  }
  if("accid" %!in% names(master)){
    master$accid <- 0
  }
  if("taxonrank" %!in% names(master)){
    if(verbose){cat(paste("\nUpdating taxon ranks...\n"))
      pb = txtProgressBar(min = 0, max = nrow(master), initial = 0)
    }
    for(i in 1:nrow(master)){
      master$taxonrank[i] <- guess_taxo_rank(master$canonical[i])
      if(verbose){setTxtProgressBar(pb,i)}
    }
  }
  if(verbose){cat(paste("\nProcessing...\n"))
    pb = txtProgressBar(min = 0, max = nrow(master), initial = 0)
  }
  for(i in 1:nrow(master)){
    if(master$taxonrank[i]=="Subspecies"
       & master$accid[i]==0){
      spname <- paste(unlist(strsplit(master$canonical[i],
                                      split = "\\s+"))[1:2],collapse=" ")
      if(spname %in% master$canonical){
        master$accid[i] <- get_accid(master,spname)
        if(nrow(master[which(master$accid==master$id[i]),])>0){
          master$accid[which(master$accid==master$id[i])] <- master$accid[i]
        }
      }
    }
    if(verbose){setTxtProgressBar(pb,i)}
  }
  if(verbose){cat("\n")}
  remrec <- master[which(master$taxonrank=="Subspecies" & master$accid==0),]
  master <- master[which(master$id %!in% remrec$id),]
  if(return_unmatched){
    if(nrow(remrec)==0){
      cat("\nNO Orphan subspecies\n")
      return(NULL)
    } else {
      cat("\nReturning",nrow(remrec),"Orphan subspecies\n")
      return(remrec)
    }
  } else {
    if(nrow(remrec)>0){
      warning(paste(nrow(remrec),"Orphan subspecies"))
    }
    return(master)
  }
}
