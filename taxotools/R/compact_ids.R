#' @title compact id numbers
#' @description Compacting and converting the id values to numeric if required
#' to make sure dependent functions work well
#' @param dat taxonomic list in a data frame with id and accid columns
#' @param id column name for 'id'. Default 'id' 
#' @param accid column name for 'accid'. Default 'accid'
#' @param startid starting id number for the list. Default 1
#' @param verbose verbose output on the console
#' @return returns data frame 
#' @details Helper function to make sure values for ids are in right format and
#' are compact
#' @family List functions
#' @examples
#' \donttest{
#' mylist <- data.frame("id" = c("1","2","3","4","5"),
#'                      "canonical" = c("Hypochlorosis ancharia",
#'                                      "Pseudonotis humboldti",
#'                                      "Myrina ancharia",
#'                                      "Hypochlorosis ancharia obiana",
#'                                      "Hypochlorosis lorquinii"),
#'                      "family" = c("Lycaenidae", "Lycaenidae", 
#'                                   "Lycaenidae", "Lycaenidae",
#'                                   "Lycaenidae"),
#'                      "accid" = c("0","1","1","0","0"),
#'                      "source" = c("itis","wiki","wiki","itis",
#'                                   "itis"),
#'                      stringsAsFactors = FALSE)
#' 
#' mylist_c <- compact_ids(mylist)
#' 
#' mylist_c <- compact_ids(mylist,startid=1001)
#' 
#' mylist <- data.frame("id" = c(11,12,13,14,15),
#'                      "canonical" = c("Hypochlorosis ancharia",
#'                                      "Pseudonotis humboldti",
#'                                      "Myrina ancharia",
#'                                      "Hypochlorosis ancharia obiana",
#'                                      "Hypochlorosis lorquinii"),
#'                      "family" = c("Lycaenidae", "Lycaenidae", 
#'                                   "Lycaenidae", "Lycaenidae",
#'                                   "Lycaenidae"),
#'                      "accid" = c(0,11,11,0,0),
#'                      "source" = c("itis","wiki","wiki","itis",
#'                                   "itis"),
#'                      stringsAsFactors = FALSE)
#'                      
#' mylist_c <- compact_ids(mylist)
#' }
#' @rdname compact_ids
#' @export
compact_ids <- function(dat,id="id",accid="accid",
                        startid=1,verbose=TRUE){
  if(is.null(dat)){
    return(NULL)
  }
  if(id %!in% names(dat)){
    message("id field missing. Returning NULL")
    return(NULL)
  }
  if(accid %!in% names(dat)){
    message("accid field missing. Returning NULL")
    return(NULL)
  }
  dat <- rename_column(dat,id,"id_")
  dat <- rename_column(dat,accid,"accid_")
  dat$id <- seq(startid,(nrow(dat)+startid-1))
  dat$accid <- 0
  if(verbose){pb = txtProgressBar(min = 0, max = nrow(dat), initial = 0)}
  for(i in 1:nrow(dat)){
    if (!(is.na(dat$accid_[i]) | dat$accid_[i]==0 |
          dat$accid_[i]=="0" | dat$accid_[i]=="")){
      dat$accid[i] <- dat$id[which(dat$id_==dat$accid_[i])]
    }    
    if(verbose){setTxtProgressBar(pb,i)}
  }
  if(verbose){cat("\n")}
  dat$id_ <- dat$id
  dat$accid_ <- dat$accid
  dat <- rename_column(dat,"id_",id)
  dat <- rename_column(dat,"accid_",accid)
  dat[,c("id","accid")] <- list(NULL)
  return(dat)
}
