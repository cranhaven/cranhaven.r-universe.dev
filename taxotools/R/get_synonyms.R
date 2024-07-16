#' @title get synonyms 
#' @description get all the synonyms from the master list for the names
#' in the checklist
#' @param master master list of names (taxolist)
#' @param checklist list of names to be processed (taxolist)
#' @param commasep return list should be comma separated list or each synonym on
#' its own row. Default false
#' @param verbose verbose output on the console
#' @return Data frame with names from the checklist and their synonyms present
#' in the master list
#' @importFrom plyr rbind.fill
#' @family List functions
#' @examples
#' \donttest{
#' master <- data.frame("id" = c(1,2,3,4,5),
#'                         "canonical" = c("Hypochlorosis ancharia",
#'                                         "Pseudonotis humboldti",
#'                                         "Myrina ancharia",
#'                                         "Hypochlorosis ancharia obiana",
#'                                         "Hypochlorosis lorquinii"),
#'                         "family" = c("Lycaenidae", "Lycaenidae", 
#'                                      "Lycaenidae", "Lycaenidae",
#'                                       "Lycaenidae"),
#'                         "accid" = c(0,1,1,0,0),
#'                         "source" = c("itis","wiki","wiki","itis",
#'                                      "itis"),
#'                         stringsAsFactors = FALSE)
#' checklist <- data.frame("id" = c(1,2,3),
#'                      "canonical" = c("Hypochlorosis ancharia",
#'                                      "Hypochlorosis tenebrosa",
#'                                      "Hypochlorosis ancharia tenebrosa"),
#'                      "family" = c("Lycaenidae", "Lycaenidae", "Lycaenidae"),
#'                      "accid" = c(0,1,0),
#'                      "source" = c("itis","itis","itis"),
#'                      stringsAsFactors = FALSE)
#' 
#' get_synonyms(master,checklist,commasep=FALSE)
#' get_synonyms(master,checklist,commasep=TRUE)
#' }
#' @rdname get_synonyms
#' @export
get_synonyms <- function(master = NULL,
                         checklist = NULL,
                         commasep=FALSE,
                         verbose = TRUE){
  if(is.null(master)){
    warning("master data missing")
    return(NULL)
  }
  if(is.null(checklist)){
    warning("checklist data missing")
    return(NULL)
  }
  master <- as.data.frame(master)
  checklist <- as.data.frame(checklist)
  addlist <- NULL
  names(master) <- tolower(names(master))
  names(checklist) <- tolower(names(checklist))
  if(!is.numeric(master$id)){
    master$id <- as.numeric(master$id)
  }
  idcount <- max(master$id) + 1
  checklist <- compact_ids(checklist,"id","accid",idcount,verbose)
  check_acc <- checklist[which(checklist$accid==0),]
  for(i in 1:nrow(check_acc)){
    if(verbose){cat(paste("\n",i))}
    recset <- get_id_recs(checklist,check_acc$id[i])
    if(!is.null(recset)){
      accid_set <- c()
      for(j in 1:nrow(recset)){
        if(recset$canonical[j] %in% master$canonical) {
          set_accid <- get_accid(master,as.character(recset$canonical[j]),
                                 verbose)
          accid_set <- c(accid_set,set_accid)
        }
      }
      accid_set <- unique(accid_set)
      if(length(accid_set)==1){
        for(k in 1:length(accid_set)){
          if(verbose){cat("|")}
          addrec <- master[which(accid_set[k] == master$accid),]
          addrecm <- master[which(accid_set[k] == master$id),]
          addlist <- rbind(addlist,addrec,addrecm)
          if(verbose){cat("+")}
        }
      }
    }
  }
  if(verbose){cat("\n")}
  addmast <- master[which(master$id %in% addlist$accid),]
  addlist <- rbind.fill(addlist,addmast)
  retval <- taxo2syn(addlist)
  retval <- retval[,c("canonical","synonym")]
  
  if(!commasep & nrow(retval)>0){
    retval <- melt_cs_field(retval,"synonym")
  }
  if(nrow(retval)>0){
    retval <- retval[which(!is.na(retval$synonym)),]
  }
  if(nrow(retval)<1){
    retval <- NULL
  }
  return(retval)
}