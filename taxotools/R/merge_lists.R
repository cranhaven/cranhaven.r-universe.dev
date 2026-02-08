#' @title merge two lists of names
#' @description Useful in generating a master list of names from multiple
#' sources
#' @param master master list of names
#' @param checklist list to be merged
#' @param output data returned by the function, one of the five options all, 
#' onlyadd, add, merged, new or multi. Default all
#' @param verbose verbose output on the console
#' @return Data frame with addition column merge_tag. The merge_tag contains 
#' four possible values. \describe{\item{orig - }{names in the master}\item{add -
#'  }{ checklist names that matched using synonym linkages including direct 
#'  matches} \item{new - }{checklist names that did NOT match with master.
#'  Potentially new taxa} \item{multi -}{taxon from checklist for which two
#'  synonyms matched with two different accepted names in master}}
#' @details Matches names is checklist with names on master and returns 
#' following data:
#' \describe{\item{all }{= orig + add + new + multi: all the data} \item{onlyadd
#'  }{= add : returns records from checklist that match with master}
#' \item{add }{= orig + add : returns all records from master + matched records
#'  from checklist} \item{merged }{= orig + add + new : returns all records from
#'  master + matched records from checklist + new taxon from checklist}
#'  \item{new }{= returns only new taxon entities that did not match with 
#'  master} \item{multi }{= taxon from checklist for which two synonyms matched
#'   with two different accepted names in master}}
#' @family List functions
#' @examples
#' \donttest{
#' master <- data.frame("id" = c(1,2,3),
#'                      "canonical" = c("Hypochlorosis ancharia",
#'                                      "Hypochlorosis tenebrosa",
#'                                      "Hypochlorosis ancharia tenebrosa"),
#'                      "family" = c("Lycaenidae", "Lycaenidae", "Lycaenidae"),
#'                      "accid" = c(0,1,0),
#'                      "source" = c("itis","itis","itis"),
#'                      stringsAsFactors = FALSE)
#' 
#' checklist <- data.frame("id" = c(1,2,3,4,5),
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
#' merged_all <- merge_lists(master,checklist,output="all")
#' new_taxa <- merge_lists(master,checklist,output="new")
#' merged_with_new <- merge_lists(master,checklist,output="merged")
#' merged_add <- merge_lists(master,checklist,output="add")
#' multi_linked <- merge_lists(master,checklist,output="multi")
#' }
#' @rdname merge_lists
#' @export
merge_lists <- function(master = NULL,
                        checklist = NULL,
                        output="all",
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
  noaddlist <- NULL
  multilist <- NULL
  names(master) <- tolower(names(master))
  names(checklist) <- tolower(names(checklist))
  master <- compact_ids(master,"id","accid",1,verbose)
  idcount <- max(master$id) + 1
  checklist <- compact_ids(checklist,"id","accid",idcount,verbose)
  check_acc <- checklist[which(checklist$accid==0),]
  for(i in 1:nrow(check_acc)){
    if(verbose){cat(paste("\n",i))}
    recset <- get_id_recs(checklist,check_acc$id[i])
    if(!is.null(recset)){
      found <- FALSE
      found_count <- 0
      accid_set <- c()
      for(j in 1:nrow(recset)){
        if(recset$canonical[j] %in% master$canonical) {
          found <- TRUE
          set_accid <- get_accid(master,as.character(recset$canonical[j]),
                                 verbose)
          accid_set <- c(accid_set,set_accid)
          found_count <- found_count + 1
        }
      }
      if(length(unique(accid_set))==0){
        noaddlist <- rbind(noaddlist,recset)
        if(verbose){cat("-")}
        next
      }
      if(length(unique(accid_set))==1){
        for(k in 1:dim(recset)[1]){
          if(verbose){cat("|")}
          addrec <- recset[k,]
          addrec$id <- idcount
          idcount <- idcount + 1
          addrec$accid <- set_accid
          addlist <- rbind(addlist,addrec)
          if(verbose){cat("+")}
        }
      } else {
        multilist <- rbind(multilist,recset)
        if(verbose){print(accid_set)}
        if(verbose){cat("*")}
      }
    }
  }
  if(verbose){cat("\n")}
  if(!is.null(addlist)){
    startnew <- max(addlist$id)+1
  } else {
    startnew <- max(master$id)+1
  }
  noaddlist <- compact_ids(noaddlist,id="id",accid = "accid",                                  startid=startnew,verbose)
  retdf <- master
  retdf$merge_tag <- "orig"
  if(!is.null(addlist)){
    addlist$merge_tag <- "add"
    retdf <- plyr::rbind.fill(retdf,addlist)
  }
  if(!is.null(noaddlist)){
    noaddlist$merge_tag <- "new"
    retdf <- plyr::rbind.fill(retdf,noaddlist)
  }
  if(!is.null(multilist)){
    multilist$merge_tag <- "multi"
    retdf <- plyr::rbind.fill(retdf,multilist)
  }
  switch(output, 
         all={
         },
         add={
           retdf <- retdf[which(retdf$merge_tag %in% c("orig","add")),]
         },
         onlyadd={
           retdf <- retdf[which(retdf$merge_tag %in% c("add")),]
         },
         merged={
           retdf <- retdf[which(retdf$merge_tag %in% c("orig","add","new")),]
         },
         new={
           retdf <- retdf[which(retdf$merge_tag == "new"),]
         },
         multi={
           retdf <- retdf[which(retdf$merge_tag == "multi"),]
         },
         {
           message(paste("Could not resolve 'output =",output,
                         "'Returning all data"))
         }
  )
  if(nrow(retdf)<1){
    retdf <- NULL
  }
  return(retdf)
}

get_id_recs <- function(checklist,id){
  retset <- NULL
  rec <- checklist[which(checklist$id==id),]
  recs <- checklist[which(checklist$accid==id),]
  retset <- rbind(retset,rec,recs)
  if(dim(retset)[1]<1){
    return(NULL)
  } else {
    return(retset)
  }
}

get_accid <- function(master,name,verbose=FALSE){
  if(verbose){cat(".")}
  id <- 0
  mset <- master[which(master$canonical==name),]
  if(dim(mset)[1]>0){
    if(mset[1,c("accid")]==0){
      id <- mset[1,c("id")]
    } else {
      id <- mset[1,c("accid")]
    }
  }
  return(as.numeric(id))
}
