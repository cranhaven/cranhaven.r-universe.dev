#' Build a character (comma) separated List within field
#'
#' Builds a character (comma) separated list within a field given a data frame
#' with primary field repeating values and secondary field with values to be
#' character separated in the same field (secondary)
#'
#' @param data data frame containing primary and secondary data columns
#' @param pri Primary field name (repeating values)
#' @param sec Secondary field (values would be added to same record,
#' comma separated)
#' @param duplicate If true, duplicate entries are allowed in secondary field
#' @param sepchar Character separator between the data items. Default is comma
#' @param verbose verbose output, Default: FALSE
#' @return a data frame with two fields Primary and secondary (comma
#' separated list)
#' @examples \donttest{
#'SynList <- data.frame("canonical" = c("Abrothrix longipilis",
#'                                     "Abrothrix longipilis",
#'                                     "Abrothrix longipilis",
#'                                     "Abrothrix longipilis",
#'                                     "Abrothrix jelskii",
#'                                     "Abrothrix jelskii"),
#'                    "synonym" = c("Akodon longipilis",
#'                                  "Acodon hirtus",
#'                                  "Akodon longipilis apta",
#'                                  "Akodon longipilis castaneus",
#'                                  "Chroeomys jelskii",
#'                                  "Acodon jelskii pyrrhotis"),
#'                     stringsAsFactors = FALSE)
#'   cast_cs_field(SynList,"canonical","synonym")
#'}
#'
#' @family List functions
#' @export
cast_cs_field <- function(data,pri,sec,duplicate=FALSE,sepchar=",",
                          verbose=FALSE){
  if(missing(data)){
    stop("Data needs to be passed for processing")
  }
  if(missing(pri)){
    stop("Primary data field (pri) needs to be specified")
  }
  if(missing(sec)){
    stop("Secondary data field (sec) needs to be specified")
  }
  if(nrow(data)<2){
    return(data)
  }
  tdata <- data
  tdata <- rename_column(tdata,pri,"pri")
  tdata <- rename_column(tdata,sec,"sec")
  if(!is.null(tdata) & 
     c("pri") %in% names(tdata) & 
     c("sec") %in% names(tdata)){
    tdata$pri <- as.character(tdata$pri)
    tdata$sec <- as.character(tdata$sec)
    tdata <- tdata[which(!is.na(tdata$pri)),]
    tdata <- tdata[order(tdata$pri),]
    oldpri <- tdata$pri[1]
    oldrec <- tdata[1,]
    retdat <- NULL
    newsec <- tdata$sec[1]
    if(verbose){pb = txtProgressBar(min = 0, max = nrow(tdata), initial = 0)}
    for(i in 2:nrow(tdata)){
      if(tdata$pri[i]==oldpri){
        if(!is.empty(newsec) & !is.empty(tdata$sec[i])){
          newsec <- paste(newsec,sepchar," ",tdata$sec[i])
        } else {
          newsec <- ifelse(!is.empty(newsec),newsec,tdata$sec[i])
        }
      } else {
        rec <- oldrec
        rec$sec <- newsec
        retdat <- rbind(retdat,rec)
        oldpri <- tdata$pri[i]
        oldrec <- tdata[i,]
        newsec <- tdata$sec[i]
      }
      if(!duplicate){
        newsec <- dedup_csl(newsec,sepchar)
      }
      if(verbose){setTxtProgressBar(pb,i)}
    }
    if(verbose){cat("\n")}
    rec <- oldrec
    rec$sec <- newsec
    retdat <- rbind(retdat,rec)
    retdat <- as.data.frame(retdat)
    retdat <- rename_column(retdat,"pri",pri)
    retdat <- rename_column(retdat,"sec",sec)
    rownames(retdat) <- NULL
    return(retdat)
  } else {
    return(NULL)
  }
}


dedup_csl <- function(vec,sepchar){
  if(is.na(vec)){
    return(NA)
  }
  tmp <- strsplit(vec,sepchar)[[1]]
  tmp <- trimws(tmp)
  tmp <- unique(tmp)
  return(paste(tmp, collapse=paste(sepchar," ",sep="")))
}
