#' Generate a list melting character (comma) separated field values into multiple
#' records
#'
#' Builds a list, melting character (comma) separated field values given a data frame
#' with a field with repeating values
#'
#' @param data data frame containing a data columns with character(comma) separated values
#' @param melt Field name with character(comma) separated values
#' @param sepchar Character separator between the data items. Default is comma
#' @param verbose verbose output, Default: FALSE
#' @return a data frame with separate records for each value in field specified
#' @examples \donttest{
#'   scnames <- c("Abrothrix longipilis", "Abrothrix jelskii")
#'   syn_list <- list_itis_syn(scnames)
#'   cs_syn_list <- cast_cs_field(syn_list ,"Name","Syn")
#'   syn_list_new <- melt_cs_field(cs_syn_list,"Syn")
#'}
#'
#' @family List functions
#' @export
melt_cs_field <- function(data,melt,sepchar=",",verbose=FALSE){
  tdata <- data
  tdata <- rename_column(tdata,melt,'pri')
  if(!is.null(tdata)){
    tdata$pri <- as.character(tdata$pri)
    retdat <- NULL
    if(verbose){pb = txtProgressBar(min = 0, max = nrow(tdata), initial = 0)}
    for(i in 1:nrow(tdata)){
      crec <- tdata[i,]
      if(tdata$pri[i]!="" & !is.na(tdata$pri[i])){
        items <- strsplit(tdata$pri[i],sepchar,fixed = TRUE)[[1]]
        for (j in 1:length(items)){
          addrec <- crec
          addrec$pri <- trimws(items[j])
          retdat <- rbind(retdat,addrec)
        }
      } else {
        addrec <- crec
        retdat <- rbind(retdat,addrec)
      }
      if(verbose){setTxtProgressBar(pb,i)}
    }
    if(verbose){cat("\n")}
    retdat <- as.data.frame(retdat)
    retdat <- rename_column(retdat,'pri',melt)
    rownames(retdat) <- NULL
    return(retdat)
  }  else {
    return(NULL)
  }
}
