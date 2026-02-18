#' Search for references in Eschmeyer's Catalog of Fishes
#' 
#' @description Search for references in the Catalog of Fishes by keyword or reference number.
#' @param query Either a character vector of keywords to search for or a numeric vector with reference numbers
#' @param type Either "RefNo" if searching for a reference number or "keyword" if searching by keywords
#' reference numbers.
#' @param sleep.time Numeric. Time in seconds to sleep between query calls to the California Academy of Sciences page. This is set by default to 10 seconds, which is in their robots.txt. Adjust at your own risk.
#' @param verbose Logical. Should query progress be messaged to the screen? Default is TRUE.
#' @details This function searches Catalog of Fishes references. Users can supply either a keyword 
#' (i.e. cichlidae, revision, etc.) or a reference number to retrieve reference information.
#' @return A data.frame containing columns for the query, reference number, and full citation.
#' @examples 
#' #Perform a search of references that contain the keyword Amphilophus.
#' my.refs<-rcatfish_references(query = "Amphilophus", type = "keyword")
#' #Perform a search of references based on a Catalog of Fishes reference number
#' my.refs<-rcatfish_references(query = 2787, type = "RefNo")
#' @author Samuel R. Borstein, Brian C. O'Meara
#' @references 
#' Fricke, R. (Year Accessed). Eschmeyer's Catalog of Fishes: References. https://researcharchive.calacademy.org/research/ichthyology/catalog/fishcatmain.asp
#' @export
#' 
rcatfish_references<-function(query, type, sleep.time = 10, verbose = TRUE){
  if(length(type) > 1){#Check length of type. Must be 1.
    stop("Length of argument type must be 1")
  }
  
  stopifnot("Argument type must be either RefNo or keyword" = type %in% c("RefNo", "keyword"))#Error if type is not a valid option
  
  ResTable<-as.data.frame(matrix(ncol=3, nrow = 0))
  colnames(ResTable)<-c("Query","RefNo","Reference")
  raw.query <- query
  ifelse(type=="RefNo",query<-paste("\"ref. ",query,"\"",sep=""),query<-query)
  for(query.index in seq_along(query)){
    if (verbose == TRUE) {
      message(paste0("Now on query ",query.index, " of ", length(query), ", ",raw.query[query.index]))
    }
    result <- RCurl::postForm("https://researcharchive.calacademy.org/research/ichthyology/catalog/fishcatmain.asp",
                              contains=query[query.index], tbl="Ref", submit="Submit",
                              style='POST',.encoding = "utf-8")#Perform search
    result.rv <- xml2::read_html(result)#parse results
    local.results <- as.character(rvest::html_elements(result.rv, '.result')[-1])#Strip out header
    #Strip out returns and form into list
    local.results <- local.results[!grepl('<p class=\"result\">\r\n</p>\n', local.results)]
    if(length(local.results)==0){
      warning(paste0("No results found for supplied query ",query[query.index]))
      next
    }
    Results2Parse<-StripFun(local.results)#Strip out html tags
    #Parse out the results
    for (refIndex in seq(1, length(Results2Parse),by = 2)){
      currentRef<-Results2Parse[c(refIndex,refIndex+1)]
      currentRef[1]<-gsub("\n\r\n", replacement = "",x = currentRef[1])#Kill returns
      SplitRefNo<-stringr::str_split_fixed(currentRef[1],"\\[",n = 2)#Sub out reference info
      RefNo <- stringr::str_extract_all(SplitRefNo[2], "[0-9]")#Grab the numbers
      RefNo<-as.numeric(paste0(RefNo[[1]], collapse = ""))#Paste them
      #Make reference from the two halves of info, authors and citation
      Reference<-trimws(paste0(gsub(") ", ").",SplitRefNo[1], fixed = T),gsub("\n","", currentRef[2])))
      current.res<-as.data.frame(matrix(nrow = 1,ncol = 3))
      colnames(current.res)<-colnames(ResTable)
      current.res[1,]<-c(query[query.index],RefNo,Reference)
      ResTable<-rbind(ResTable,current.res)
    }
    if (length(query)>1) {
      Sys.sleep(sleep.time)#Sleep between calls for non-overload if query is longer than 1
    }
  }
  return(ResTable)
}
