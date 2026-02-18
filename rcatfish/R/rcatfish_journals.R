#' Search for journals in the Catalog of Fishes
#' @param query Character vector to search for.
#' @param phrase Logical. Should query be passed as a quoted phrase (e.g. "Journal of Zoology"). Default is FALSE.
#' @param sleep.time Numeric. Time in seconds to sleep between query calls to the California Academy of Sciences page. This is set by default to 10 seconds, which is in their robots.txt. Adjust at your own risk.
#' @param verbose Logical. Should query progress be messaged to the screen? Default is TRUE.
#' @return Two column data frame. The first column contains the name of the query and the second column contains information on the journal.
#' @details This function returns information on the journals cited in the Eschmeyer's Catalog of Fishes.
#' @examples
#' my.journals<-rcatfish_journals(query="Cichlid")
#' my.journals<-rcatfish_journals(query="Evolution")
#' @author Samuel R. Borstein
#' @references 
#' Fricke, R. & Eschmeyer, W.N. (Year Accessed). Eschmeyer’s Catalog of Fishes: Journals. https://researcharchive.calacademy.org/research/ichthyology/catalog/journals.asp
#' 
#' @export

rcatfish_journals<-function(query, phrase = FALSE,sleep.time = 10, verbose = TRUE){
  journal.dat <- data.frame(matrix(nrow = 0,ncol = 2),stringsAsFactors = FALSE)
  colnames(journal.dat) <- c("query","Journal")
  for(query.index in seq_along(query)){
    if (verbose == TRUE) {
      message(paste0("Now on query ",query.index, " of ", length(query), ", ",query[query.index]))
    }
    result <- RCurl::postForm("https://researcharchive.calacademy.org/research/ichthyology/catalog/journals.asp",
                            contains=ifelse(phrase == TRUE, shQuote(query, type = "cmd"), query), Submit="Search",style='POST',.encoding = "utf-8")#Perform search
    result.rv <- xml2::read_html(result)#parse results
    local.results <- as.character(rvest::html_elements(result.rv,".bodytext")[-1])#Strip out header
    strip.res <- StripFun(local.results)#Strip out html tags
    refs <- strsplit(strip.res,"\r\n\t\t\n")#split records into individual
    refs <- strsplit(refs[[1]],"\r\n\t\t\r\n  ")#split records into individual
    refs <- gsub(pattern = "\r\n\\[.*].*\r\n\n",replacement = '',x = refs)#Kill of some extra text
    refs<-gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", refs, perl=TRUE)
    current.refs<-data.frame(rep(query[query.index],length(refs)),refs,stringsAsFactors = FALSE)
    journal.dat <- rbind.data.frame(journal.dat,current.refs,stringsAsFactors = FALSE)
    if (length(query)>1) {
      Sys.sleep(sleep.time)#Sleep between calls for non-overload if query is longer than 1
    }
  }
  colnames(journal.dat)<-c("Query","Journals")
  return(journal.dat)
}

