#' Accesses the Eschmeyer's Catalog of Fishes Guide to Fish Collections
#' 
#' @param query Character Vector of search terms to search for. Default is NULL.
#' @param abbreviation Character vector containing the musuem collection code(s). Default is NULL. 
#' @param country Character vector of country names to search for. Default is NULL.
#' @param phrase Logical. Should query be passed as a quoted phrase (e.g. "Museum of Zoology"). Default is FALSE.
#' @param sleep.time Numeric. Time in seconds to sleep between query calls to the California Academy of Sciences page. This is set by default to 10 seconds, which is in their robots.txt. Adjust at your own risk.
#' @param verbose Logical. Should query progress be messaged to the screen? Default is TRUE.
#' @return Data frame, in which each row is a natural history collection and each column the corresponding collection information (see itemized list below).
#' \itemize{
#'   \item Code - Character. Natural history collection abbreviation code.
#'   \item Name  - Character. Full name of the natural history collection.
#'   \item Country  - Character. Country of the natural history collection.
#'   \item OtherAbbr - Character. Other and previous abbreviation(s) used for that natural history collection.
#'   \item OldName - Character. Previous name(s) used by the natural history.
#'   \item NewName - Character. New name(s) of the natural history collection.
#'   \item WebPage - Character. URL for the webpage of the natural history collection if one exists.
#'   \item CollectionDB - Character. URL for accessing the collection database of the natural history collection if one exists.
#'   \item TypesDB - Character. URL for accessing the type database of the natural history collection if one exists.
#'   \item Remarks - Character. Additional information about the natural history collection (e.g. includes specimens from other collections, closed, etc.).
#'   \item Publications - Character. Comma separated character vector of Eschmeyer's Catalog of Fishes reference numbers citing the natural history collection.
#' }
#' @details This function retrieves and parses natural history collection data from the Eschmeyer's Catalog of
#' Fishes. Returned information includes the collection abbreviation, country, alternate abbreviations,
#' old and new collection names, websites and databases associated with the collection, remarks, and
#' publications associated with the collection.
#' 
#' It is important that search criteria match exactly how they are on the Catalog of Fishes, otherwise
#' data will not be found (case must match, spelling, etc.). Users wishing to query more than one query
#' mixed with either a country or abbreviation should note that all parameters must be of equal length, so if searching
#' two queries both in U.S.A., the country should be supplied twice (see the final example in the examples below).
#' @examples
#' #Search Museum Code For UMMZ (University of Michigan Museum of Zoology)
#' my.collections <- rcatfish_collections(abbreviation = "UMMZ", country = NULL, 
#' query = NULL, verbose = TRUE)
#' \donttest{
#' #Search For Collections in France
#' my.collections <- rcatfish_collections(abbreviation = NULL, country = "France", query = NULL)
#' #Search For Collections in The United States that are in Illinois
#' my.collections <- rcatfish_collections(abbreviation = NULL, country = "U.S.A.", query = "Illinois")
#' #Search For Collections in the United States that are in California & Alaska.
#' my.collections <- rcatfish_collections(abbreviation = NULL, country = rep("U.S.A.",2), 
#' query = c("California","Alaska"), sleep.time = 10)
#' #Search for Collections with Museum of Zoology in the name
#' my.collections <- rcatfish_collections(query = "Museum of Zoology", phrase = TRUE)
#' }
#' @author Samuel R. Borstein
#' @references 
#' Fricke, R. & Eschmeyer, W.N. (Year Accessed). Eschmeyer's Catalog of Fishes: Guide to Fish Collections. https://researcharchive.calacademy.org/research/ichthyology/catalog/collections.asp.
#' @export

rcatfish_collections <- function(query = NULL, abbreviation = NULL, country = NULL, phrase = FALSE, sleep.time = 10, verbose = TRUE){
  collections.final<- data.frame(matrix(ncol = 11,nrow = 0))
  colnames(collections.final)<-c("Code","Name","Country","OtherAbbr","OldName","NewName","WebPage","CollectionDB","TypesDB","Remarks","Publications")
  max.length<-max(length(abbreviation), length(country), length(query))
  sleep.time <- ifelse(max.length>1, sleep.time, 0)#Set sleep time for queries longer than 1
  for(query.index in 1:max.length){
    if (verbose == TRUE) {
      message(paste0("Now on query ",query.index, " of ", max.length))
    }
    url <- ("https://researcharchive.calacademy.org/research/ichthyology/catalog/collections.asp")
    pgsession <- rvest::session(url)#start session
    pgform <- rvest::html_form(pgsession)#get form fields
    if(is.null(query)){
      vals <- rvest::html_form_set(pgform[[1]],Mus = abbreviation[query.index], Country = country [query.index], Mus_name = query[query.index])#Add in search values
    }else{
      vals <- rvest::html_form_set(pgform[[1]],Mus = abbreviation[query.index], Country = country [query.index], Mus_name = ifelse(phrase == TRUE, shQuote(query[query.index], type = "cmd"), query[query.index]))#Add in search values
    }
    res <- rvest::session_submit(pgsession, vals, submit = "xAction")#Submit search form
    result.rv <- xml2::read_html(res)#parse xml
    TableColl <- as.character(rvest::html_elements(result.rv, 'table')[-c(1:5)])#Get database lines
    local.result <- TableColl[!grepl('<p class=\"result\">\r\n</p>\n', TableColl)]#Kill spaces
    current.dat<- data.frame(matrix(ncol = 11,nrow = length(local.result)))
    colnames(current.dat)<-c("Code","Name","Country","OtherAbbr","OldName","NewName","WebPage","CollectionDB","TypesDB","Remarks","Publications")
    #local.result <- StripFun(local.result)
    #collection.code <- sapply(strsplit(local.result, "\\s+"), "[", 1)
    for(record.index in seq_along(local.result)){
      collection.dat <- qdapRegex::ex_between(local.result[record.index], left = "<b>",right = "</b></td>", fixed = TRUE)
      current.dat$Code[record.index] <- str_extract(string = collection.dat[[1]][1], "\\S{1,}")
      ifelse(grepl("See ",collection.dat[[1]][[2]]),current.dat$Name[record.index]<-paste0("See ",qdapRegex::ex_between(collection.dat[[1]][2],">","</a>")),current.dat$Name[record.index]<-collection.dat[[1]][[2]])
      #collection code is [[1]][1] and name is [[1]][2]
      #collection.name <- qdapRegex::ex_between(local.result, left = "\\n",right = "\\n", fixed = TRUE)[[1]][1]
      current.dat$Country[record.index] <- qdapRegex::ex_between(local.result[record.index], left = "Country:</b>",right = "</td>", fixed = TRUE)
      current.dat$OtherAbbr[record.index] <- qdapRegex::ex_between(local.result[record.index],left = "Other Abbrev.:</b>", right = ".")
      current.dat$OldName[record.index] <- qdapRegex::ex_between(local.result[record.index],left = "Old Name:</b>", right = "<b>")   
      current.dat$NewName[record.index] <- qdapRegex::ex_between(local.result[record.index],left = "New Name:</b>", right = "<b>", fixed = TRUE)
      current.dat$WebPage[record.index] <- qdapRegex::ex_between(local.result[record.index],left = "<b>Main site:</b> <a href=\"", right = "\\\"")
      current.dat$CollectionDB[record.index] <- qdapRegex::ex_between(local.result[record.index],left = "<b>Coll. database</b>: <a href=\"", right = "\\\"")
      current.dat$TypesDB[record.index] <- qdapRegex::ex_between(local.result[record.index],left = "<b>Types database</b>: <a href=\"", right = "\\\"")
      current.dat$Remarks[record.index] <- qdapRegex::ex_between(local.result[record.index],left = "Remarks:</b>", right = "</td>")
      current.dat$Publications[record.index] <-paste(qdapRegex::ex_between(local.result[record.index],left = "getref.asp?id=", right = "\"", fixed = TRUE)[[1]],sep = ",",collapse = ",")
    }
    collections.final<-rbind.data.frame(collections.final,current.dat)
    Sys.sleep(sleep.time)
  }
    return(collections.final)
}
