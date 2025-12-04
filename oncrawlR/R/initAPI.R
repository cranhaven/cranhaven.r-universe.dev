#' Prepare Token for API calls
#'
#' @param path path of your conf file
#'
#' @details
#' Example file for oncrawl_configuration.txt
#'
#' key = 5516LP29W5Q9XXXXXXXXXXXXOEUGWHM9
#' debug = FALSE
#' api = https://app.oncrawl.com/api/v2/
#'
#' @examples
#' \dontrun{
#' initAPI("oncrawl_configuration.txt")
#' }
#'
#' @return ok if no error with API authentification
#' @author Vincent Terrasi
#' @export
#' @importFrom utils read.csv read.delim
#'
initAPI <- function(path) {

  if(!file.exists(path)) stop("Please, set your API Key in the file oncrawl_configuration.txt")

  tab <- read.delim(path, header=FALSE, sep="=", stringsAsFactors = FALSE, strip.white=FALSE)

  if (!exists("tab")) stop("Please, set your API Key in the file oncrawl_configuration.txt")

  token <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", tab[1,2])
  options(oncrawl_token = token)

  debug <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", tab[2,2])
  if ( debug=="TRUE" )
    options(oncrawl_debug = TRUE)
  else
    options(oncrawl_debug = FALSE)

  api <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", tab[3,2])
  options(oncrawl_api = api)

  token <- getOption('oncrawl_token')

  if(nchar(token)<=10) {
    token <- NULL
    api <- NULL
    debug <- NULL
    stop("Please, set your API Key in the file oncrawl_configuration.txt")
  }

  return("ok")
}

