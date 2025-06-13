#' @importFrom  httr GET
#' @importFrom  httr POST
#' @importFrom  httr DELETE
#' @importFrom  httr PUT
#' @importFrom  httr content
#' @importFrom  httr parse_url
#' @importFrom  httr build_url
#' @importFrom  httr authenticate
#' @importFrom  httr add_headers
#' @importFrom  httr reset_config
#' @importFrom  httr set_config
#' @importFrom  httr upload_file
#' @importFrom  data.table rbindlist
#' @importFrom  data.table :=
#' @importFrom  xml2 read_html
#' @importFrom  jsonlite toJSON
#' @importFrom  jsonlite fromJSON
#' @importFrom  jsonlite unbox
#' @importFrom  jsonlite base64_dec
#' @importFrom  whisker whisker.render
NULL

#' Create an account object
#'
#' Creates a wrapper around the httr \code{\link{authenticate}} function. Adds an "account" class.
#' @param user The SauceLabs user. By default an environmental variable "SLUSER" is looked for.
#' @param password The SauceLabs password for user. By default an environmental variable "SLPASS" is looked for.
#'
#' @return returns an object of class "account".
#' @export
#'
#' @examples
#' \dontrun{
#' myAcc <- account()
#' }

account <- function(user = Sys.getenv("SLUSER"), password = Sys.getenv("SLPASS")){
  acc <- list(request = authenticate(user = user, password = password))
  class(acc) <- c(class(acc), "account")
  acc
}


#' Send a query to SauceLabs.
#'
#' \code{queryAPI} A function to send a query to SauceLabs. Intended for seleniumPipes
#'    internal use mainly.
#' @param verb The http method to use. See \code{\link{VERB}}
#' @template account
#' @param url The url of the remote server endpoint.
#' @param source The name of the RSauceLabs function that called queryDriver.
#' @template ellipsis
#'
#' @return The contents of the response from the remote server. See \code{\link{content}} for details.
#' @export
#'
#' @examples
#' \dontrun{
#' # function intended for internal use
#' }
queryAPI <- function(verb = GET, account, url, source, ...){
  on.exit(reset_config())
  set_config(account$request)
  res <- if(!is.null(list(...)[["body"]])){
    verb(url = url, add_headers ("Content-Type" = "application/json"), ...)
  }else{
    verb(url = url, ...)
  }
  # Add error checking code here
  checkSLRes(res)
  res <- content(res)
  res
}

checkSLRes <- function(response){
  if(identical(response$status_code, 200L)){
    return()
  }else{
    warning(paste("Response had status:"), response$status_code)
  }
}
