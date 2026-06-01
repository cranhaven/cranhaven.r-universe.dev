#' Refresh Access Token
#'
#' @param old_auth Your old auth object. See \code{\link{myTarAuth}}
#' @param client_id Your client ID
#' @param client_secret Your client secret
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' \dontrun{
#' auth <- myTarRefreshToken(old_auth = myTargetAuth,
#'                          client_id = "xxxx",
#'                          client_secret = "xxxx.")
#' }
myTarRefreshToken <-
function(old_auth = NULL, client_id = NULL,client_secret = NULL){
  if(is.null(old_auth) | is.null(client_id) | is.null(client_secret)){
    
    stop("All arguments is require!!!",call. = FALSE)
  }
  query_body <- paste0("grant_type=", "refresh_token",
                       "&refresh_token=", old_auth$refresh_token,
                       "&client_id=", client_id,
                       "&client_secret=", client_secret)
  
  mtRefrsh <- POST(stringr::str_interp("${getOption('rmytarget.url')}api/v2/oauth2/token.json"),body = query_body, content_type(type = "application/x-www-form-urlencoded"))
  stop_for_status(mtRefrsh)
  mtRefrsh <- content(mtRefrsh, "parsed", "application/json")
  
  old_auth$access_token <- mtRefrsh$access_token
  return(old_auth)
}

