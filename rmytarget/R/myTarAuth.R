#' Authentication in 'MyTarget API'
#' @description Authentication in 'MyTarget API' by Code Grant Schema or other auth schems. For detail you can see \href{https://target.my.com/help/advertisers/api_authorization/ru}{documentation}.
#' @param login Account name, used in file name if you save credential
#' @param grant_type Your account grant, get one of two values, "client_credentials" or "agency_client_credentials". Not use in Code Grant Schema. 
#' @param client_id Yoyr client ID. Not use in Code Grant Schema. 
#' @param client_secret Your client secret. Not use in Code Grant Schema. 
#' @param agency_client_name Your client user name. Only for "agency_client_credentials" grant_type.
#' @param code_grant logical, Use code gran authorise schema, \href{https://target.my.com/help/advertisers/api_authorization/ru}{detail}
#' @param token_path Path to directory where you save credential data.
#'
#' @return No return value, called for side effects
#' @export
#' 
#' @seealso API authorization \href{https://target.my.com/help/advertisers/api_authorization/ru}{documentation} by 'MyTarget'.
#' @author Alexey Seleznev
#' @examples
#' \dontrun{
#'# Recomendation auth by code grant schema
#'myTarAuth(login = "my_account_name")
#'
#'}						  
myTarAuth <-
  function(login              = getOption("rmytarget.login"),
           grant_type         = "client_credentials", 
           client_id          = getOption('rmytarget.client_id'),
           client_secret      = getOption("rmytarget.client_secret"),
           agency_client_name = NULL,
           code_grant         = getOption("rmytarget.code_grant_auth"),
           token_path         = myTarTokenPath()){
    
    
    if (code_grant == TRUE) {
      
      # try load token
      if(file.exists(paste0(token_path, "/", login, ".mytar.Auth.RData"))) {
        
        message("Load token from ", paste0(token_path, "/", login, ".mytar.Auth.RData"))
        load(paste0(token_path, "/", login, ".mytar.Auth.RData"))
        if (!is.null(parse_token$error)) {
          stop(parse_token$error,": ", parse_token$error_description)
        }
        # check expire token, and update him
        if ( !length(parse_token$expire_at) == 0 ) {
          if (as.numeric(parse_token$expire_at - Sys.time(), units = "mins") < 30 & !is.null(parse_token$expires_in) ) {
            message("Token expire after ", round(as.numeric(parse_token$expire_at - Sys.time(), units = "mins"), 0), " mins")
            message("Auto refreshing token")
            
            parse_token <- myTarRefreshToken(old_auth = parse_token, client_id = client_id, client_secret = client_secret)
            
            if (! is.null(parse_token$error)) {
              stop(parse_token$error,": ", parse_token$error_description)
            }
            
            parse_token$expire_at <- Sys.time() + as.numeric(parse_token$expires_in, units = "secs") 
            save(parse_token, file = paste0(token_path, "/", login, ".mytar.Auth.RData"))
            message("Token saved at ", paste0(token_path, "/", login, ".mytar.Auth.RData"))
            return(parse_token)
          }
          
        } else {
          # return token if he live more than 60 mins
          return(parse_token)
        }
      }
      
      # brows
      check_left_tokens <- "first_check"
      try_num           <- 1
      
      while (check_left_tokens %in% c("first_check", "error")) {
        
        # create state token
        min_l <- letters[runif(n = 10, min = 1, max = length(letters))]
        up_l  <- LETTERS[runif(n = 10, min = 1, max = length(LETTERS))]
        nums  <- as.character(round(runif(20, min = 0, max = 9), 0))
        state <- paste0(sample(c(min_l, up_l, nums), size = 14, replace = T), collapse = "")
        
        browseURL(str_interp("${getOption('rmytarget.url')}oauth2/authorize?response_type=code&client_id=${client_id}&state=${state}&scope=read_payments,read_ads,read_clients,read_manager_clients"))
        code <- readline(prompt = "Enter code from browser: ")
        
        raw_token <- POST(url = str_interp("${getOption('rmytarget.url')}api/v2/oauth2/token.json"),
                          body = list(grant_type = "authorization_code", code = code, client_id = client_id, permanent = "true"), 
                          encode = "form")
        parse_token <- content(raw_token, as = "parsed", type = "application/json")
        parse_token$expire_at <- Sys.time() + as.numeric(parse_token$expires_in, units = "secs")
        
        if ( !is.null(parse_token$error) ) {
          if ( parse_token$error == "token_limit_exceeded") {
            message("WARNING!!! You tokens limit exceeded, delete all previous tokens?")
            del <-  readline("y / n: ")
              if (del  %in% c("y", "yes", "ok", "save") ) {
                log2 <- readline("Enter login for client: ")
                del_token <- POST(url = str_interp("${getOption('rmytarget.url')}api/v2/oauth2/token/delete.json"),
                                  body = list(client_secret = client_secret, client_id = client_id, username = log2), 
                                  encode = "form")
                content(del_token)
              } else {
                stop(parse_token)
              }
          }
        } else {
          check_left_tokens <- "done"
        }
        
        try_num <- try_num + 1
        
        if ( try_num > 5 ) break
      }
      
      # savetoken
      if ( !is.null(parse_token$access_token) ) {
        
        # save token in file
        message(str_interp("You tokens left for this account: ${parse_token$tokens_left}"))
        message("Do you want save API credential in local file (",paste0(token_path, "/", login, ".rymAuth.RData"),"), for use it between R sessions?")
        ans <- readline("y / n (recomedation - y): ")
        if ( tolower(ans) %in% c("y", "yes", "ok", "save") ) {
          # create folder if need
          if (!dir.exists(token_path)) {
            dir.create(token_path)
          }
          save(parse_token, file = paste0(token_path, "/", login, ".mytar.Auth.RData"))
          message("Token saved at ", paste0(token_path, "/", login, ".mytar.Auth.RData"))
        }
      }
      
      return(parse_token)
    } else {
      
      query_body <- paste0("grant_type=", grant_type,
                           "&client_id=", client_id,
                           "&client_secret=", client_secret,
                           ifelse(grant_type == "agency_client_credentials", paste0("&agency_client_name=",agency_client_name),""))
      
      mtAuth <- POST(str_interp("${getOption('rmytarget.url')}token.json"),body = query_body, content_type(type = "application/x-www-form-urlencoded"))
      
      stop_for_status(mtAuth)
      
      mtAuth <- content(mtAuth, "parsed", "application/json")
      return(mtAuth)
    }
    
  }


myTarTokenPath <- function() {
  
  if ( is.null(getOption('rmytarget.token_path')) ) {
    token_path <- getwd()
  } else {
    token_path <- getOption('rmytarget.token_path')
  }
  
  return(token_path)
  
}

#' Set MyTarget login
#'
#' @param login Your login, or client name in MyTarget account
#'
#' @return No return value, called for side effects
#' @export
myTarSetLogin <- function(login) {
  
  options('rmytarget.login' = login)
  
}

#' Set path to auth cache
#'
#' @param token_path Path to directory where you save credential data
#'
#' @return No return value, called for side effects
#' @export
myTarSetTokenPath <- function(token_path) {
  
  options('rmytarget.token_path' = token_path)
  
}