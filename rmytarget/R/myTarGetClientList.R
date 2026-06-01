#' Get Agency Clients from 'MyTarget'
#'
#' @inheritParams myTarAuth 
#' @inheritParams myTarGetAdList 
#'
#' @return data frame with agency clients
#' @export
#' @seealso {MyTarget API Documenation}{https://target.my.com/doc/api/detailed/#resource_agency_clients}
#'
#' @examples
#' \dontrun{
#' myTarSetLogin("Your Login")
#' clients <- myTarGetClientList()
#' }
myTarGetClientList <-
  function(auth = NULL, 
           login = getOption('rmytarget.login'), 
           token_path = myTarTokenPath()){
    if (is.null(auth)) {
      auth <- myTarAuth(login = login, token_path = token_path)
    }
    
    limit  <- 5
    offset <- 0
    count  <- NULL
    result <- list()
    clients <- list()
    
    while ( is.null(count) || count > offset ) {
      
    asw <- GET(stringr::str_interp("${getOption('rmytarget.url')}api/v2/agency/clients.json"),
               query = list(limit  = limit,
                            offset = offset),
               add_headers(Authorization = paste0("Bearer ",auth$access_token)))
    stop_for_status(asw)
    answer <- content(asw, "parsed", "application/json")
    
    # check limit
    #if ( ! myTarCheckLimits(answer) ) stop("Limit error")
    
    for ( i in answer$items ) {
      temp <- list( id = i$user$id,
                    username = i$user$username,
                    userstatus = i$user$status,
                    a_balance = i$user$account$a_balance,
                    balance = i$user$account$balance,
                    type = i$user$account$type,
                    client_info = i$user$additional_info$client_info,
                    client_name = i$user$additional_info$client_name,
                    status = i$status,
                    access_type = i$access_type, 
                    status = i$status  )
      
       clients <- append(clients, list(temp))
    }
    
    count  <- answer$count
    offset <- offset + limit
    }
    
    clients <- bind_rows(clients) 
    
    return(clients)
  }
