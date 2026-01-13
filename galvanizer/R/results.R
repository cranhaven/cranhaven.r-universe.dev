#' Retrieve Highbond Results - Records
#'
#' @description Downloads the content in the structure as saved by Highbond
#'   Results, including metadata and questionnaire responses (if applicable).
#'
#' @details Table ID can be found in the path of your Result table.
#'   https://<company_name>.results.highbond.com/projects/<COLLECTION_ID>/controls/<ANALYSIS_ID>/control_tests/<TABLE_ID>
#'
#' @param auth Highbond authentication credentials, created from \code{\link{setup_highbond}}
#' @param table_id The table to be downloaded
#' @param timezone Defaults to \code{Sys.timezone()}. See \code{OlsonNames()} for a list of allowable timezones.
#'
#' @importFrom dplyr mutate_at vars
#' @importFrom rlang .data
#' @importFrom lubridate as_datetime
#'
#' @return A list containing the contents, table requested and the original response
#' @export
#'
#' @examples
#' \dontrun{
#' response <- get_results_records(auth, 567890)
#' }
get_results_records <- function(auth, table_id, timezone = Sys.timezone()){
  # Result records do not follow the structure of the rest of the endpoints, so
  # its ok to be more custom
  
  hb_checkauth(auth)
  
  # Print table name
  tablename <- get_results_tables(auth, table_id = table_id)$name
  message(paste("Retrieving", tablename))
  
  # Download table
  url <- paste0(hb_url(auth), 'tables/', table_id, '/records')
  resp <- hb_api_get(auth, url)
  parsed <- api_jsonParseDf(resp)
  
  # Separate records and column definition
  hbData <- parsed$data
  colMap <- data.frame(field_name = parsed$columns$field_name, type = parsed$columns$data_type)
  
  if((length(hbData) == 0)){
    hbData <- NULL
  } else {
    hbData <- hbData %>%
      mutate_at(vars(dplyr::filter(colMap, .data$type == 'datetime')$field_name), ~ as_datetime(., tz = timezone)) %>%
      mutate_at(vars(dplyr::filter(colMap, .data$type == 'numeric')$field_name), ~ as.numeric(.)) %>%
      mutate_at(vars(dplyr::filter(colMap, .data$type == 'logical')$field_name), ~ as.logical(.)) %>%
      mutate_at(vars(dplyr::filter(colMap, .data$type == 'date')$field_name), ~ as_datetime(., tz = timezone)) %>%
      mutate_at(vars(dplyr::filter(colMap, .data$type == 'character')$field_name), ~ as.character(.)) 
  }
  
  structure(
    list(
      content = list(data = hbData, columns = parsed$columns),
      table = list(table_id = table_id, table_name = tablename),
      response = resp
    ),
    class = 'highbond_results'
  )
}

#' Upload Highbond Results - Records
#'
#' @description Uploads the content in to Highbond Results.
#'
#' @details Current Results in the table can be purged or not. If data is not
#'   purged, a check is performed that compatible types are being upload and all
#'   fields exist. If purged, there will be no checks done beforehand, which may
#'   alter structure of existing table.
#'   
#'   If the Result table contains questionnaire responses, then any records that
#'   are being updated may need to have the responses also included as well.
#'   These fields are usually prefixed with a 'q'.
#'   
#'   A result row is generally appended, however a Primary Key field within the
#'   Data Analytic Settings for that table may be specified so a record with the
#'   same primary key may be merged, rather than duplicated.
#
#' @inheritParams get_results_records
#'
#' @param upload A data frame to be uploaded. Classes supported are
#'   c('character', 'numeric', 'logical', 'Date', 'POSIXct', 'POSIXlt')
#' @param purge FALSE by default. Whether or not to delete the contents of the table before overwriting.
#' @param skipquestions FALSE by default. Set TRUE if not purging and you don't want to overwrite the responses within the Results table already.
#' @param sizelimit 75000 bytes (~75 kb) by default. Used to estimate chunk size. Reduce size if upload chunks tend to fail.
#'
#' @return No return value, although errors will be verbose
#' @export
#' 
#' @importFrom dplyr mutate_if vars inner_join anti_join mutate across
#' @importFrom tidyselect vars_select_helpers
#' @importFrom utils object.size
#' @importFrom jsonlite toJSON
#' @importFrom httr POST add_headers
#' @importFrom utils capture.output
#' @importFrom lubridate is.POSIXct is.POSIXlt
#'
#' @examples
#' \dontrun{
#'   upload <- data.frame(field_one = c('A','B','C'),
#'   field_two = c(1, 2, 3),
#'   field_three = c(10L, 11L, 12L),
#'   field_four = c(TRUE, FALSE, TRUE),
#'   field_five = c(as.Date('2019-01-01'), as.Date('2020-01-01'), as.Date('2021-12-31')),
#'   field_six = c(as.POSIXct(Sys.time()), as.POSIXct(Sys.time()), as.POSIXct(Sys.time())))
#'   post_results_records(auth, upload = upload, purge = TRUE)
#'   }
post_results_records <- function(auth, table_id, 
                                  upload = NULL, purge = FALSE, skipquestions = FALSE, sizelimit = 75000){
  
  if(is.null(upload) | !is.data.frame(upload)){
    stop('Please specify a dataframe to upload into Highbond Results')
  }
  
  # Print table name
  tablename <- get_results_tables(auth, table_id = table_id)$name
  message(paste("Uploading to", tablename))
  
  `%!in%` <- Negate('%in%')
  
  # Determine if any columns are not valid columns to be uploaded
  
  dfCols <- sapply(lapply(upload, class), function(x) x[1])
  
  if (any(dfCols %!in% c('character', 'integer', 'numeric', 'logical', 'Date', 'POSIXct', 'POSIXlt'))){
    stop("Invalid data types to upload. Examine the classes in the dataframe.")
  }
  
  # Convert datetime to ISO 8701. Call it datetime for HB
  upload <- upload %>%
    mutate(across(vars_select_helpers$where(is.POSIXct), ~ strftime(.x, format = '%Y-%m-%dT%H:%M:%S%z'))) %>%
    mutate(across(vars_select_helpers$where(is.POSIXlt), ~ strftime(.x, format = '%Y-%m-%dT%H:%M:%S%z')))
    # mutate_if(lubridate::is.POSIXct, strftime, format = '%Y-%m-%dT%H:%M:%S%z') %>%
    # mutate_if(lubridate::is.POSIXlt, strftime, format = '%Y-%m-%dT%H:%M:%S%z')
  
  # Convert integers to numeric
  
  upload <- upload %>%
    mutate(across(vars_select_helpers$where(is.integer), as.numeric))
  
  # Reassess new fields, changing name of data types to match Highbond expectation
  dfCols <- gsub('posixct', 'datetime', tolower(dfCols))
  dfCols <- gsub('integer', 'numeric', tolower(dfCols))
  
  # Get and compare exisitng column specifications
  # TODO Add purge checks
  
  if(!purge){
    hbCols <- get_results_columns(auth, table_id)
    
    if (nrow(hbCols) == 0) {
      stop("No columns were detected in specified table. If this is a new table, use argument 'purge = TRUE'")
    }
    
    hbCompare <- data.frame(hb_name = hbCols$id, hb_coltype = hbCols$data_type,
                            row.names = NULL,
                            stringAsFactors = FALSE)
    
    dfCompare <- data.frame(df_name = names(dfCols), col_type = unlist(dfCols),
                            row.names = NULL,
                            stringsAsFactors = FALSE)
    
    # If we're skipping the comparison on the questionnaire, remove any fields with a q
    if (skipquestions){
      hbCompare <- hbCompare %>%
        dplyr::filter(!grepl('[q][0-9]', .data$hb_name))
      
      dfCompare <- dfCompare %>%
        dplyr::filter(!grepl('[q][0-9]', .data$df_name))
    }
    
    missingLocalFields <- anti_join(dfCompare, hbCompare, by = c('df_name' = 'hb_name'))
    if (nrow(missingLocalFields) > 0){
      message(paste0("These fields are missing locally", capture.output(missingLocalFields), collapse = '\n'))
    }
    
    missingRemoteFields <- anti_join(hbCompare, dfCompare, by = c('hb_name' = 'df_name'))
    if (nrow(missingRemoteFields) > 0){
      message(paste0("These fields do not exist in the Highbond Results table", capture.output(missingRemoteFields), collapse = '\n'))
    }
    
    unmatchedFields <- inner_join(dfCompare, hbCompare, by = c('df_name' = 'hb_name'))
    unmatchedFields <- unmatchedFields %>%
      mutate(isMatchTypes = (.data$col_type == .data$hb_coltype)) %>%
      dplyr::filter(.data$isMatchTypes == FALSE)
    
    if (nrow(unmatchedFields) > 0){
      message(paste0("These fields are mismatched in type.", capture.output(unmatchedFields), collapse = '\n'))
    }
    
    if (nrow(missingLocalFields) + nrow(missingRemoteFields) + nrow(unmatchedFields)){
      stop("Please resolve before uploading", .call = TRUE)
    }
  }
  
  # Split datasets to upload
  size <- object.size(toJSON(upload))
  pages <- floor(as.numeric(size / sizelimit)) + 1
  
  uploadChunks <- list(pages) # Create list earlier for R resize list efficiency
  
  if (pages > 1){
    n <- nrow(upload)
    chunk <- ceiling(nrow(upload) / pages)
    reps <- rep(1:ceiling(n/chunk), each = chunk)[1:n]
    uploadChunks <- split(upload, reps)
  } else{
    uploadChunks[[1]] <- upload
  }
  
  # Upload
  i <- 1
  
  while (i <= length(uploadChunks)){
    uploadtoHB <- list()
    uploadtoHB$data$columns <- as.list(dfCols)
    uploadtoHB$data$records <- uploadChunks[[i]]
    uploadtoHB$options$purge <- purge
    
    url <- paste0(hb_url(auth), 'tables/', table_id, '/upload')
    
    message(paste('Uploading', i, 'of', length(uploadChunks), 'to', url))
    
    # Highbond example   
    # x <- '{"data":{"columns":{"field_one":"character","field_two":"character"},"records":[{ "field_one": "X","field_two": "2"},{"field_one": "XYZ","field_two": "676"}]},"options":{"purge":true}}'
    
    # Should this POST function be moved elsewhere?
    POST(url,
         body = toJSON(uploadtoHB, auto_unbox = TRUE),
         add_headers(Authorization = paste('Bearer', auth$key),
                     `Content-Type` = 'application/json'),
         encoding = "raw")
    
    # Turn off purges
    purge <- FALSE
    i <- i + 1
  }
  
  return(NULL)
}

#' Retrieve Highbond Results - Collections
#'
#' @description Downloads a list of collections
#' 
#' @param collection_id Collection ID. Optional.
#' 
#' @return A data frame of Collections
#'
#' @inheritParams get_results_records
#' @export
get_results_collections <- function(auth, collection_id = NULL){
  # Behaves differently than all the other endpoints, so OK to be custom
  
  type <- hb_callingtarget() # Get the last word in the function name

  primary <- collection_id
  secondary <- NULL 
  
  url <- paste0(hb_url(auth), hb_url_component(type, primary))
  plural <- is.null(primary) # Data always nested in data
  
  data <- hb_prj_get_controller(auth, url, NULL, plural) # Download the data
  
  return(data)
}

#' Retrieve Highbond Results - Analyses
#'
#' @description Downloads a list of Analyses in a Collection
#' 
#' @param collection_id Collection ID. Required if other parameter is blank.
#' @param analysis_id Analyses ID. Required if other parameter is blank.
#' 
#' @return A data frame of Analyses in a Collection
#'
#' @inheritParams get_results_records
#' @export
get_results_analyses <- function(auth, collection_id = NULL, analysis_id = NULL){
  component <- 'analyses'
  primary <- collection_id
  secondary <- analysis_id
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary, secondary))
  plural <- is.null(secondary) # Data always nested in data
  
  params <- NULL # No parameters to send with results
  data <- hb_prj_get_controller(auth, url, params, plural) # Download the data
  
  return(data)
}

#' Retrieve Highbond Results - Tables
#'
#' @description Downloads the primary details of one or all tables in an Analyses
#' 
#' @param analysis_id Analysis ID. Required if other parameter is blank.
#' @param table_id Table ID. Required if other parameter is blank.
#' 
#' @return A data frame of Tables in an Analyses
#'
#' @inheritParams get_results_records
#' @export
get_results_tables <- function(auth, analysis_id = NULL, table_id = NULL){
  component <- 'tables'
  primary <- analysis_id
  secondary <- table_id
  
  hb_project_one_only(primary, secondary) # Checks
  url <- paste0(hb_url(auth), hb_url_component(component, primary, secondary))
  plural <- is.null(secondary)
  
  params <- NULL # No parameters to send with results
  data <- hb_prj_get_controller(auth, url, params, plural) # Download the data
  
  return(data)
}

#' Retrieve Highbond Results - Columns
#'
#' @description Gets the schema of a single table. 
#' 
#' @param table_id Table ID. Required.
#' 
#' @return A data frame column name and types of a single Table
#'
#' @inheritParams get_results_records
#' @export
get_results_columns <- function(auth, table_id){
  # Behaves differently than all the other endpoints, so OK to be custom
  
  component <- 'columns'
  primary <- table_id
  
  url <- paste0(hb_url(auth), hb_url_component(component, primary))
  plural <- TRUE # Data always nested in data
  
  params <- NULL # No parameters to send with results
  data <- hb_prj_get_controller(auth, url, params, plural) # Download the data
  
  return(data)
}

#' Create, Update or Delete Results - Collections
#'
#' @inheritParams get_results_records
#'
#' @param name The name of the Results object to create
#' @param collection_id The ID number of the collection
#' @param ... List(s) to add additional data
#'
#' @details Each endpoint has a list of required elements, as listed in the
#'   parameters.
#'
#'   For optional data, such as attributes, you may pass additional lists. The
#'   top level name of this optional list should match the name of the json
#'   level to be added, with key-value pairs within the list. Ultimately, this
#'   list object will be added as a child json level, under the 'data' top level
#'   json.
#'
#'   For example: `attributes` is a json is nested under data, and therefore
#'   should be created as the name of the object. Then additional information
#'   can be added to it as a key-value pair to the list.
#'
#' @return If creating or updating, a data frame with the new details. 
#'   When deleting, JSON response indicating success or failure.
#'   
#' @export
#'
#' @examples
#' \dontrun{
#' # Create and delete a Collection
#' auth <- setup_highbond(Sys.getenv('highbond_openapi'), 
#'   Sys.getenv('highbond_org'), 
#'   Sys.getenv('highbond_datacenter'))
#'   
#' name <- 'galvanizer Test Collection'
#' response <- create_results_collections(auth, name)
#' collection_id <- response$id[[1]]
#' response <- delete_results_collections(auth, collection_id)
#'
#' # Create a Collection with attributes
#' myattr <- list(description = 'My first description')
#' response <- create_results_collections(auth, name, attributes = myattr)
#' collection_id <- response$id[[1]]
#'
#' # Update a Collection with attributes
#' myattr <- list(name = 'galvanizer Super Test Collection', description = 'My second description')
#' response <- update_results_collections(auth, collection_id, attributes = myattr)
#'
#' # Delete a Collection
#' response <- delete_results_collections(auth, collection_id)
#' }
create_results_collections <- function(auth, name, ...){
  primary <- NULL
  secondary <- NULL 
  
  type <- hb_callingtarget() # Get the last word in the function name
  action <- hb_callingfunc() # Get the first word in the function name
  
  # Create the required elements of the payload
  payload <- hb_initpayload(type)
  payload$data$attributes$name <- name
  
  results_payload(auth, primary, secondary, type, action, payload, ...)
}

#' @describeIn create_results_collections Update a collection
#' @export
update_results_collections <- function(auth, collection_id, ...){
  primary <- collection_id
  secondary <- NULL 
  
  type <- hb_callingtarget() # Get the last word in the function name
  action <- hb_callingfunc() # Get the first word in the function name
  
  # Create the required elements of the payload
  payload <- hb_initpayload(type)
  payload$data$id <- primary
  
  results_payload(auth, primary, secondary, type, action, payload, ...)
}

#' @describeIn create_results_collections Delete a collection
#' @export
delete_results_collections <- function(auth, collection_id){
  primary <- collection_id
  secondary <- NULL 
  
  type <- hb_callingtarget() # Get the last word in the function name
  action <- hb_callingfunc() # Get the first word in the function name
  
  results_payload(auth, primary, secondary, type, action)
}

#' Create, Update or Delete Results - Analyses
#'
#' @inherit create_results_collections params details return
#'
#' @param analysis_id The ID number of the analyses
#' @export
create_results_analyses <- function(auth, collection_id, name, ...){
  primary <- collection_id
  secondary <- NULL 
  
  type <- hb_callingtarget() # Get the last word in the function name
  action <- hb_callingfunc() # Get the first word in the function name
  
  # Create the required elements of the payload
  payload <- hb_initpayload(type)
  payload$data$attributes$name <- name
  
  results_payload(auth, primary, secondary, type, action, payload, ...)
}

#' @describeIn create_results_analyses Update an analyses
#' @export
update_results_analyses <- function(auth, analysis_id, ...){
  primary <- NULL
  secondary <- analysis_id 
  
  type <- hb_callingtarget() # Get the last word in the function name
  action <- hb_callingfunc() # Get the first word in the function name
  
  # Create the required elements of the payload
  payload <- hb_initpayload(type)
  payload$data$id <- secondary
  
  results_payload(auth, primary, secondary, type, action, payload, ...)
}

#' @describeIn create_results_analyses Delete an analyses
#' @export
delete_results_analyses <- function(auth, analysis_id){
  primary <- NULL
  secondary <- analysis_id 
  
  type <- hb_callingtarget() # Get the last word in the function name
  action <- hb_callingfunc() # Get the first word in the function name
  
  results_payload(auth, primary, secondary, type, action)
}

#' Create, Update or Delete Results - Tables
#'
#' @inherit create_results_analyses params details return
#'
#' @param table_id The ID number of the table
#' @export
create_results_tables <- function(auth, analysis_id, name, ...){
  primary <- analysis_id
  secondary <- NULL 
  
  type <- hb_callingtarget() # Get the last word in the function name
  action <- hb_callingfunc() # Get the first word in the function name
  
  # Create the required elements of the payload
  payload <- hb_initpayload(type)
  payload$data$attributes$name <- name
  
  results_payload(auth, primary, secondary, type, action, payload, ...)
}

#' @describeIn create_results_tables Update an analyses
#' @export
update_results_tables <- function(auth, table_id, ...){
  primary <- NULL
  secondary <- table_id 
  
  type <- hb_callingtarget() # Get the last word in the function name
  action <- hb_callingfunc() # Get the first word in the function name
  
  # Create the required elements of the payload
  payload <- hb_initpayload(type)
  payload$data$id <- secondary
  
  results_payload(auth, primary, secondary, type, action, payload, ...)
}

#' @describeIn create_results_tables Delete an analyses
#' @export
delete_results_tables <- function(auth, table_id){
  primary <- NULL
  secondary <- table_id 
  
  type <- hb_callingtarget() # Get the last word in the function name
  action <- hb_callingfunc() # Get the first word in the function name
  
  results_payload(auth, primary, secondary, type, action)
}

#' Create or Delete Results - Columns
#'
#' \link[galvanizer]{post_results_records} will upload a data frame with the
#' correct specification for the fields uploaded. You can use this function to
#' add columns manually if preferred, especially if a non-standard column type.
#'
#' @details When creating columns, the argument will accept a data frame. This
#'   data frame must only have three columns - `field_name`, `display_name`, and
#'   `data_type.` See API for allowable `data_type.`
#'
#'   When deleting columns, the argument will accept a data frame. This data
#'   frame must only have one column - `field_name`
#'
#' @inherit create_results_analyses params details return
#'
#' @param table_id The ID number of the table
#' @param columns A data frame with columns to be added
#'
#' @export
#' @examples
#' \dontrun{
#'   auth <- setup_highbond(Sys.getenv('highbond_openapi'), 
#'     Sys.getenv('highbond_org'), 
#'     Sys.getenv('highbond_datacenter'))
#'     
#'   field_name <- c("a", "b", "c", "d", "e", "f", "g")
#'   display_name <- c("field_one", "field_two", "field_three", 
#'     "field_four", "field_five", "field_six", "field_seven")
#'   data_type <- c("character", "numeric", 'logical', 
#'     'date', 'datetime', 'file', 'digital_signature')
#'   columns <- data.frame(field_name, display_name, data_type)
#'   
#'   table_id <- 12345
#'   response <- create_results_columns(auth, table_id, columns)
#' }
#' 
create_results_columns <- function(auth, table_id, columns){
  # This URL is a real special snowflake
  # Due to 1) /bulk/ at end and 2) content_type being json and not vnd.api+json
  
  type <- hb_callingtarget() # Get the last word in the function name
  primary <- table_id
  
  # Future improvement - check columns contents
  payload <- list()
  payload$data <- columns
  payload <- jsonlite::toJSON(payload)
  
  url <- paste0(hb_url(auth), paste0('tables/', primary, '/', type, '/bulk'))
  
  # INCONSISTENCY IN API ENDPOINT - WATCH FOR REVISION TO VND.API+JSON
  hb_headers_col <- httr::add_headers(Authorization = paste("Bearer", auth$key),
                                  `Content-Type` = 'application/json',
                                  `Accept-Encoding` = '')
  
  response <- httr::POST(url,
                         body = payload,
                         hb_headers_col,
                         encoding = "raw")
  
  hb_validateDownload(response)
  
  json <- httr::content(response, 'text')
  content <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  parsed <- hb_parse_content(content, TRUE)
  
  return(parsed)
}

delete_results_columns <- function(auth, table_id, columns){
  # @describeIn create_results_columns Delete columns
  
  # This endpoint is returning a 500 error and I'm not sure why. Come back to it later
  # This endpoint is also a special snowflake
  
  type <- hb_callingtarget() # Get the last word in the function name
  primary <- table_id 
  
  # Future improvement - check columns contents
  payload <- list()
  payload$data <- columns
  payload <- jsonlite::toJSON(payload)
  
  url <- paste0(hb_url(auth), paste0('tables/', primary, '/', type, '/bulk'))
  
  # INCONSISTENCY IN API ENDPOINT - WATCH FOR REVISION TO VND.API+JSON
  hb_headers_col <- httr::add_headers(Authorization = paste("Bearer", auth$key),
                                      `Content-Type` = 'application/json',
                                      `Accept-Encoding` = '')
  
  #print(payload)
  
  response <- httr::DELETE(url,
                         body = payload,
                         hb_headers_col,
                         encoding = "raw")
  
  #hb_validateDownload(response)
  
  return(response)
}
