# Staging Area --------------------------------------------------------

#' Get Staged Data
#' 
#' Gets metadata from the staging area.
#' 
#' @param building Character vector or integer corresponding to the building name or id. If you enter multiple building ids or names, only the first entry is considered.
#' 
#' @param verbose Logical. If TRUE (default), prints status and progress messages.
#' 
#' @return A data.frame of metadata from the staging area.
#' 
#' @export
get_staged_data <- function(building, verbose = TRUE){
  
  if(length(building)>1){
    stop('Length of building parameter greater than 1. Enter only one building id or name')
  }

  building_info <- get_building_info(building, verbose = verbose)

  if(verbose){
    cat('Querying staging data...\n')
  }


  endpoint <- paste0('staging/',building_info$id,'?points=True')

  stage <- api.get(endpoint)
  
  if(length(stage$points_by_equip_id) == 0){
    stop(sprintf('Staged data not found for building %s.', building_info$name))
  }

  #Equip Data
  
  equip_data <- stage$equipment
  equip_data_names <- names(equip_data)
  equip_data_names <- gsub('data\\.','', equip_data_names)
  equip_data_names <- paste0('e.', equip_data_names)
  names(equip_data) <- equip_data_names

  equip_data_names <- data.frame(names=names(equip_data)) %>%
    filter(!grepl('confidences|\\.cnf|auto_tagger|\\.err|type_id|tags|created|guid',
                  names))

  equip_data <- equip_data %>%
    select(equip_data_names$names)

  #Points Data

  points_list <- stage$points_by_equip_id

  points_data <- data.table::rbindlist(points_list, fill = TRUE)

  points_data_names <- names(points_data)
  points_data_names <- gsub('data\\.','',points_data_names)
  points_data_names <- paste0('p.',points_data_names)
  names(points_data) <- points_data_names

  rem_col <- paste('auto_tagger','.cnf','polarity','reliability',
                   'activeText','event','covIncrement','presvalue',
                   'statusflags','outofservice','unit_id','type_id',
                   'datasource','limit','deadband','@prop',
                   'timedelay',
                   'notif','acked','resolution','state_text',
                   'relinquish','priority','p\\.e\\.','confidences',
                   'check','created',sep='|')

  points_data_names <- data.frame(names=points_data_names) %>%
    filter(!grepl(rem_col,names,ignore.case=T))

  points_data<- points_data %>%
    select(points_data_names$names)


  staged_data <- left_join(equip_data,
                           points_data,
                           by = c('e.equip_id' = 'p.equip_id')) %>%
    mutate(across(c(.data$e.last_promoted, .data$p.last_promoted,
                    .data$e.modified, .data$p.modified),
              ~ as_datetime(as.numeric(substr(., 1, 10)),
                            tz = 'America/New_York'))) %>%
    select(sort(tidyselect::peek_vars()))
  
  if(verbose){
    cat('Staging data created.')
  }

  return(staged_data)

}

##Upload data to the staging area or assign __SKIP__ equip_id to topics on the staging area
##skip_topics is optional (T or F)(Use with Caution)

#' Upload to Staging Area
#' 
#' Uploads data to the staging area.
#' 
#' @inheritParams get_staged_data
#' 
#' @param data_to_upload A data.frame to upload to the staging area. Must contain e.equip_id and p.topic columns.
#' 
#' @param skip_topics Logical. If True, the uploaded topics will be assigned `__SKIP__` equip_id.
#'
#' @return A named list containing any errors that may have occured during data upload.
#'  
#'@export
upload_staging <- function(building,
                           data_to_upload,
                           skip_topics = FALSE,
                           verbose = TRUE
                           ){

  building_info <- get_building_info(building, verbose = verbose)

  if(missing(data_to_upload)) {

    stop('data_to_upload is missing in the function call. data_to_upload should be a dataframe including at least e.equip_id & p.topic for the upload to succeed')

  }else if (!('p.topic' %in% names(data_to_upload))) {
    stop('p.topic column not found in staging_upload.')
   }

  if(skip_topics == TRUE){
    operation <- 'skipping'
    
    data_to_upload <- data_to_upload %>%
          transmute(e.equip_id = '__SKIP__', 
                    .data$p.topic)
    
  } else {
    operation <- 'uploading'
  }

  data_to_upload_json <- data_to_upload %>%
    toJSON()

  proceed <- askYesNo(sprintf('Do you want to proceed %s %s topic/s for %s?', 
                              operation,nrow(data_to_upload), building_info$name))

  if(is.na(proceed) | proceed != TRUE){
    stop('Stopping Operation.')
  }

  if(verbose){
    cat(sprintf('%s topics...\n', operation))
  }

  #get endpoint
  endpoint <- paste0('staging/', building_info$id)

  post_points <- api.post(endpoint,
                          json_body = data_to_upload_json)
  
  output <- post_points$row_errors

  if(length(output)==0){
    message <- "Upload successful \n"
  } else{
    message <- "Upload unsuccesful. Please check errors for more information. \n"
  }
  
  if(verbose){
    cat(message)
  }
  
  if(length(output) != 0){
  return(output)
}
}

#' Promote data on Staging Area
#' 
#' Promote valid data on the staging area to the live building.
#' 
#' @inheritParams get_staged_data
#' 
#' @param data_to_promote (Optional) If missing, all valid topics are promoted. A data.frame containing columns 'e.equip_id' & 'p.topic'.
#' 
#' @return A named list containing any errors that may have occured during data promotion.
#' 
#' @export

promote_staged_data <- function(building, data_to_promote, verbose = TRUE){

  building_info <- get_building_info(building, verbose = verbose)
  
  if(missing(data_to_promote)){

    proceed <- askYesNo(
      sprintf('Do you want to proceed with promoting all valid topics for %s?', building_info$name)
      )

    if(is.na(proceed) | proceed != TRUE){
      stop('Stopping Operation.')
    }

      promote_json <- list(equip_ids='',
                           topics='') %>%
        toJSON()

      promote_json <- gsub('\\["', '[', promote_json)
      promote_json <- gsub('"\\]', ']', promote_json)

      operation <- 'promote_all'

    } else {

      data_to_promote <- data_to_promote %>%
        filter(.data$e.equip_id != '__SKIP__')

      equip_count <-length(unique(data_to_promote$e.equip_id))

      proceed <-
        askYesNo(
          sprintf('Do you want to proceed with promoting %s equipment and their valid topics to %s?', equip_count, building_info$name))

      if(is.na(proceed)|proceed != TRUE){
        stop('Stopping Operation.')
      }

        promote_json <- list(equip_ids = data_to_promote$e.equip_id,
                             topics = data_to_promote$p.topic) %>%
          toJSON()

        operation <- 'promote_some'
    }

  endpoint <- paste0('staging/', building_info$id, '/apply')

  promote_data <- api.post(endpoint,
                           json_body = promote_json)

  output <- promote_data[-1]
  
  return(output)
}