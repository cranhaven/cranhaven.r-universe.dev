#Point Selector

# Point Selector ----------------------------------------------------------

#' PointSelector
#' 
#' A list of parameters to query metadata.
#' 
#' @returns An empty named list of possible point selection criteria.
#' 
#' @examples 
#' \dontrun{
#' query <- PointSelector()
#' 
#' query$buildings <- 101
#' query$equipment_types <- 'ahu'
#' query$point_types <- c('Supply Air Temperature','Supply Air Static Pressure')
#' }
#' 
#' @export
PointSelector <- function(){
  query <- list(orgs='',buildings='',point_ids='',point_names='',
                point_topics='',updated_since='',point_types='',
                equipment='',equipment_types='',point_hashes='')
  
  return(query)
}



# Select Points -----------------------------------------------------------

#' Select Points
#' 
#' Returns a list of ids based on the input query from PointSelector. Uses http POST call to query data.
#' 
#' @param query query supplied from PointSelector.
#' 
#' @return A named list of all the points requested by the query.
#' 
#' @examples 
#' \dontrun{
#' query <- PointSelector()
#' 
#' query$buildings <- 427
#' query$equipment_types <- 'ahu'
#' query$point_types <- c('Supply Air Temperature','Supply Air Static Pressure')
#' 
#' selection <- select_points(query)
#' }
#' 
#' @export
select_points <- function(query){
  
  if(query$updated_since!='') {
    updated_since <- as.numeric(
      as.POSIXct(query$updated_since))
  } else {
    updated_since<- ''
  }
  
  query <- query[names(query)!='updated_since']
  
  query_json <- query %>%
    toJSON()
  
  query_json <- gsub('\\[""\\]','[]',query_json)
  
  if(updated_since!=''){
    query_json <-
      gsub('}',paste0(',"updated_since":',updated_since,'}'),query_json)
  }
  
  endpoint <- 'points/select'
  
  point_selector_output <- api.post(endpoint,
                                    json_body=query_json)
  
  selection <- point_selector_output
  
  for(i in 1:length(point_selector_output)){
    selection[[i]] <- as.vector(unlist(point_selector_output[[i]])) 
  }
  
  return(selection)
}


# Points by ID ---------------------------------------------------------

#' Points by ID
#' 
#' Queries data points by their ids.
#' 
#' @param id Integer or list of integers. One or many point ids.
#' 
#' @return A data.frame of the requested points, or an empty list if there are no points with those ids.
#' 
#' @examples
#' \dontrun{
#' points <- get_points_by_ids(c(10000,10001))
#' 
#' # If you are using the point selector function:
#' query <- PointSelector()
#' 
#' query$buildings <- 101
#' query$equipment_types <- 'ahu'
#' query$point_types <- c('Supply Air Temperature','Supply Air Static Pressure')
#' 
#' selection <- select_points(query)
#' 
#' points <- get_points_by_ids(selection$points)
#' }
#' 
#' @export
get_points_by_ids <- function(id){
  
  id_unlist <- unlist(id)
  
  #Separate point ids into chunks of 500
  chunks <- split(id_unlist,
                  ceiling(seq_along(id_unlist)/500))
  
  points<-data.frame()
  
  for(i in 1:length(chunks)){
    
    point_ids <- toJSON(chunks[[i]])
    
    #URL encode JSON list of points
    point_ids <- URLencode(point_ids,reserved = T)
    
    endpoint <- paste0('points?point_ids=',point_ids)
    
    points_chunk <- api.get(endpoint)
    
    points <- plyr::rbind.fill(points,points_chunk)
  }
  
  return(points)
  
}


# Equipment by ID ---------------------------------------------------------

#' Equipment by ID
#' 
#' Queries equipment by their ids.
#' 
#' @param id Integer or integer vector, containing one or many equipment ids. 
#'
#' @return A data.frame of the requested equipment, or an empty list if no equipment matches those ids.
#' 
#' @examples
#' \dontrun{
#' equipment <- get_equipment_by_ids(c(1000,1001))
#' 
#' # If you are using the point selector function:
#' query <- PointSelector()
#' 
#' query$buildings <- 101
#' query$equipment_types <- 'ahu'
#' 
#' selection <- select_points(query)
#' 
#' equipment <- get_equipment_by_ids(selection$equipment)
#' }
#' 
#' @export
get_equipment_by_ids <- function(id){
  
  id_unlist <- unlist(id)
  
  #Convert list of ids to JSON payload
  id_json <- list(equipment_ids = id_unlist) %>%
    toJSON()
  
  equipment <- api.post(endpoint = 'equipment/query',
                        json_body = id_json,
                        output = 'dataframe')
  
  return(equipment)
}

