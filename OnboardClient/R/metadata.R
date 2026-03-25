# Building Metadata 


# Building Info -----------------------------------------------------------

#' Building Info
#' 
#' Retrieves building id(s) and name(s). Assigns each to list variables in the parent environment called "id" and "name", and prints each list.
#' 
#' @param buildings Integer, character, or vectors of those types, providing building id(s) or name(s). You can provide multiple buildings at once.
#' 
#' @param verbose Logical. If TRUE (default), print status messages.
#'
#' @return A data.frame of building info with two columns, 'id' and 'name'.
#' 
#' @export
get_building_info <- function(buildings, verbose = TRUE){
  
  all_buildings <- get_buildings()
  
  building_info <- data.frame()
  
  for(i in 1:length(buildings)){
    
    single <- buildings[i]
    
    building <- all_buildings[all_buildings$name == single,]
    
    if(nrow(building) == 0) {
      
      building <- all_buildings[all_buildings$id == single, ]
      
      if (nrow(building) == 0) {
        stop(sprintf('No building found for name/id: %s.',single))
      }
    }
    
    building <- select(building, .data$id, .data$name)
    
    building_info <- rbind(building_info, building)
    
  }
  
  if(verbose){
    cat(sprintf('Found building(s): %s...\n',
                  paste(building_info$name,collapse=', ')))
  }

  
  return(building_info)
}

# Metadata ----------------------------------------------------------------


#' Metadata
#' 
#' Retrieves points and equipment for a given building or selection and outputs a clean metadata data.frame.
#' 
#' @inheritParams get_building_info
#' 
#' @param selection Selection list from point selector.
#' 
#' @return A data.frame of clean metadata for the requested points.
#' 
#' @examples 
#' \dontrun{
#' metadata <- get_metadata(buildings=c(427,"Laboratory"))
#' 
#' OR
#' 
#' query <- PointSelector()
#' 
#' query$buildings <- 427
#' query$equipment_types <- 'ahu'
#' query$point_types <- c('Supply Air Temperature','Supply Air Static Pressure')
#' 
#' selection <- select_points(query)
#' 
#' metadata <- get_metadata(selection)
#' }
#' 
#' @export
get_metadata <- function(buildings, selection, verbose = TRUE){
  
  if(missing(selection) & missing(buildings)){
    stop('Provide either building name/id or selection list.')
  } else if (missing(selection)){
    
    building_info <- get_building_info(buildings, verbose = verbose)
  
    query <- PointSelector()
    
    query$buildings <- building_info$id
    
    selection <- select_points(query)
  }
  
  if(length(selection$points)==0){
    stop('No metadata found.')
  }
  
  point_ids <- selection$points
  equipment_ids <- selection$equipment
  
  if(verbose){
    cat(sprintf('Querying %s points...\n',length(point_ids)))
  }
  
  points <- get_points_by_ids(point_ids)
  
  if(verbose){
    cat(sprintf('Querying %s equipment...\n',length(equipment_ids)))
  }

  equipment <- get_equipment_by_ids(equipment_ids)
  
  #Create a metadata for the specified building ID
  metadata <- inner_join(equipment,points,
                         by = c('id' = 'equip_id'),
                         suffix = c('', '.y')) %>%
    #Get tagged units if NA
    mutate(across(.data$tagged_units,
                  ~ifelse(is.na(.),
                          units,as.character(.)))) %>%
    #Replace equip_type tag with subtype tag if present
    mutate(across(.data$equip_type_tag,
                  ~ifelse(is.na(equip_subtype_tag),
                          .,equip_subtype_tag))) %>%
    select(
      .data$building_id,
      equipment_id = .data$id,
      point_id = .data$id.y,
      .data$device,
      .data$objectId,
      .data$name,
      .data$description,
      .data$first_updated,
      .data$last_updated,
      .data$value,
      .data$tagged_units,
      point_type = .data$type,
      equip_type = .data$equip_type_tag,
      .data$suffix,
      .data$equip_id,
      .data$parent_equip,
      .data$floor_num_physical,
      .data$floor_num_served,
      .data$area_served_desc,
      .data$topic
    ) %>%
    # Grab Equip Refs by joining with Equip DB again
    mutate(parent_equip = as.integer(.data$parent_equip)) %>%
    left_join(
      select(equipment, id, .data$equip_id),
      by = c('parent_equip' = 'id'),
      suffix = c('', '.y')
    ) %>%
    select(everything(),
           equip_ref = .data$equip_id.y,
           -.data$parent_equip) %>%
    #Convert unix time-stamps to EST
    mutate(across(c(.data$first_updated, .data$last_updated),
                  ~ as_datetime(as.numeric(substr(., 1, 10)))))
  
  if(verbose){
    cat('Metadata generated.')
  }

  return(metadata)
}