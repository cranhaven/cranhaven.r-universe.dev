#datamodel

# Equipment Types --------------------------------------------------------------

#' Equipment Types
#' 
#' Query all equipment types from Onboard's Data Model.
#' 
#' @returns A data.frame containing all equipment types.
#' @export
get_equip_types <- function(){
  
  equiptype <- api.get('equiptype')
  
  subtypes <- sapply(equiptype$sub_types, as.data.frame)
  subtypes <- data.table::rbindlist(subtypes)
  
  equip_types <- equiptype %>%
    filter(.data$active == T) %>%
    select(-c(.data$sub_types, .data$critical_point_types, .data$flow_order, .data$active)) %>%
    left_join(subtypes,by = c('id' = 'equipment_type_id'),
              suffix = c('','_subtype'))
  
  return(equip_types)
}

# Point Types -------------------------------------------------------------

#' Point Types
#' 
#' Queries all point types, measurements and their units from Onboard's Data Model and returns a clean output.
#' 
#' @return A data.frame containing all point types.
#' 
#' @export
get_point_types <- function(){
  
  pointtypes <- api.get('pointtypes')
  
  measurements <- api.get('measurements')
  
  # Get measurements and their associated units
  units <- data.frame()
  
  for (i in 1:nrow(measurements)){
    row_num =i
    
    measurement_single <- measurements[row_num,]
    
    measurement_units <- measurement_single$units
    measurement_units <- data.table::rbindlist(measurement_units)
    measurement_units[,'measurement_id']<- measurement_single$id
    
    units <- plyr::rbind.fill(units,measurement_units)
  }
  
  units <- units %>%
    select(.data$measurement_id,
           unit_name=.data$name_long,
           unit=.data$name_abbr,.data$data_type) %>%
    mutate(measurement_id=as.integer(.data$measurement_id))
  
  measurements_units <- left_join(measurements,
                                  units,
                                  by=c('id' = 'measurement_id')) %>%
    select(id,
           measurement_name=.data$name,
           .data$units_convertible,
           .data$qudt_type,
           .data$unit_name,
           .data$unit,
           .data$data_type)
  
  #Unite data frames
  point_types <- left_join(select(
    pointtypes, .data$id, .data$tag_name, .data$measurement_id, .data$tags),
    measurements_units,
    by = c("measurement_id" = 'id')) %>%
    mutate(across(.data$tags,  ~ gsub('c\\(|\\)', '', .))) %>%
    select(.data$id,
           point_type = .data$tag_name,
           .data$measurement_name,
           .data$unit,
           .data$data_type,
           .data$tags)
  
  return(point_types)
  
}

