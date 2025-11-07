#' @title Create Plot Data
#' 
#' @description If data is provided, this functions attempts to merge the provided data with the geom-data 
#' inherent in the package on the chosen plotlevel. If no data is provided, only the geom-data for 
#' the chosen plotlevel is returned. This data is used to create the DK-plot. 
#' 
#' @inheritParams plotDK
#' @param ... Further arguments to pass to merge_data
#'
#' @return A \code{data.frame} with either raw geom-data or geom-data merged with any data provided.


create_plot_data <- function(data, 
                             id, 
                             plotlevel, 
                             show_missing = FALSE,
                             ...) {
  if(is.null(data)) {
    get_geom_data(plotlevel)
  } else {
    merge_data(
      data = data,
      id = id, 
      plotlevel = plotlevel, 
      show_missing = show_missing
    )
  }
}


#' @noRd
merge_data <- function(data, 
                       id, 
                       plotlevel, 
                       ...) {
  
  geom_data <- get_geom_data(plotlevel)
  
  joined_data <- join_input_geom_data(
    geom_data = geom_data, 
    data = data, 
    id = id, 
    ...
  )
  
  joined_data
}



#' @noRd
#' @importFrom purrr set_names
join_input_geom_data <- function(geom_data, 
                                data, 
                                id, 
                                show_missing) {
  
  
  join_var <- get_join_var(
    data,
    id
  )
  
  if(is.factor(data[[id]])) {
    data[[id]] <- as.character(data[[id]])
  }
  
  if(is.character(data[[id]])) {
    data[[id]] <- tolower(data[[id]])
  }
  
  join_fun(
    geom_data = geom_data,
    data = data,
    show_missing = show_missing,
    by = set_names(id, join_var)
  )
}

#' @noRd
get_join_var <- function(data,
                         id) {
  
  if(is.numeric(data[[id]])) {
    join_var <- "id_numeric"
  } else if (is.character(data[[id]])) {
    join_var <- "id"
  }
  
  join_var
}


#' @noRd
#' @importFrom dplyr left_join inner_join
join_fun <- function(geom_data, data, show_missing, ...) {
  if(show_missing) {
    left_join(geom_data, data, ...)
  } else {
    inner_join(geom_data, data, ...)
  }
}

#' @noRd
get_geom_data <- function(plotlevel) {
  if (!plotlevel %in% c("province", "region", "municipality", "zipcode")) {
    stop(
      "Plotlevel must be either: 'province', 'region',  'municipality' or 'zipcode'."
    )
  }
  
  switch(
    plotlevel,
    "region"  = plotDK::region,
    "province" = plotDK::province,
    "municipality" = plotDK::municipality,
    "zipcode" = plotDK::zipcodes
  )
}