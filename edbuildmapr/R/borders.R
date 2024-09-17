#'@title A function to create a file of the borders between neighboring
#'  districts
#'@name borders
#'@description This function allows you to create a dataframe or linestring
#'  spatial object of the borders between neighboring districts from any polygon
#'  shapefile. It is optimized for school districts in the US, but any polygon
#'  shapefile can be used.
#'@param shapefile The polygon shapefile for which you want to define the
#'  borders. To import the school district shapefile for school years between
#'  2013 and 2019, input the four digit year. Import any polygon shapefile by
#'  inputting the absolute path to the shapefile on your computer. Defaults to
#'  the 2019 school district shapes.
#'@param state State name. Can only be used with the school district shapefile.
#'  Defaults to NULL to find all borders nationwide.
#'@param id Unique variable used to create id for each pair of neighbors.
#'  Defaults to GEOID, the unique id in Census data.
#'@param diff_var Name of a numeric variable by which to rank the difference
#'  between neighbors. Use diff_var = “options” to print a list of the
#'  variables. Defaults to "StPovRate", which returns the percentage point
#'  difference in Student Poverty Rate.
#'@param export The type of object to return, dataframe or shape. Default to
#'  dataframe.
#'@keywords school districts map EdBuild
#'@usage borders(shapefile = "2019", state = NULL, id = "GEOID", diff_var =
#'  "StPovRate", export = "dataframe")
#'@import dplyr sf tidyselect magrittr
#'@importFrom utils download.file unzip
#'@importFrom spdep poly2nb
#'@importFrom tibble obj_sum
#'@return A dataframe or spatial object where each observation is a neighboring
#'  pair of districts.
#'@format A data frame with 7 variables or spatial object with 8 variables:
#'  \describe{ \item{year}{data year} \item{u_id}{Unique id of neighbor pair, a
#'  compilation of id1 and id2} \item{id1}{Unique id of first district}
#'  \item{id2}{Unique id of second district} \item{length}{Length of border in
#'  meters for the school district shapefiles, and in the units associated with
#'  the projection of the shapefile if the user imports their own shapefile}
#'  \item{diff_var_1}{Value of the selected \code{diff_var} for the first
#'  district} \item{diff_var_2}{Value of the selected \code{diff_var}  for the
#'  second district} \item{diff_in_diff_var}{Difference in the selected
#'  \code{diff_var} between district one and two} \item{geography}{Linestring
#'  spatial object if user selected to export as a shape} }
#'@seealso \code{\link{sd_shapepull}}, \code{\link{sd_neighbor_map}}
#'@export
#'@examples
#' \donttest{dataframe_ex <- borders(shapefile = "2018",
#'                  state = "New Jersey",
#'                  id = "GEOID",
#'                  diff_var = "MHI",
#'                  export = "dataframe")


borders = function(shapefile = "2019", state = NULL, id = "GEOID", diff_var = "StPovRate", export = "dataframe") {

  if (shapefile == "2013" | shapefile == "2014" | shapefile == "2015" | shapefile == "2016" | shapefile == "2017" | shapefile == "2018" | shapefile == "2019") {
    shape_all_fields <- sd_shapepull(shapefile, with_data = TRUE)

    state_list <- shape_all_fields$State

   if (is.null(state) == TRUE) {
     message( "You have not specified a state so borders() will calculate all school district borders in the nation. This will take approximately 15-20 minutes to run.")
    }
   else if(state %in% state_list == FALSE) {
      message( "The state you specified is not available. Please check your spelling and try again.")
    }

    else {
      shape_all_fields %<>% filter(State == state)
    }
  }
  else {
    shape_all_fields <- sf::read_sf(shapefile)
  }

  shape <- shape_all_fields %>%
    dplyr::select(tidyselect::all_of(id), geometry)

  diff_var_variables <- shape_all_fields
  sf::st_geometry(diff_var_variables) <- NULL

  if(export != "shape" & export != "dataframe") {
    message("Error: Please input a correct value for export,  You can export the pairs list as a shapefile (export = 'shape'), or as a dataframe (export = 'dataframe'). ")
  }
  else if(tidyselect::all_of(diff_var) == "options") {
    message("Please see below for variables you can use to define neighbors.")
    print(names(diff_var_variables)) ## runs if diff_var is not in variable name list of pairs_data_diff
  }
  else if(!(tidyselect::all_of(diff_var) %in% names(diff_var_variables))) {
    message(paste0("Error: The difference variable you selected (", diff_var, "), is not included in the dataset. Please see below for variables you can use to define neighbors."))
    print(names(diff_var_variables)) ## runs if diff_var is not in variable name list of pairs_data_diff
  }

  else{

  #### clean this shapefile
  shape.clean <- sf::st_make_valid(shape) # making all geometries valid

  ifelse(grepl("POLYGON", tibble::obj_sum(shape.clean$geometry)),
         shape_sf <- shape.clean,
         shape_sf <- sf::st_collection_extract(shape.clean, "POLYGON")

  ) # taking just the polygons from the original shape data, in case it includes stray points or other

  touches <- spdep::poly2nb(shape_sf)  # finding neighbors - gets all the neighbors, using snapping  and queen which st_intersect does not do

  ##### interset each shape with its neghbor
  intersection <- lapply(1:length(touches), function(from) {
    sf::st_agr(shape.clean) = "constant"
    intersection_part <- sf::st_intersection(shape.clean[from,], shape.clean[touches[[from]],]) %>%
      dplyr::select(paste0(id, ""), paste0(id, ".1")) %>%
      dplyr::rename(ID1 = paste0(id, ""),
                    ID2 = paste0(id, ".1")) %>%
      dplyr::mutate(u_id = paste(ID1, ID2, sep = "_"))

    if (nrow(intersection_part) > 1) {
      ifelse(grepl("MULTILINESTR|LINESTR", tibble::obj_sum(intersection_part$geometry)) |
             inherits(st_geometry(intersection_part), c("sfc_MULTILINESTRING", "sfc_LINESTRING")),
             ### tibble::obj_sum only returns the first 15 characters so grepl only searches for first 15
             lineseg <- intersection_part,
             lineseg <- sf::st_collection_extract(intersection_part, "LINESTRING"))
    }
    else {
      ifelse(grepl("GEOMETRYCOL", tibble::obj_sum(intersection_part$geometry)) |
             !inherits(st_geometry(intersection_part), c("sfc_MULTILINESTRING", "sfc_LINESTRING")),
             ### tibble::obj_sum only returns the first 15 characters so grepl only searches for first 15
             lineseg <- sf::st_collection_extract(intersection_part, "LINESTRING"),
             lineseg <- intersection_part)
    }

    lineseg <- sf::st_cast(lineseg, "MULTILINESTRING", ids = lines$u_id) %>%
      dplyr::mutate(length = sf::st_length(lineseg))
  })


  #### remove all entries in the list with 0 number of rows
  lines <- Filter(nrow, intersection)

  line_pairs <- dplyr::bind_rows(lines)

  colnames(line_pairs)[1] <- "id1"
  colnames(line_pairs)[2] <- "id2"

  pairs <- line_pairs %>%
    dplyr::group_by(u_id) %>%
    dplyr::summarise(id1 = first(id1),
                     id2 = first(id2),
                length = round(sum(length), 0))

  if (!is.null(tidyselect::all_of(diff_var))) {
    if(tidyselect::all_of(diff_var) %in% names(diff_var_variables) == TRUE) {
      shape_data <- shape_all_fields %>%
        dplyr::select(paste0(id, ""), tidyselect::all_of(diff_var))
      sf::st_geometry(shape_data) <- NULL
      colnames(shape_data)[1] <- "data_id"
      pairs_data_diff <- pairs %>%
        dplyr::left_join(shape_data, by = c("id1" = "data_id")) %>%
        dplyr::left_join(shape_data, by = c("id2" = "data_id"))
      colnames(pairs_data_diff)[6] <- "data_1"
      colnames(pairs_data_diff)[7] <- "data_2"
      pairs_data_diff <- pairs_data_diff %>%
        dplyr::mutate(data_diff = data_1 - data_2)
      colnames(pairs_data_diff)[6] <- paste0(tidyselect::all_of(diff_var), "_1")
      colnames(pairs_data_diff)[7] <- paste0(tidyselect::all_of(diff_var), "_2")
      pairs_data_diff <- pairs_data_diff %>%
        dplyr::filter(dplyr::case_when(data_diff > 0 ~ data_diff > 0, ## filter reciprocals based on diff_var first
                         data_diff == 0 ~ as.numeric(id1) > as.numeric(id2), ### then if diff_var is the same across neighbors, filter by larger GEOID
                         TRUE ~ data_diff > 0)) %>%
        dplyr::arrange(desc(data_diff)) ### get() converts the diff_var from string to variable name
      colnames(pairs_data_diff)[8] <- paste0(tidyselect::all_of(diff_var), "_diff")

      if (export == "dataframe") {
        pairs_df <- pairs_data_diff
          sf::st_geometry(pairs_df) <- NULL
        return (pairs_df)
      }
      else if (export == "shape")  {
        return (pairs_data_diff)
      }
      else {
        message("Error: You can only export the pairs list as a shapefile (export = 'shape'), or as a dataframe (export = 'dataframe').")
      }
    }

    else {
      message(paste0("Error: The difference variable you selected (", tidyselect::all_of(diff_var), "), is not included in the dataset. Please see below for variables you can use to define neighbors."))
      print(names(diff_var_variables)) ## runs if diff_var is not in variable name list of pairs_data_diff
    }
  }

  else {
    if (export == "dataframe") {
      pairs_df <- pairs
      sf::st_geometry(pairs_df) <- NULL
      return (pairs_df)
    }
    else if (export == "shape") {
      return(pairs)
    }
    else {
      message("Error: Please input a correct value for export,  You can export the pairs list as a shapefile (export = 'shape'), or as a dataframe (export = 'dataframe'). ")
    }
  }
  }
}

