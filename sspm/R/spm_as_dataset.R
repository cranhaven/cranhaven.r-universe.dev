#' Create a `sspm_dataset` dataset structure
#'
#' This casts a `data.frame` or `sf` object into  an object of class
#' [`sspm_dataset`][sspm_dataset-class]. This object is the format the package
#' uses to manage and manipulate the modeling data.
#'
#' @param data **\[data.frame OR sf\]** The dataset.
#' @param time **\[character\]** The column of `data` for the temporal
#'     dimensions (i.e. year).
#' @param coords **\[character\]** The column of `data` for longitude and
#'     latitude of the observations.
#' @param biomass **\[character\]** Columns to be encoded as biomasses (required).
#' @param density **\[character\]** Columns to be encoded as densities (optionnal).
#' @param biomass_units **\[character\]** Units for biomass columns, default to "kg".
#' @param density_units **\[character\]** Units for density columns, default to "kg/km^2".
#' @param name **\[character\]** The name of the dataset, default to "Biomass".
#' @param uniqueID **\[character\]** The column of `data` that is unique for all
#'     rows of the data matrix.
#' @param crs Coordinate reference system, passed onto \link[sf]{st_as_sf}.
#' @inheritParams spm_smooth
#' @param ... Arguments passed onto methods.
#'
#' @examples
#' data(borealis_simulated, package = "sspm")
#' biomass_dataset <- spm_as_dataset(data.frame(borealis_simulated), name = "borealis",
#'                                   density = "weight_per_km2",
#'                                   time = "year_f",
#'                                   coords = c('lon_dec','lat_dec'),
#'                                   uniqueID = "uniqueID")
#' biomass_dataset
#'
#' @return
#' An object of class [`sspm_dataset`][sspm_dataset-class].
#'
#' @export
setGeneric(name = "spm_as_dataset",
           def = function(data, name, time, uniqueID, coords = NULL, ...) {

             assert_column(data, uniqueID)

             if (!(length(unique(data[[uniqueID]])) == nrow(data))) {
               stop("`uniqueID` must be unique for each row of `data`", call. = FALSE)
             }

             assert_column(data, time)

             if (!checkmate::test_factor(data[[time]])) {
               stop("`time` must be a factor", call. = FALSE)
             }

             standardGeneric("spm_as_dataset")
           }
)

# Methods -----------------------------------------------------------------

#' @rdname spm_as_dataset
#' @export
setMethod(f = "spm_as_dataset",
          signature(data = "data.frame", coords = "missingOrNULL"),
          function(data, name, time, uniqueID, coords, crs = NULL,
                   boundaries = NULL, biomass = NULL, density = NULL,
                   biomass_units = NULL, density_units = NULL) {

            stop("Argument `coords` must be provided when data matrix is a dataframe",
                 call. = FALSE)
          }
)

# If data.frame with coords, make it sf
#' @rdname spm_as_dataset
#' @export
setMethod(f = "spm_as_dataset",
          signature(data = "data.frame", coords = "list"),
          function(data, name, time, uniqueID, coords, crs = NULL,
                   boundaries = NULL, biomass = NULL, density = NULL,
                   biomass_units = "kg", density_units = "kg/km^2") {
            coords <- unlist(coords)
            spm_as_dataset(data, name, time, uniqueID, coords, crs, boundaries,
                           biomass, density, biomass_units, density_units)
          }
)


# If data.frame with coords, make it sf
#' @rdname spm_as_dataset
#' @export
setMethod(f = "spm_as_dataset",
          signature(data = "data.frame", coords = "character"),
          function(data, name, time, uniqueID, coords, crs = NULL,
                   boundaries = NULL, biomass = NULL, density = NULL,
                   biomass_units = "kg", density_units = "kg/km^2") {

            # Check coords
            if (!checkmate::test_subset(coords, names(data))) {
              stop("`coords` must be columns of `data`", call. = FALSE)
            }
            if (length(coords) != 2) {
              stop("`coords` must be of length 2", call. = FALSE)
            }

            # From a data.frame and coords, cast as sf (keep columns)
            info_message <-
              paste0(" Casting data matrix into simple feature collection using columns: ",
                     paste(cli::col_green(coords), collapse = ", "))
            cli::cli_alert_info(info_message)

            # TODO better CRS checks
            if (is.null(crs)) {
              info_message <-
                paste0(" Warning: sspm is assuming WGS 84 CRS is to be ",
                       "used for casting")
              cli::cli_alert_warning(info_message)
              crs <- sf::st_crs(4326)
            }

            new_data <- sf::st_as_sf(x = data, coords = coords, crs = crs,
                                     remove = FALSE)

            spm_as_dataset(new_data, name, time, uniqueID, coords, crs, boundaries,
                           biomass, density, biomass_units, density_units)
          }
)

# If sf, ingest as is
#' @rdname spm_as_dataset
#' @export
setMethod(f = "spm_as_dataset",
          signature(data = "sf", coords = "ANY"),
          function(data, name, time, uniqueID, coords, crs = NULL,
                   boundaries = NULL, biomass = NULL, density = NULL,
                   biomass_units = "kg", density_units = "kg/km^2") {

            # Cast columns
            data <- cast_special_variables(data, biomass = biomass,
                                           density = density,
                                           biomass_units = biomass_units,
                                           density_units = density_units)

            # Test if point
            if (any(sf::st_is(data, "POINT"))) {

              the_sspm_dataset <- new("sspm_dataset",
                                      name = name,
                                      data = data,
                                      biomass = biomass,
                                      density = density,
                                      time = time,
                                      uniqueID = uniqueID,
                                      coords = coords)

              if (!is.null(boundaries)){
                the_sspm_dataset <- join_datasets(the_sspm_dataset, boundaries)
              }

            } else if(any(sf::st_is(data, "POLYGON")) ||
                      any(sf::st_is(data, "MULTIPOLYGON"))) {

              if (!is.null(boundaries)){
                cli::cli_alert_info("polygon geometry, boundaries argument ignored")
              }

              # Extract all distinct patches and give them an ID
              patches <- data %>%
                dplyr::select("geometry") %>%
                dplyr::distinct() %>%
                dplyr::mutate(patch_id = paste("P", 1:dplyr::n(), sep = "")) %>%
                dplyr::mutate(patch_id =
                                factor(.data$patch_id, levels =
                                         paste0("P", 1:length(unique(.data$patch_id))))) %>%
                dplyr::mutate(boundary_col = "B1")

              # Fuse all patches in a single boundary object
              boundary_data <- patches %>%
                sf::st_union() %>%
                sf::st_as_sf() %>%
                dplyr::mutate(boundary_col = "B1") %>%
                dplyr::rename(geometry = "x")

              boundaries <- spm_as_boundary(boundaries = boundary_data,
                                            boundary = "boundary_col",
                                            patches = patches,
                                            points = NULL)

              the_sspm_dataset <- new("sspm_dataset",
                                      name = name,
                                      data = data,
                                      biomass = biomass,
                                      density = density,
                                      time = time,
                                      uniqueID = uniqueID,
                                      coords = coords,
                                      is_mapped = TRUE,
                                      boundaries = boundaries)

            } else {

              stop("sf object must be of type POINT or POLYGON to be sspm datasets")

            }

            return(the_sspm_dataset)
          }
)

# -------------------------------------------------------------------------

cast_special_variables <- function(df, biomass, density, biomass_units, density_units){

  checkmate::assert_character(biomass, null.ok = TRUE)
  checkmate::assert_character(density, null.ok = TRUE)
  checkmate::assert_character(biomass_units, null.ok = TRUE)
  checkmate::assert_character(density_units, null.ok = TRUE)

  if (!is.null(biomass)){
    for (b_var in biomass){
      df[[b_var]] <- set_biomass(df[[b_var]], units = biomass_units)
    }
  }

  if (!is.null(density)){
    for (d_var in density){
      df[[d_var]] <- set_biomass_density(df[[d_var]], units = density_units)
    }

  }

  return(df)
}
