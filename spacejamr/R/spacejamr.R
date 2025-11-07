# Author: Darren Colby
# Date: 2/3/2022
# Purpose: To create a basic class and methods for "spacejamr" objects

# Constructor methods -----------------------------------------------------


#' validate_spacejamr
#'
#' @description Validates the input to the spacejamr constructor.
#'
#' @usage validate_spacejamr(path, guess_crs = TRUE)
#'
#' @details This function should not be called directly
#'
#' @param path the path to a shapefile as a string.
#'
#' @param guess_crs whether to try to guess the best coordinate reference system
#' for the supplied shapefile
#'
#' @return A list containing a window object of class "owin" and the crs.
#' @author Darren Colby
#' Email: dscolby17@gmail.com
#' @noRd
validate_spacejamr <- function(path, guess_crs) {

   stopifnot(is.character(path))

   shapefile <- sf::read_sf(path)  # Raw shapefile

   # Sets the CRS if it is not already set
   if (guess_crs) {

      # In case an appropriate CRS is not found
      try({

         # Best projected crs
         suggested_crs <- crsuggest::suggest_top_crs(shapefile)

         # Transform shapefile to the best projected coordinate reference system
         transformed_shapefile <- sf::st_transform(shapefile, suggested_crs)

         # Set the crs in the shapefile
         transformed_shapefile <- suppressWarnings(sf::st_set_crs(shapefile,
                                                                  suggested_crs))

         window <- spatstat.geom::as.owin(transformed_shapefile)

      })



   } else { # Case when the CRS is already set or appropriate CRS is not found

      window <- spatstat.geom::as.owin(window)

   }


   return_list <- list(window = window, crs = suggested_crs)

   return(return_list)

}


#' new_spacejamr
#'
#' @description Create a new spacejamr object
#'
#' @usage new_spacejamr(path, guess_crs = TRUE)
#'
#' @details This function should not be called by the user
#'
#' @param path the path to a shapefile as a string.
#'
#' @param gues_crs whether to try to guess the coordinate reference system of
#' the supplied shapefile
#'
#' @return a spacejamr object containing two items. window: object of class
#' 'owin' that stores geographical boundaries. crs: integer value referring to
#' the coordinate reference system of the geographical boundaries.
#'
#' @author Darren Colby
#' Email: dscolby17@gmail.com
#' @noRd
new_spacejamr <- function(path, guess_crs) {

   # Validate the input
   validated_shapefile <- validate_spacejamr(path, guess_crs)

   # Set the class
   spacejamr_object <- structure(validated_shapefile,
                                 class = c("spacejamr", "owin"))

   return(spacejamr_object)

}


#' Initialize a new spacejamr object
#'
#' @description Creates a new spacejamr object that for further analysis
#'
#' @usage as.spacejamr(path, guess_crs = TRUE)
#'
#' @details The returned spacejamr object will contain a window object
#' containing a geographical boundary and its coordinate reference system.
#' Since any simulated point process will be simulated in two dimensions, the
#' coordinate reference system of the supplied shapefile should be a projected
#' coordinate reference system or 'guess_crs' should be set to TRUE. In that
#' case, the coordinate reference system will be set to the most appropriate
#' coordinate reference system for the shapefile. Note that guessing the
#' coordinate reference system will take longer than if one is already set.
#'
#' @param path the path to a shapefile as a string.
#'
#' @param guess_crs whether to try to guess the coordinate reference system of
#' the supplied shapefile
#'
#' @return a spacejamr object containing two items. window: object of class
#' 'owin' that stores geographical boundaries. crs: integer value referring to
#' the coordinate reference system of the geographical boundaries.
#'
#' @examples \donttest{
#' ri <- as.spacejamr(system.file("shape/ri.shp", package = "spacejamr"))
#' }
#'
#' @author Darren Colby \cr
#' Email: dscolby@@gmail.com
#' @export
as.spacejamr <- function(path, guess_crs = TRUE) {

    # Call the new_spacejamr constructor method
    spacejamr_object <- new_spacejamr(path, guess_crs)

    return(spacejamr_object)

}


# Plot, print, and summary methods ----------------------------------------


#' Plot the spatial extent of a spacejamr object
#'
#' @description Plot method for the spacejamr class
#'
#' @param x an object of class spacejamr.
#' @param y ignored.
#' @param ... ignored.
#' @param title an optional title for the plot. Default is "Spatial Window".
#' @param fill an optional fill for the plot. Default is blue.
#'
#' @details The returned plot can be refined with standard ggplot2 functions
#'
#' @return A plot of classes 'gg' and 'ggplot'
#'
#' @examples
#' # Load spacejamr object
#' data("RI")
#'
#' plot(RI)
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#' @export
plot.spacejamr <- function(x, y, ..., title = "Spatial Window", fill = "blue") {

   # Generate an sf object from the window
   window <- sf::st_as_sf(x[[1]])

   plot <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = window,
                       fill = fill) +
      ggplot2::labs(title = title) +
      ggthemes::theme_map()

   return(plot)

}


#' Print information from a spacejamr instance
#'
#' @description Print method for the spacejamr class
#'
#' @param x a spacejamr object
#' @param ... ignored.
#'
#' @details Provides a wrapper for the print.owin method in the spatstats.geom
#' package.
#'
#' @return No return value, called for side effects
#'
#' @examples
#' # Load spacejamr object
#' data("RI")
#'
#' print(RI)
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#'
#' @export
print.spacejamr <- function(x, ...) {

   spatstat.geom::print.owin(x[[1]])  # Only use the window data

}


#' Print summary information of a spacejamr instance
#'
#' @description Summary method for the spacejamr class
#'
#' @param object a spacejamr object
#' @param ... ignored.
#'
#' @details Provides a wrapper for the summary.owin method in the spatstats.geom
#' package.
#'
#' @return No return value, called for side effects
#'
#' @examples
#' # Load spacejamr object
#' data("RI")
#'
#' summary(RI)
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#'
#' @export
summary.spacejamr <- function(object, ...) {

   spatstat.geom::summary.owin(object[[1]])  # Only the window data

}

