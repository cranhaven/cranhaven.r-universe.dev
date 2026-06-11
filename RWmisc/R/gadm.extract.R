#' Extract layers by country from GADM GeoPackage file
#'
#' Extract one or more levels of administrative unit geometries from the GADM
#' database in GeoPackage format
#'
#' @param input GeoPackage file to read from
#' @param output name of file to save output to
#' @param countries country or countries to limit results to, if `NULL` returns
#' all countries
#' @param level level(s) of administrative units 0:5 to extract; note not all
#' levels are defined for all countries
#' @param ... additional arguments passed to [sf::st_write()]
#'
#' @details This function is designed to extract subsets of the
#' [Database of Global Administrative Areas (GADM)](https://gadm.org/). It uses
#' the version of the database in GeoPackage format that provides one layer for
#' each level of administrative division, available at
#' [https://gadm.org/download_world.html](https://gadm.org/download_world.html).
#' The current version of this file is `gadm36_levels.gpkg`. It is intended
#' for programmatic and reproducible subsetting of the database without
#' requiring the user to individually download specific country data files.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## extract
#' gadm.extract("gadm36_levels.gpkg", "Nordics.gpkg",
#'              c("Denmark", "Finland", "Iceland", "Norway", "Sweden"),
#'              level = 0:2)
#'
#' ## add layers 3 and 4, use delete_layer = TRUE to rewrite existing layers
#' gadm.extract("gadm36_levels.gpkg", "Nordics.gpkg",
#'              c("Denmark", "Finland", "Iceland", "Norway", "Sweden"),
#'              level = 0:4, delete_layer = TRUE)
#' }

gadm.extract <- function(input, output, countries = NULL, level = 0:5, ...) {

  ## add file extension to output if missing
  if (!grepl('^.*\\.gpkg$', output)) output <- paste0(output, '.gpkg')

  if (is.null(countries)) {

    for (i in level) {

      ## extract level(s) for all countries
      st_write(st_read(input, layer = paste0('level', i)),
               output, layer = paste0('level', i), ...)

    }

  } else {

    for (i in level) {

      ## extract level(s) for specified countries
      st_write(st_read(input, layer = paste0('level', i),
                       query = paste0('SELECT * FROM ',
                                      paste0('level', i),
                                      ' WHERE NAME_0 IN ("',
                                      paste0(countries, collapse = '", "'),
                                      '")')),
               output, layer = paste0('level', i), ...)

    }

  }

}
