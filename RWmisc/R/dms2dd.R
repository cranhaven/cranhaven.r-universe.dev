#' Convert from degrees, minutes, and seconds to decimal degrees
#'
#' Convert latitudes and longitudes from degrees, minutes, and seconds to
#' decimal degrees for conversion to spatial objects.
#'
#' @name dms2dd
#' @param lon a character vector of longitude coordinates in degrees, minutes,
#' and seconds; see `details`
#' @param lat a character vector of latitude coordinates in degrees, minutes,
#' and seconds; see `details`
#'
#' @return An \eqn{n * 2} matrix where *n* is the length of `lon` and `lat`.
#'
#' @details `lon` and `lat` are expected to be in the format
#' `"degrees° minutes' seconds" (direction)"` where `direction` is optional. If
#' `direction` is not present, `dms2dd` will use negative signs (`-`) to
#' determine positioning of coordinates.
#'
#' @import units
#'
#' @export
#'
#' @examples
#' ll <- data.frame(lon = c("-122° 19' 55\"",
#'                          "71° 3' 32\" W"),
#'                  lat = c("47° 36' 22\"",
#'                          "42° 21' 36\" N"),
#'                  stringsAsFactors = FALSE)
#' dms2dd(ll[, 'lon'], ll[, 'lat'])

dms2dd <- function(lon, lat) {

  if (length(lon) != length(lat)) {
    stop('latitude and longitudes must be the same length')
  }

  mat <- matrix(nrow = length(lon), ncol = 2,
                dimnames = list(1:length(lon), c('lon', 'lat')))

  for (i in 1:length(lon)) {

    coords <- strsplit(c(lon[i], lat[i]), '\\s')

    card_dir <- vapply(coords, function(x) length(x) == 4, FUN.VALUE = TRUE)

    if (all(card_dir)) {

      east <- ifelse(grepl('E', coords[[1]][4]), 1, -1)
      north <- ifelse(grepl('N', coords[[2]][4]), 1, -1)

    } else if (sum(card_dir) == 1) {

        stop(paste0('Cooordinates not in unambiguous format; ',
                   'each coordinate pair must use either N, E, S, W or ',
                   'positive/negative degrees\n',
                   'coordinates: ', paste(c(lon[i], lat[i]), collapse = ', ')))

    } else {

      east <- 1
      north <- 1

    }

    mat[i, 1] <- (set_units(set_units(as.numeric(sub("\u00b0|deg|degree", "",
                                                     coords[[1]][1])),
                                      'arc_degree'),
                            'degree_east') +
                    set_units(set_units(as.numeric(sub("'", "",
                                                       coords[[1]][2])),
                                        'arc_minute'),
                              'degree_east') +
                    set_units(set_units(as.numeric(sub("\"", "",
                                                       coords[[1]][3])),
                                        'arc_second'),
                              'degree_east')) * east

    mat[i, 2] <- (set_units(set_units(as.numeric(sub("\u00b0|deg|degree", "",
                                                     coords[[2]][1])),
                                      'arc_degree'),
                            'degree_north') +
                    set_units(set_units(as.numeric(sub("'", "",
                                                       coords[[2]][2])),
                                        'arc_minute'),
                              'degree_north') +
                    set_units(set_units(as.numeric(sub("\"", "",
                                                       coords[[2]][3])),
                                        'arc_second'),
                              'degree_north')) * north


  }

  return(mat)

}
