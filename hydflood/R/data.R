#' @name df.pnv
#' @rdname df.pnv
#' @title Reference \code{data.frame} used to classify flood duration into
#'    potential natural vegetation.
#' 
#' @description Reference \code{data.frame} used to classify flood duration into
#'    potential natural vegetation (PNV). It is an extended and more detailled
#'    table to reclassify flood duration into PNV based on Ochs et al. (2020).
#' 
#' @format A \code{data.frame} containing 7 columns with attributes to
#'    reclassify flood duration into potential natural vegetation.
#'    \describe{
#'       \item{from}{lower limits of flood duration (included, type \code{numeric}).}
#'       \item{to}{upper limits of flood duration (not included, type \code{numeric}).}
#'       \item{id}{numeric replacements used to sort classes (type \code{numeric}).}
#'       \item{vegtype}{names of the potential natural vegetation classes (type \code{character}).}
#'       \item{r}{numeric coding for the r (red) of an rgb color code.}
#'       \item{g}{numeric coding for the g (green) of an rgb color code.}
#'       \item{b}{numeric coding for the b (blue) of an rgb color code.}
#'       \item{html}{html color coding (type \code{character}).}
#'    }
#' 
#' @references 
#'   \insertRef{ochs_potential_2020}{hydflood}
#' 
"df.pnv"

#' @name sf.afe
#' @rdname sf.afe
#' 
#' @title Active floodplain along the River Elbe
#' 
#' @description This dataset contains a polygon of the active floodplain along
#'    the German interior parts of the River Elbe from the Czech border to the
#'    weir in Geesthacht in the coordinate reference system 
#'    \href{https://spatialreference.org/ref/epsg/etrs89-utm-zone-33n/}{ETRS 1989 UTM 33N}.
#' 
#'   Originally, this polygon was produced for the floodplain status report 
#'   (Auenzustandsbericht; Brunotte et al. (2009), Bundesamt für Naturschutz 
#'   (2009)) at a scale of 1:25,000. For hydflood it was updated with recent 
#'   flood protection measures and manually improved with recent digital 
#'   elevation models and aerial images at a scale of < 1:10,000.
#' 
#' @format A \code{sf} containing 1 polygon
#' 
#' @seealso \code{\link{sf.af}}, \code{\link{sf.afr}}
#' 
#' @references 
#'   \insertRef{brunotte_flussauen_2009}{hydflood}
#'   
#'   \insertRef{brunotte_flussauen_data_2009}{hydflood}
#'   
#'   \insertRef{bfn_auenzustandsbericht_2009}{hydflood}
#' 
"sf.afe"

#' @name sf.af
#' @rdname sf.af
#' @title Obtain projected versions of \code{sf.afe} and \code{sf.afr}
#' 
#' @description Obtain projected versions of \code{\link{sf.afe}} and
#'   \code{\link{sf.afr}}
#' 
#' @param name either 'Elbe' or 'Rhine'.
#' 
#' @return \code{sf} with the projected active floodplain 
#' 
#' @seealso \code{\link{sf.afe}}, \code{\link{sf.afr}}
#' 
#' @examples 
#'   library(hydflood)
#'   sf.af(name = "Elbe")
#' 
#' @export
sf.af <- function(name = NULL) {
    if (name == "Elbe") {
        x <- sf.afe
        sf::st_crs(x) <- sf::st_crs(25833)
        return(x)
    } else if (name == "Rhine") {
        x <- sf.afr
        sf::st_crs(x) <- sf::st_crs(25832)
        return(x)
    } else {
        stop("error")
    }
}

#' @name sf.afr
#' @rdname sf.afr
#' 
#' @title Active floodplain along the River Rhine
#' 
#' @description This dataset contains a polygon of the active floodplain along
#'   the German, freeflowing parts of the River Rhine from the weir Iffezheim to
#'   the Dutch border in the coordinate reference system 
#'   \href{https://spatialreference.org/ref/epsg/etrs89-utm-zone-32n/}{ETRS 1989 UTM 32N}.
#' 
#'   Originally, this polygon was produced for the floodplain status report 
#'   (Auenzustandsbericht; Brunotte et al. (2009), Bundesamt für Naturschutz 
#'   (2009)) at a scale of 1:25,000. For hydflood it was updated with recent 
#'   flood protection measures and manually improved with recent digital 
#'   elevation models and aerial images at a scale of < 1:10,000.
#' 
#' @format A \code{sf} containing 1 polygon 
#' 
#' @seealso \code{\link{sf.af}}, \code{\link{sf.afe}}
#' 
#' @references 
#'   \insertRef{brunotte_flussauen_2009}{hydflood}
#'   
#'   \insertRef{brunotte_flussauen_data_2009}{hydflood}
#'   
#'   \insertRef{bfn_auenzustandsbericht_2009}{hydflood}
#' 
"sf.afr"

#' @name sf.tiles_elbe
#' @rdname sf.tiles_elbe
#' 
#' @title Tiling along the active floodplain of the River Elbe
#' 
#' @description This dataset contains 49 rectangular polygons / tiles along the
#'    active floodplain along the German interior parts of the River Elbe from the
#'    Czech border to the weir in Geesthacht in the coordinate reference system
#'    \href{https://spatialreference.org/ref/epsg/etrs89-utm-zone-33n/}{ETRS 1989 UTM 33N}.
#'   
#'   The tiles represent the original tiling of the internally used digital
#'   elevation model (Weber 2020).
#' 
#' @format A \code{sf} containing 49 polygons with 18 attributes:
#' \describe{
#'   \item{id}{of the tile (type \code{integer}).} 
#'   \item{name}{of the tile (type \code{character}).}
#'   \item{river}{of the tile (type \code{character}) in this case 'ELBE'.}
#'   \item{name_km}{of the tile (type \code{character}).}
#'   \item{from_km}{river kilometer of the tiles upper limit (type \code{numeric}).}
#'   \item{to_km}{river kilometer of the tiles lower limit (type \code{numeric}).}
#'   \item{gs_upper}{name of the tiles upper gauging station (type \code{character}).}
#'   \item{gs_lower}{name of the tiles lower gauging station (type \code{character}).}
#'   \item{geometry}{\code{sfc_POLYGON} column storing the geometries.}
#'   \item{xmin}{of the tile extent (type \code{integer}). Minimum of UTM Easting (m).}
#'   \item{xmax}{of the tile extent (type \code{integer}). Maximum of UTM Easting (m).}
#'   \item{ymin}{of the tile extent (type \code{integer}). Minimum of UTM Northing (m).}
#'   \item{ymax}{of the tile extent (type \code{integer}). Maximum of UTM Northing (m).}
#'   \item{lon_min}{of the tile extent (type \code{numeric}). Minimum of Longitude (decimal °).}
#'   \item{lon_max}{of the tile extent (type \code{numeric}). Maximum of Longitude (decimal °).}
#'   \item{lat_min}{of the tile extent (type \code{numeric}). Minimum of Latitude (decimal °).}
#'   \item{lat_max}{of the tile extent (type \code{numeric}). Maximum of Latitude (decimal °).}
#'   \item{url}{of the tile (type \code{character}).}
#' }
#' 
#' @seealso \code{\link{sf.tiles}}, \code{\link{sf.tiles_rhine}}
#' 
#' @references 
#'   \insertRef{weber_dgms_2020}{hydflood}
#'   
#'   \insertRef{weber_dgm_elbe_2020}{hydflood}
#' 
"sf.tiles_elbe"

#' @name sf.tiles_rhine
#' @rdname sf.tiles_rhine
#' 
#' @title Tiling along the active floodplain of the River Rhine
#' 
#' @description This dataset contains 40 rectangular polygons / tiles along the
#'    active floodplain along the German, freeflowing parts of the River Rhine
#'    from the weir Iffezheim to the Dutch border near Kleve in the coordinate
#'    reference system
#'    \href{https://spatialreference.org/ref/epsg/etrs89-utm-zone-32n/}{ETRS 1989 UTM 32N}.
#'   
#'   The tiles represent the original tiling of the internally used digital
#'   elevation model (Weber 2020).
#' 
#' @format A \code{sf} containing 40 polygons with 18 attributes:
#' \describe{
#'   \item{id}{of the tile (type \code{integer}).} 
#'   \item{name}{of the tile (type \code{character}).}
#'   \item{river}{of the tile (type \code{character}) in this case RHINE'.}
#'   \item{name_km}{of the tile (type \code{character}).}
#'   \item{from_km}{river kilometer of the tiles upper limit (type \code{numeric}).}
#'   \item{to_km}{river kilometer of the tiles lower limit (type \code{numeric}).}
#'   \item{gs_upper}{name of the tiles upper gauging station (type \code{character}).}
#'   \item{gs_lower}{name of the tiles lower gauging station (type \code{character}).}
#'   \item{geometry}{\code{sfc_POLYGON} column storing the geometries.}
#'   \item{xmin}{of the tile extent (type \code{integer}). Minimum of UTM Easting (m).}
#'   \item{xmax}{of the tile extent (type \code{integer}). Maximum of UTM Easting (m).}
#'   \item{ymin}{of the tile extent (type \code{integer}). Minimum of UTM Northing (m).}
#'   \item{ymax}{of the tile extent (type \code{integer}). Maximum of UTM Northing (m).}
#'   \item{lon_min}{of the tile extent (type \code{numeric}). Minimum of Longitude (decimal °).}
#'   \item{lon_max}{of the tile extent (type \code{numeric}). Maximum of Longitude (decimal °).}
#'   \item{lat_min}{of the tile extent (type \code{numeric}). Minimum of Latitude (decimal °).}
#'   \item{lat_max}{of the tile extent (type \code{numeric}). Maximum of Latitude (decimal °).}
#'   \item{url}{of the tile (type \code{character}).}
#' }
#' 
#' @seealso \code{\link{sf.tiles_elbe}}, \code{\link{sf.tiles_rhine}}
#' 
#' @references
#'   \insertRef{weber_dgms_2020}{hydflood}
#'   
#'   \insertRef{weber_dgm_rhine_2020}{hydflood}
#' 
"sf.tiles_rhine"

#' @name sf.tiles
#' @rdname sf.tiles
#' @title Obtain projected versions of \code{sf.tiles_elbe} and
#'    \code{sf.tiles_rhine}
#' 
#' @description Obtain projected versions of \code{\link{sf.tiles_elbe}} and
#'    \code{\link{sf.tiles_rhine}}
#' 
#' @param name either 'Elbe' or 'Rhine'.
#' 
#' @return \code{sf} with projected tiles
#' 
#' @seealso \code{\link{sf.tiles_elbe}}, \code{\link{sf.tiles_rhine}}
#' 
#' @examples 
#'   library(hydflood)
#'   sf.tiles(name = "Elbe")
#' 
#' @export
sf.tiles <- function(name = NULL) {
    if (name == "Elbe") {
        x <- sf.tiles_elbe
        sf::st_crs(x) <- sf::st_crs(25833)
        return(x)
    } else if (name == "Rhine") {
        x <- sf.tiles_rhine
        sf::st_crs(x) <- sf::st_crs(25832)
        return(x)
    } else {
        stop("error")
    }
}
