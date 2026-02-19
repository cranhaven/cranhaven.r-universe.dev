#' Adds a tile layer from Amap to a leaflet map.
#'
#' This function adds a tile layer from Amap to a leaflet map object.
#'
#' @param map A leaflet map object to which the tile layer will be added.
#' @param attribution A string containing the attribution text to be displayed
#'   on the map. It defaults to "&copy; <a href=\"http://amap.com\">amap.com</a>".
#' @param ... Additional arguments to be passed to the `leaflet::addTiles`
#'   function.
#'
#' @return The leaflet map object with the added tile layer.
#' @export
#'
#' @examples
#' library(leaflet)
#' leaflet() %>%
#'   addTilesAmap() %>%
#'   setView(
#'     lng = 120.33739,
#'     lat = 31.13533,
#'     zoom = 3
#'   )
#'
addTilesAmap <- function(map, attribution = "&copy; <a href=\"http://amap.com\">amap.com</a >",
                         ...) {
  leaflet::addTiles(map, "http://webrd02.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}",
    leaflet::tileOptions(tileSize = 256, minZoom = 3, maxZoom = 17),
    attribution = attribution, ...
  )
}
