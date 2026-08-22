#' Repair sf polygons
#' @return An sf object
#' @param x An sf object with unprojected coordinates
#' @param crs The crs for transforming the sf
#' @import mapview
#' @import sf
#' @import stringr
#' @import dplyr
#' @export st_transform_repair
#' @keywords sf
#' @keywords spatial
#' @keywords map
#' @keywords meridian

st_transform_repair <- function(x,crs){
  # x <- x |> sf::st_make_valid() |> sf::st_transform(4326)
  if(!is.character(crs)){ ## check if st_crs() function is used
    crs = crs$input}else{
    }
  if(stringr::str_detect(crs,"ortho")){ ## check for ortho
    temp.sf <- sf::st_cast(x, 'MULTILINESTRING') %>%
    sf::st_cast('LINESTRING', do_split=TRUE) %>%
    sf::st_transform(crs = crs)
    temp.sf$npts = mapview::npts(temp.sf$geometry, by_feature = TRUE)
    temp.sf <- temp.sf %>% dplyr::filter(npts > 3) %>%
      sf::st_cast('POLYGON')
  }else{
  anti_meridian <- stringr::str_extract(crs, "\\lon_0=-{0,1}\\d{1,3}") ## step by step regex
  anti_meridian <- stringr::str_replace(anti_meridian, "lon_0=","")
  anti_meridian <- as.numeric(stringr::str_replace(anti_meridian, "\\s",""))
  anti_meridian <- ifelse(is.na(anti_meridian),0,anti_meridian) ## zero is no new prime meridian
  temp.sf <- x %>% st_break_antimeridian(lon_0 = anti_meridian) %>%  sf::st_transform(crs = crs)}
  ## transform after setting new meridian
  return(temp.sf)
}
