#' Create outline of sf projection
#' @return An sf object
#' @param crs The crs for transforming the sf
#' @import sf
#' @import stringr
#' @export st_transform_outline
#' @keywords sf
#' @keywords spatial
#' @keywords map


st_transform_outline <- function(crs){
  if(is.character(crs)){
   crs=crs
  }else{
   crs=eval(st_crs(crs))$input 
  }
  anti_meridian <- stringr::str_extract(crs, "\\lon_0=-{0,1}\\d{1,3}") ## step by step regex
  anti_meridian <- stringr::str_replace(anti_meridian, "lon_0=","")
  anti_meridian <- as.numeric(stringr::str_replace(anti_meridian, "\\s",""))
  anti_meridian <- ifelse(is.na(anti_meridian),0,anti_meridian) ## zero is no new prime meridian

  new_equator <- stringr::str_extract(crs, "\\lat_0=-{0,1}\\d{1,3}") ## step by step regex
  new_equator <- stringr::str_replace(new_equator, "lat_0=","")
  new_equator <- as.numeric(stringr::str_replace(new_equator, "\\s",""))
  new_equator <- ifelse(is.na(new_equator),0,new_equator) ## zero is no new prime meridian


  if(str_detect(crs,"ortho")){
    offset_a <- anti_meridian+90
    offset_b <- anti_meridian-90
    jitter <- 0.01


    lat <- c(seq(from=new_equator+90+jitter,to=new_equator-90-jitter,by=-0.25))
    lon <- c(rep(offset_a-jitter,length(lat)),rep(offset_b+jitter,length(lat)))
    oceans.sf <- cbind(lon, c(lat, rev(lat))) |> sf::st_linestring() |> sf::st_cast("POLYGON") |>
      sf::st_sfc(crs=4326) |> sf::st_transform(crs=crs)
  }else{
    offset <- anti_meridian+180
    jitter <- 0.01
    lat <- c(seq(from=-90,to=90,by=0.25))
    lon <- c(rep(offset-jitter,length(lat)),rep(offset+jitter,length(lat)))
    oceans.sf <- cbind(lon, c(lat, rev(lat))) |> sf::st_linestring() |> sf::st_cast("POLYGON") |>
      sf::st_sfc(crs=4326) |> sf::st_transform(crs=crs)}
  oceans.sf <- sf::st_transform(oceans.sf, crs=crs)
  return(oceans.sf)
}

