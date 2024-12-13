#' Function that creates an sf-object from IFS data
#'
#' @eval MRGparam("ifs")
#' @eval MRGparam("crsOut")
#' @eval MRGparam("locAdj")
#'
#' @details The geo-location in the FSS file has a particular format. For 2020, it includes country, coordinate reference system (CRS), resolution
#' (precision of coordinates) and coordinates
#' in one attribute ("GEO_LCT"). For past years, the FSS data structure differs and it includes three separate columns, like latitudes, longitudes and coordinate reference system.
#' This function splits the attribute in its individual parts, and creates an
#' sf-object with the correct coordinates and CRS.
#'
#' @returns An \code{\link[sf]{sf}}-object with the locations of the survey or census data
#'
#' @examples
#' data(ifs_dk)
#' ifg = fssgeo(ifs_dk)
#'
#'
#' @export
fssgeo = function(ifs, crsOut = 3035, locAdj = FALSE) {
  if (length(unique(ifs$YEAR)) > 1) stop("The data set includes observations from different years ")
  if (unique(ifs$YEAR) >= 2020){
    geo = ifs$GEO_LCT
    gspl = strsplit(geo, "_")
    countries = unlist(lapply(gspl, "[[", 1))
    country = unique(countries)
    gsplr = unlist(lapply(gspl, "[[", 2))
    gsplr = strsplit(gsplr, "RES")
    crsi = unlist(lapply(gsplr, "[[", 1))
    crsi = as.numeric(gsub("CRS", "", crsi))
    gsplr2 = unlist(lapply(gsplr, "[[", 2))
    gspl2 = strsplit(gsplr2, "MN")
    gsplr3 = unlist(lapply(gspl2, "[[", 2))
    clist = as.numeric(unlist(strsplit(gsplr3, "E")))
    coor = matrix(clist, ncol = 2, byrow = TRUE)
    ifs$xx = coor[,2]
    ifs$yy = coor[,1]
    ifs$crsi = crsi
    ifs$country = countries
  } else {
    ifs$yy=ifs$A_1_1_NUMBER
    ifs$xx=ifs$A_1_2_NUMBER
    ifs$crsi = ifs$A_1_3_CRD_REF
    ifs$crsi = ifelse(ifs$crsi %in% c("ETRS89","4"), 4258, 3035)
  }
  sf_crs<-NULL
  crss = unique(ifs$crsi)
  for (icrs in crss){
    #' @importFrom dplyr filter bind_rows
    df <- ifs %>% filter(crsi == icrs)
    stf <- st_as_sf(df, coords = c("xx", "yy"), crs = st_crs(icrs)) %>% 
      st_transform(crs = crsOut)
    sf_crs <-  bind_rows(sf_crs, stf)
  }
  #' @importFrom sf st_crs
  if (!isFALSE(locAdj)) sf_crs = locAdjFun(sf_crs, locAdj)
  sf_crs
}
