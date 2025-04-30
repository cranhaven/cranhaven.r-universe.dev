#' Download metadata (registry) on weather monitoring stations from ARPA Lombardia website
#'
#' @description 'get_ARPA_Lombardia_W_registry' returns the registry (list) of all the weather sensors
#' and stations belonging to the ARPA Lombardia network. The information reported are: ID of each sensor and
#' station, geo-location (coordinates in degrees), altitude (mt), starting date and ending date.
#' The column 'NameStation' identifies the name of each station, while 'IDStation' is an ID code (assigned
#' from ARPA) uniquely identifying each station.
#' For more information about the municipal data visit the section 'Meteo' at the webpages:
#' https://www.dati.lombardia.it/stories/s/auv9-c2sj and
#' https://www.dati.lombardia.it/Ambiente/Stazioni-Meteorologiche/nf78-nj6b
#'
#' @return A data frame of class 'data.frame' and 'ARPALdf'. The object is fully compatible with Tidyverse.
#'
#' @examples
#' get_ARPA_Lombardia_W_registry()
#'
#'
#' @export

get_ARPA_Lombardia_W_registry <-
  function() {
    Metadata <- W_metadata_reshape()
    structure(list(Metadata = Metadata))
    attr(Metadata, "class") <- c("ARPALdf","ARPALdf_W","tbl_df","tbl","data.frame")
    return(Metadata)
  }
