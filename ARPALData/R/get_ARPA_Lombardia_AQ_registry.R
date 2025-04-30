#' Download metadata (registry) on air quality monitoring stations from ARPA Lombardia website
#'
#' @description 'get_ARPA_Lombardia_AQ_registry' returns the registry (list) of all the air quality sensors
#' and stations belonging to the ARPA Lombardia network. The information reported are: ID of each sensor and
#' station, geo-location (coordinates in degrees), altitude (mt), starting date and ending date.
#' The column 'NameStation' identifies the name of each station, while 'IDStation' is an ID code (assigned
#' from ARPA) uniquely identifying each station.
#' For more information about the municipal data visit the section 'Monitoraggio aria' at the webpage:
#' https://www.dati.lombardia.it/stories/s/auv9-c2sj
#'
#' @return A data frame of class 'data.frame' and 'ARPALdf'. The object is fully compatible with Tidyverse.
#'
#' @examples get_ARPA_Lombardia_AQ_registry()
#'
#' @export

get_ARPA_Lombardia_AQ_registry <-
  function() {
    Metadata <- AQ_metadata_reshape()
    structure(list(Metadata = Metadata))
    attr(Metadata, "class") <- c("ARPALdf","ARPALdf_AQ","tbl_df","tbl","data.frame")
    return(Metadata)
  }
