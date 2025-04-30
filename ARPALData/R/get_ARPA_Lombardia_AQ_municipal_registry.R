#' Download metadata (registry) on air quality monitoring stations at municipal level from ARPA Lombardia website
#'
#' @description 'get_ARPA_Lombardia_AQ_municipal_registry' returns the registry (list) of all the air quality sensors
#' owned by ARPA Lombardia for each municipality of Lombardy. The information reported are: ID of each sensor and
#' station, starting date and ending date. The column 'NameStation' identifies the name of each municipality.
#' The column 'IDStation' is an ID code (assigned from ARPA) uniquely identifying each municipality.
#' For more information about the municipal data visit the section 'Stime comunali sull'aria' at the webpage:
#' https://www.dati.lombardia.it/stories/s/auv9-c2sj
#'
#' @return A data frame of class 'data.frame' and 'ARPALdf'. The object is fully compatible with Tidyverse.
#'
#' @examples get_ARPA_Lombardia_AQ_municipal_registry()
#'
#' @export

get_ARPA_Lombardia_AQ_municipal_registry <-
  function() {
    Metadata <- AQ_municipal_metadata_reshape()
    structure(list(Metadata = Metadata))
    attr(Metadata, "class") <- c("ARPALdf","ARPALdf_AQ_mun","tbl_df","tbl","data.frame")
    return(Metadata)
  }
