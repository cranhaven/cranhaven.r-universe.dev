#' SODAS to iGAP
#'
#' @name SODAS_to_iGAP
#' @aliases SODAS_to_iGAP
#' @description To convert SODAS format interval dataframe to the iGAP format.
#' @usage SODAS_to_iGAP(XMLPath)
#' @param XMLPath Disk path where the SODAS *.XML file is.
#' @returns Return a dataframe with the iGAP format.
#' @importFrom RSDA SODAS.to.RSDA
#' @examples
#' ## Not run:
#  # We can read the file directly from the SODAS XML file as follows:
#  # abalone <- SODAS_to_MM('C:/Users/user/AppData/abalone.xml)
#' data(Abalone)
#' @export

SODAS_to_iGAP <- function(XMLPath){
  data <- RSDA::SODAS.to.RSDA(XMLPath)
  df <- RSDA_to_iGAP(data)
  return(df)
}
