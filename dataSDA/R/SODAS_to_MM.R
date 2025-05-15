#' SODAS to MM
#'
#' @name SODAS_to_MM
#' @aliases SODAS_to_MM
#' @description To convert SODAS format interval dataframe to the MM format.
#' @usage SODAS_to_MM(XMLPath)
#' @param XMLPath Disk path where the SODAS *.XML file is.
#' @returns Return a dataframe with the MM format.
#' @importFrom RSDA SODAS.to.RSDA
#' @examples
#' ## Not run:
#  # We can read the file directly from the SODAS XML file as follows:
#  # abalone <- SODAS_to_MM('C:/Users/user/AppData/abalone.xml)
#' data(Abalone)
#' @export

SODAS_to_MM <- function(XMLPath){
  data <- RSDA::SODAS.to.RSDA(XMLPath)
  df <- RSDA_to_MM(data, RSDA = T)
  return(df)
}
