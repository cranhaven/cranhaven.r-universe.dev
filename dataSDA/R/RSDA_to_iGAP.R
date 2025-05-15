#' RSDA to iGAP
#'
#' @name RSDA_to_iGAP
#' @aliases RSDA_to_iGAP
#' @description To convert RSDA format interval dataframe to iGAP format.
#' @usage RSDA_to_iGAP(data)
#' @param data The RSDA format with interval dataframe.
#' @returns Return a dataframe with the iGAP format.
#' @examples
#' data(mushroom.int)
#' RSDA_to_iGAP(mushroom.int)
#' @export

RSDA_to_iGAP <- function(data){
  df <- RSDA_to_MM(data, RSDA = T)
  df.iGAP <- MM_to_iGAP(df)
  return(df.iGAP)
}
