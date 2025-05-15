#' MM to iGAP
#'
#' @name MM_to_iGAP
#' @aliases MM_to_iGAP
#' @description To convert MM format to iGAP format.
#' @usage MM_to_iGAP(data)
#' @param data The dataframe with the MM format.
#' @returns Return a dataframe with the iGAP format.
#' @importFrom dplyr select
#' @importFrom tidyr unite
#' @importFrom magrittr %>%
#' @examples
#' data(Face.iGAP)
#' Face <- iGAP_to_MM(Face.iGAP, 1:6)
#' MM_to_iGAP(Face)
#' @export

MM_to_iGAP <- function(data){
  data1 <- clean_colnames(data)
  cols <- unique(names(data1))
  df <- cbind(do.call(cbind, lapply(cols,
                                    function(x){tidyr::unite(data, x, grep(x, names(data), value = TRUE),
                                                            sep = ',', remove = TRUE)} %>% dplyr::select(x))
  ))
  names(df) <- cols
  return(df)
}
