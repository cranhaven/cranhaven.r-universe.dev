#' @title encoding_data
#' @description this function returns a data frame where character columns are encoded as UTF-8. it is a helper function in get_data_from_bursa
#' @param data A data frame obtained from get_data_from_bursa
#'
encoding_data <-function(data){
  for (col in colnames(data)){
    if(is.character(data[[col]])==TRUE){
      Encoding(data[[col]]) <- "UTF-8"
    }
  }
  return(data)
}
