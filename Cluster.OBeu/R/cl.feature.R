#' @title
#' Clustering features
#' @description 
#' Select clustering characteristic to form the clustering data 
#' 
#' @usage cl.features(data, features = NULL, amounts = NULL, aggregate = "sum", tojson = FALSE )
#' @param data The input data
#' @param features The clustering features
#' @param amounts The amount measures of the dataset
#' @param aggregate The function to aggregate
#' @param tojson If TRUE the results are returned in json format, default returns a list
#' 
#' @details This function adapts the dataset according to the selected dimension of the dataset 
#' and the aggregation function. 
#'
#' @return This function returns the dataset for cluster analysis adapted to the desired features. 
#' @author Kleanthis Koupidis
#' @seealso \code{\link{cl.analysis}}
#' @examples
#' cl.features(city_data, features = 'Administrative_Unit')
#' 
#' # works also for other datasets
#' cl.features(iris, features = 'Species')
#' @rdname cl.features
#' @export
#' 

cl.features <- function(data, features = NULL, amounts = NULL, aggregate = "sum", tojson = FALSE ) {
  
  # Convert to data frame
  data <- as.data.frame(data)
  
  if (is.null(features)) {
    features <- names(which(sapply(data, is.factor) | sapply(data, is.character))) 
  }
  
  if (is.null(amounts)) { 
    amounts <- names(which(sapply(data, is.double) | sapply(data, is.numeric)))
  }
  
  # Melt data
  molten_data <- reshape2::melt(data, id.vars = features, measure.vars = amounts)
  
  if (length(features) > 1) {
    features <- stringr::str_c(features, collapse = "+")
  }
  
  # Form Dataset
  cluster.data <- reshape2::dcast(molten_data, 
                                  noquote(paste(features, "~" , "variable")), fun.aggregate = sum)
  
  return(cluster.data)
}
