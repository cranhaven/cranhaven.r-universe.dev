#' @title 
#' Read and Calculate the Basic Information for Cluster Analysis Tasks from Open Spending API
#'  
#' @description
#' Extract and analyze the input data provided from Open Spending API, using the \code{\link{cl.analysis}} function.
#' 
#' @usage open_spending.cl(json_data, dimensions=NULL, amounts=NULL, measured.dimensions=NULL,
#' cl.aggregate="sum", cl.method=NULL, cl.num=NULL, cl.dist="euclidean")
#' 
#' @param json_data The json string, URL or file from Open Spending API
#' @param dimensions The dimensions/feature of the input data
#' @param amounts The measures of the input data
#' @param measured.dimensions The dimensions to which correspond amount/numeric variables
#' @param cl.aggregate Aggregate function of the input data
#' @param cl.method The clustering algorithm
#' @param cl.num The number of clusters
#' @param cl.dist The distance metric
#' 
#' @details 
#' This function is used to read data in json format from Open Spending API, in order to implement 
#' cluster analysis through \code{\link{cl.analysis}} function.
#' 
#' @return A json string with the resulted parameters of the \code{\link{cl.analysis}} function.
#'
#' @author Kleanthis Koupidis
#' 
#' @seealso \code{\link{cl.analysis}}
#' 
#' @rdname open_spending.cl
#'
#' @export
#' 

open_spending.cl <- function(json_data, dimensions = NULL, amounts = NULL, measured.dimensions = NULL,
                             cl.aggregate = "sum", cl.method = NULL, cl.num = NULL, cl.dist = "euclidean") {
  
  linkexist <- RCurl::url.exists(json_data)
  if (isTRUE(!linkexist)) stop("Not valid json data input")
  
  dt <- jsonlite::fromJSON(json_data)
  components <- c("data", "cells")
  select.comp <- match.arg(components, names(dt), several.ok = TRUE)
  dt <- as.data.frame (dt[select.comp])
  amounts2 <- unlist(strsplit(amounts,"\\|"))
  dimensions <- unlist(strsplit(dimensions,"\\|"))
  
  if (select.comp == "data") {
    
    names(dt) <- gsub("data.", "", names(dt))
    variables <- c(dimensions, amounts2)
    dt2 <- dt[variables]
    dt2[dimensions] <- sapply(dt2[dimensions],as.character)
    
  } else {
    names(dt) <- gsub("cells.", "", names(dt))
    dt <- dt[, c(dimensions,measured.dimensions,amounts)]
    melt <- reshape::melt.data.frame(dt, id.vars = c(dimensions,measured.dimensions))
    melt$value <- as.numeric(melt$value)
    
    if (length(dimensions > 1)) {
      dimensions2 <- paste(dimensions, collapse = "+" )
    } else {
      dimensions2 <- dimensions
    }
    
    if (length(measured.dimensions > 1)) {
      measured.dimensions2 <- paste(measured.dimensions,collapse = "+") 
    } else {
      measured.dimensions2 <- measured.dimensions
    }
    
    formula <- paste(dimensions2, measured.dimensions2, sep = "~")
    dt2 <- reshape2::dcast(data = melt, formula = formula, fun.aggregate = sum, drop = FALSE)
    amounts2 <- unique(dt[,paste0(measured.dimensions)])
  }
  
  dt2 <- stats::na.omit(dt2)
  cl.result <- cl.analysis(
    cl.data = dt2, cl_feature = dimensions, amount = amounts2, cl.aggregate = cl.aggregate,
    cl.meth = cl.method, clust.numb = cl.num, dist = cl.dist)
  
  cl.results <- jsonlite::toJSON(cl.result)
  
  return (cl.results)
}
