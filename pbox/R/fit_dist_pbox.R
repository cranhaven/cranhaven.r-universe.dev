#' Fit Marginal Distributions
#'
#' Fits the best marginal distribution for each variable in a data frame using the `gamlss::fitDist`
#' function from the GAMLSS package. This function is designed to evaluate multiple distributions,
#' returning a summary of fit for each, along with the Akaike Information Criterion (AIC) for comparison.
#'
#' @name fit_dist_pbox
#' @export
#' @param data A data frame or data table that contains the variables for which distributions
#'        will be fitted. The data will be coerced to a data.table internally if not already one.
#' @param ... Additional arguments passed to the `fitDist` function.
#' @return A list containing two elements:
#'         \item{allDitrs}{List of the fitted distributions for each variable.}
#'         \item{distTable}{A data table displaying the AIC for each tested distribution.}
#' @examples
#'   data(SEAex)
#'   distFits <- fit_dist_pbox(data=SEAex)
#'   print(distFits$allDitrs)
#'   print(distFits$distTable)
#' @importFrom gamlss fitDist
#' @importFrom utils capture.output
#' @importFrom purrr map_depth imap is_empty
#' @importFrom data.table data.table setnames as.data.table
setGeneric("fit_dist_pbox",
           def = function(data, ...) {
             standardGeneric("fit_dist_pbox")
           })

#' @rdname fit_dist_pbox
#' @description
#' Implements the generic function `fit_dist_pbox` for data frames and data tables.
#' This method utilizes statistical techniques to fit distributions to each column in the `data`
#' argument, evaluating fit using criteria like AIC to determine the best fitting model.
#' @param data A data frame or data table.
#' @param ... Additional parameters to pass to the fitting function.


setMethod("fit_dist_pbox",
          definition=function(data,...){

            if(is_empty(data)){
              stop("Input is empty!")
            }

# temporary solution with capture.output
##Errors from fitdist should be stored somewhere?
    er_cap<-capture.output(allDitrs<-lapply(data,function(x)  suppressWarnings(gamlss::fitDist(x, ...))),type = "message")

    fitsList<-purrr::map_depth(allDitrs,1,"fits")
    dt_list <- imap(fitsList, function(x,y) {
      df<-as.data.table(x,keep.rownames="DIST",value.name=names(x))
      setnames(df,"x",y)
      df
    })
    distTable<- Reduce(function(...) merge(..., all = TRUE), dt_list)
    return(list(allDitrs=allDitrs,distTable=distTable))
  # er_cap<-capture.output(allDitrs<-lapply(data,function(x)  suppressWarnings(gamlss::fitDist(x, ...))),type = "message")
  # distTable<-data.table::data.table(do.call(cbind,purrr::map_depth(allDitrs,1,"fits")), keep.rownames="DIST")
  # return(list(allDitrs=allDitrs,distTable=distTable))
})
