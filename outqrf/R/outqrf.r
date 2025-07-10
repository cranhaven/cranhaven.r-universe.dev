#' @title get numberic value from string
#'
#' @description
#' This function extracts the numeric value from a string.
#'
#' @param name a string
#'
#' @return a numeric value
#'
#' @examples
#' get_quantily_value("quantiles = 0.001")
#' @export
get_quantily_value <- function(name){
    str<- gsub("[^0-9.]", "", name)
    value <- as.numeric(str)
    return(value)
}

#' @title find the closest index
#'
#' @description
#' This function finds the closest index to a given value in a vector.
#'
#' @param x a vector
#' @param y a value
#'
#' @return the index of the closest value in the vector
#'
#' @examples
#' find_index(c(1, 2, 3, 4, 5), 3.5)
#' @export
find_index <- function(x, y) {
    index <- which(x == y)
    if (length(index) >= 1) {
        index_name <- names(x)[index]
        value<-get_quantily_value(index_name)
        return(value)
    }else {
        closest_index <- which(abs(x - y) == min(abs(x-y)))
        closest_index_name <- names(x)[closest_index]
        value <- get_quantily_value(closest_index_name)
        return(value)
    }
}

#' @title find the right rank
#'
#' @description
#' This function finds the right rank of a response value in a quantile random forest.
#'
#' @param response a vector of response values
#' @param outMatrix a matrix of out values
#' @param median_outMatrix a vector of median out values
#' @param rmse_ a vector of rmse values
#'
#' @return a vector of ranks
#'
get_right_rank <- function(response,outMatrix,median_outMatrix,rmse_){
    rank_value <-c()
    for (i in seq_along(response)){
        rank_<- find_index(outMatrix[i,],response[i])
        if (length(rank_)>1){
            #We use a method similar to the outoutForest package to determine the exact rank,
            #but instead of the predicted mean of a random forest, we subtract the median prediction of qrf.
            diff <- response[i] -median_outMatrix[i]
            if (abs(diff)>3*rmse_ && diff<0 ){
                min_value <- min(rank_)
                rank_value<-c(rank_value,min_value)
            } else if (abs(diff)>3*rmse_ && diff>0) {
                max_value <- max(rank_)
                rank_value<-c(rank_value,max_value)
            }else {
                mean_value <- mean(rank_)
                rank_value<-c(rank_value,mean_value)
            }
        }else {
             rank_value<-c(rank_value,rank_)
            }

    }
    return(rank_value)
}

#' @title find outliers
#'
#' @description
#' This function finds outliers in a dataset using quantile random forests.
#'
#' @param data a data frame
#' @param quantiles_type specify the type of quantile generation.Default is 1000.
#' @param threshold a threshold for outlier detection
#' @param verbose a boolean value indicating whether to print verbose output
#' @param impute a boolean value indicating whether to impute missing values
#' @param weight a boolean value indicating whether to use weight. if TRUE, The actual threshold will be threshold*r2.
#' @param ... additional arguments passed to the ranger function
#' @return
#' An object of class "outqrf" and a list with the following elements.
#'   - `Data`: Original data set in unchanged row order
#'   - `outliers`: Compact representation of outliers. Each row corresponds to an outlier and contains the following columns:
#'     - `row`: Row number of the outlier
#'     - `col`: Variable name of the outlier
#'     - `observed`: value of the outlier
#'     - `predicted`: predicted value of the outlier
#'     - `rank`: Rank of the outlier
#'   - `outMatrix`: Predicted value at different quantiles for each observation
#'   - `r.squared`: R-squared value of the quantile random forest model
#'   - `outMatrix`: Predicted value at different quantiles for each observation
#'   - `r.squared`: R-squared value of the quantile random forest model
#'   - `oob.error`: Out-of-bag error of the quantile random forest model
#'   - `rmse`: RMSE of the quantile random forest model
#'   - `threshold`: Threshold for outlier detection
#' @examples
#' iris_with_outliers <- generateOutliers(iris, p=0.05)
#' qrf = outqrf(iris_with_outliers)
#' qrf$outliers
#' evaluateOutliers(iris,iris_with_outliers,qrf$outliers)
#' @export
outqrf <-function(data,
                  quantiles_type=1000,
                  threshold =0.025,
                  impute = TRUE,
                  verbose = 1,
                  weight = FALSE,
                  ...){
    # Initial check
    if (!is.data.frame(data)) {
        data <- as.data.frame(data)
    }
    if (!is.numeric(threshold)) {
        stop("Threshold must be a numeric value.")
    }
    if (threshold < 0 || threshold > 1) {
        stop("Threshold should be between 0 and 1")
    }
    if (!(quantiles_type %in% c(1000, 400, 40))) {
        stop("quantiles_type should be one of 1000, 400, 40")
    }
    # impute missing values with missRanger
    if (anyNA(data)) {
        if(impute){
        data <- missRanger::missRanger(data, pmm.k = 3, num.trees = 500,data_only=TRUE,verbose=0)
        }else{
            stop("Missing values detected. Please impute them first!")
        }
    }
    # Definition of variables
    threshold_low<-threshold
    threshold_high<-1-threshold
    numeric_features <- names(data)[sapply(data,is.numeric)]
    rmse <-c()
    oob.error <-c()
    r.squared <-c()
    outliers <- data.frame()
    outMatrixs <- list()
    if(quantiles_type == 1000){
        quantiles <- seq(0.001, 0.999,0.001)
    }else if(quantiles_type == 400){
        quantiles <- c(seq(0.0025,0.9975,0.0025))
    }else{
        quantiles <- c(seq(0.025,0.9975,0.025))
    }

    if (verbose) {
    cat("\nOutlier identification by quantiles random forests\n")
    cat("\n  Variables to check:\t\t")
    cat(numeric_features, sep = ", ")
    cat("\n  Variables used to check:\t")
    cat(names(data), sep = ", ")
    cat("\n\n  Checking: ")
    }

    # Loop over numeric features
    for (v in numeric_features){
        if (verbose) {
            cat(v, " ")
        }
        covariables <- setdiff(names(data), v)
        qrf <- ranger::ranger(
            formula = stats::reformulate(covariables, response = v),
            data = data,
            quantreg = TRUE,
            ...)
        pred <- predict(qrf, data[covariables], type = "quantiles",quantiles=quantiles)
        oob.error <- c(oob.error,qrf$prediction.error)
        r.squared <- c(qrf$r.squared,r.squared)
        outMatrix <- pred$predictions
        outMatrixs[[v]]<-outMatrix
        median_outMatrix <- outMatrix[,(length(quantiles)+1)/2]

        response<- data[,v]
        diffs = response - median_outMatrix
        rmse_ <- sqrt(sum(diffs*diffs)/(length(diffs)-1))
        rmse <- c(rmse,rmse_)
        rank_value <- get_right_rank(response,outMatrix,median_outMatrix,rmse_)
        outlier <- data.frame(row = as.numeric(row.names(data)),col = v,observed = response, predicted = median_outMatrix,rank = rank_value)
        if (weight){
            outlier<- outlier|>dplyr::filter(rank<=threshold_low*qrf$r.squared| rank>=1-threshold_low*qrf$r.squared)}
        else{
            outlier<- outlier|>dplyr::filter(rank<=threshold_low| rank>=threshold_high)
        }
        outliers <- rbind(outliers,outlier)
    }
    # names of the variables
    names(rmse) <- numeric_features
    names(oob.error) <- numeric_features
    names(r.squared) <- numeric_features
    # return the results
    return_result<- list(
    Data = data,
    outliers = outliers,
    n_outliers = table(outliers$col),
    threshold = threshold,
    rmse = rmse,
    oob.error = oob.error,
    r.squared = r.squared,
    outMatrixs =outMatrixs,
    quantiles_type = quantiles_type
    )
    class(return_result) <- c("outqrf")
    return_result

}
