#' Expectation-Maximization Imputation with Evaluation Metrics
#'
#' This function performs Expectation-Maximization (EM) imputation on a dataset with missing values.
#' It uses the `imputeEM` function from the `mvdalab` package to estimate the missing values.
#' The function also calculates various evaluation metrics including RMSE, MMAE, and RRE.
#' Additionally, it performs k-means and hierarchical clustering to assess the quality of the imputation.
#'
#' @param data0 The original dataset containing the response variable and features.
#' @param data.sample The dataset used for sampling, which may contain missing values.
#' @param data.copy A copy of the original dataset, used for comparison or validation.
#' @param mr Indices of the rows with missing values that need to be predicted.
#' @param km The number of clusters for k-means clustering.
#' @return A list containing:
#' \item{Xnew}{The imputed dataset.}
#' \item{RMSE}{The Root Mean Squared Error.}
#' \item{MMAE}{The Mean Absolute Error.}
#' \item{RRE}{The Relative Eelative Error.}
#' \item{CPP1}{The K-means clustering Consistency Proportion Index.}
#' \item{CPP2}{The Hierarchical Clustering Complete Linkage Consistency Proportion Index.}
#' \item{CPP3}{The Hierarchical Clustering Single Linkage Consistency Proportion Index.}
#' \item{CPP4}{The Hierarchical Clustering Average Linkage Consistency Proportion Index.}
#' \item{CPP5}{The Hierarchical Clustering Centroid linkage Consistency Proportion Index.}
#' \item{CPP6}{The Hierarchical Clustering Median Linkage Consistency Proportion Index.}
#' \item{CPP7}{The Hierarchical Clustering Ward's Method Consistency Proportion Index.}
#' \item{timeEM}{The EM algorithm execution time.}
#' @export
#'
#' @examples
#' # Create a sample matrix with random values and introduce missing values
#' set.seed(123)
#' n <- 100
#' p <- 5
#' data.sample <- matrix(rnorm(n * p), nrow = n)
#' data.sample[sample(1:(n*p), 20)] <- NA
#' data.copy <- data.sample
#' data0 <- data.frame(data.sample, response = rnorm(n))
#' mr <- sample(1:n, 10)  # Sample rows for evaluation
#' km <- 3  # Number of clusters
#' # Perform EM imputation
#' result <- EM(data0, data.sample, data.copy, mr, km)
#' # Print the results
#' print(result$RMSE)
#' print(result$MMAE)
#' print(result$RRE)
#' print(result$CPP1)
#' print(result$Xnew)
#' @importFrom stats kmeans cutree dist hclust
#' @importFrom mvdalab imputeEM
EM <- function(data0, data.sample, data.copy, mr, km) {
    data.sample <- data.frame(data.sample)
    n <- nrow(data.sample); p <- ncol(data.sample)

    # Record the execution time
    timeEM <- system.time({

    dataEM <- imputeEM(data.sample)  # library(mvdalab)
    XEM <- dataEM$Imputed.DataFrames[[2]]
    XEM <- as.matrix(XEM)
    actuals <- data.copy[mr]
    predicteds <- XEM[mr]

    # Calculate RMSE
    RMSE <- sqrt(base::mean((actuals - predicteds)^2))

    # Calculate MMAE
    MMAE <- base::mean(abs(predicteds - actuals))

    # Calculate RRE
    RRE <- RMSE / (max(actuals) - min(actuals))

    # K-means clustering
    s <- scale(XEM)
    km <- kmeans(s, km)
    I1 <- matrix(0, nrow = n, ncol = 3)
    for(g in 1:n) {
        I1[g, 1] <- g
    }
    I1[, 2] <- km$cluster
    I1[, 3] <- data0[, p + 1]
    CPP1 <- IndexCPP(I1)

    # Hierarchical clustering
    HCdata <- XEM
    distance <- dist(HCdata)

    # Complete linkage
    HCdata.hc <- hclust(distance)
    HCdata.id <- cutree(HCdata.hc, 3)
    I2 <- matrix(0, nrow = n, ncol = 3)
    for(g in 1:n) {
        I2[g, 1] <- g
    }
    I2[, 2] <- HCdata.id
    I2[, 3] <- data0[, p + 1]
    CPP2 <- IndexCPP(I2)

    # Single linkage
    HCdata.single <- hclust(distance, method = "single")
    HCdatasingle.id <- cutree(HCdata.single, 3)
    I3 <- matrix(0, nrow = n, ncol = 3)
    for(g in 1:n) {
        I3[g, 1] <- g
    }
    I3[, 2] <- HCdatasingle.id
    I3[, 3] <- data0[, p + 1]
    CPP3 <- IndexCPP(I3)

    # Average linkage
    HCdata.average <- hclust(distance, method = "average")
    HCdataaverage.id <- cutree(HCdata.average, 3)
    I4 <- matrix(0, nrow = n, ncol = 3)
    for(g in 1:n) {
        I4[g, 1] <- g
    }
    I4[, 2] <- HCdataaverage.id
    I4[, 3] <- data0[, p + 1]
    CPP4 <- IndexCPP(I4)

    # Centroid linkage
    HCdata.centroid <- hclust(distance, method = "centroid")
    HCdatacentroid.id <- cutree(HCdata.centroid, 3)
    I5 <- matrix(0, nrow = n, ncol = 3)
    for(g in 1:n) {
        I5[g, 1] <- g
    }
    I5[, 2] <- HCdatacentroid.id
    I5[, 3] <- data0[, p + 1]
    CPP5 <- IndexCPP(I5)

    # Median linkage
    HCdata.median <- hclust(distance, method = "median")
    HCdatamedian.id <- cutree(HCdata.median, 3)
    I6 <- matrix(0, nrow = n, ncol = 3)
    for(g in 1:n) {
        I6[g, 1] <- g
    }
    I6[, 2] <- HCdatamedian.id
    I6[, 3] <- data0[, p + 1]
    CPP6 <- IndexCPP(I6)

    # Ward's method
    HCdata.ward <- hclust(distance, method = "ward.D")
    HCdataward.id <- cutree(HCdata.ward, 3)
    I7 <- matrix(0, nrow = n, ncol = 3)
    for(g in 1:n) {
        I7[g, 1] <- g
    }
    I7[, 2] <- HCdataward.id
    I7[, 3] <- data0[, p + 1]
    CPP7 <- IndexCPP(I7)
  })
    return(list(Xnew = XEM, RMSE = RMSE, MMAE = MMAE, RRE = RRE, CPP1 = CPP1, CPP2 = CPP2, CPP3 = CPP3, CPP4 = CPP4, CPP5 = CPP5, CPP6 = CPP6, CPP7 = CPP7,timeEM = timeEM))
}
