# Improved SVD Imputation with Comprehensive Evaluation
#'
#' This function performs imputation using Singular Value Decomposition (SVD) and calculates various evaluation metrics including RMSE, MMAE, RRE, and Consistency Proportion Index (CPP) using different hierarchical clustering methods.
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
#' \item{timeSVD}{The SVD algorithm execution time.}
#' @export
#'
#' @seealso \code{\link{princomp}} and \code{\link{svd}} for more information on PCA and SVD.
#' @keywords imputation SVD PCA SVD
#' @importFrom stats kmeans hclust cutree dist
SVD <- function(data0, data.sample, data.copy, mr, km) {
    data.sample <- as.matrix(data.sample)
    n <- nrow(data.sample); p <- ncol(data.sample)
    rk <- qr(data.copy)$rank
    XSVD <- SVDImpute(data.sample, rk)# Using the SVDImpute function from SVDImpute.R

    # Record the execution time
    timeSVD <- system.time({
    # Calculate RMSE
    predicteds <- XSVD[mr, ]
    actuals <- data.copy[mr]
    RMSE <- sqrt(base::mean((actuals - predicteds)^2))

    # Calculate MMAE
    MMAE <- base::mean(abs(predicteds - actuals))

    # Calculate RRE
    RRE <- sum(abs(predicteds - actuals)) / sum(actuals)

    # IndexCPP


    # K-means clustering
    s <- scale(XSVD)
    km <- kmeans(s, km)
    I1 <- matrix(0, nrow = n, ncol = 3)
    for (g in 1:n) {
        I1[g, 1] <- g
    }
    I1[, 2] <- km$cluster
    I1[, 3] <- data0[, p + 1]
    CPP1 <- IndexCPP(I1)

    # Hierarchical clustering
    HCdata <- XSVD
    distance <- dist(HCdata)

    # Complete linkage
    HCdata.hc <- hclust(distance)
    HCdata.id <- cutree(HCdata.hc, 3)
    I2 <- matrix(0, nrow = n, ncol = 3)
    for (g in 1:n) {
        I2[g, 1] <- g
    }
    I2[, 2] <- HCdata.id
    I2[, 3] <- data0[, p + 1]
    CPP2 <- IndexCPP(I2)

    # Single linkage
    HCdata.single <- hclust(distance, method = "single")
    HCdatasingle.id <- cutree(HCdata.single, 3)
    I3 <- matrix(0, nrow = n, ncol = 3)
    for (g in 1:n) {
        I3[g, 2] <- g
    }
    I3[, 2] <- HCdatasingle.id
    I3[, 3] <- data0[, p + 1]
    CPP3 <- IndexCPP(I3)

    # Average linkage
    HCdata.average <- hclust(distance, method = "average")
    HCdataaverage.id <- cutree(HCdata.average, 3)
    I4 <- matrix(0, nrow = n, ncol = 3)
    for (g in 1:n) {
        I4[g, 1] <- g
    }
    I4[, 2] <- HCdataaverage.id
    I4[, 3] <- data0[, p + 1]
    CPP4 <- IndexCPP(I4)

    # Centroid linkage
    HCdata.centroid <- hclust(distance, method = "centroid")
    HCdatacentroid.id <- cutree(HCdata.centroid, 3)
    I5 <- matrix(0, nrow = n, ncol = 3)
    for (g in 1:n) {
        I5[g, 1] <- g
    }
    I5[, 2] <- HCdatacentroid.id
    I5[, 3] <- data0[, p + 1]
    CPP5 <- IndexCPP(I5)

    # Median linkage
    HCdata.median <- hclust(distance, method = "median")
    HCdatamedian.id <- cutree(HCdata.median, 3)
    I6 <- matrix(0, nrow = n, ncol = 3)
    for (g in 1:n) {
        I6[g, 1] <- g
    }
    I6[, 2] <- HCdatamedian.id
    I6[, 3] <- data0[, p + 1]
    CPP6 <- IndexCPP(I6)

    # Ward's method
    HCdata.ward <- hclust(distance, method = "ward.D")
    HCdataward.id <- cutree(HCdata.ward, 3)
    I7 <- matrix(0, nrow = n, ncol = 3)
    for (g in 1:n) {
        I7[g, 1] <- g
    }
    I7[, 2] <- HCdataward.id
    I7[, 3] <- data0[, p + 1]
    CPP7 <- IndexCPP(I7)
 })
    return(list(Xnew = XSVD, RMSE = RMSE, MMAE = MMAE, RRE = RRE, CPP1 = CPP1, CPP2 = CPP2, CPP3 = CPP3, CPP4 = CPP4, CPP5 = CPP5, CPP6 = CPP6, CPP7 = CPP7, timeSVD = timeSVD))
}
