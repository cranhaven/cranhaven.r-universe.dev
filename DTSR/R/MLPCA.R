#' Multilinear Principal Component Analysis with Missing Data
#'
#' This function performs Multilinear Principal Component Analysis (MLPCA) to handle missing data by imputing the missing values based on the correlation structure within the data. It also calculates the RMSE and Consistency Proportion Index (CPP) using different hierarchical clustering methods.
#'
#' @param data0 The original dataset containing the response variable and features.
#' @param data.sample The dataset used for sampling, which may contain missing values.
#' @param data.copy A copy of the original dataset, used for comparison or validation.
#' @param mr Indices of the rows with missing values that need to be predicted.
#' @param km The number of clusters for k-means clustering.
#' @return A list containing:
#' \item{Xnew}{The imputed dataset.}
#' \item{RMSE}{The Root Mean Squared Error.}
#' \item{CPP1}{The K-means clustering Consistency Proportion Index.}
#' \item{CPP2}{The Hierarchical Clustering Complete Linkage Consistency Proportion Index.}
#' \item{CPP3}{The Hierarchical Clustering Single Linkage Consistency Proportion Index.}
#' \item{CPP4}{The Hierarchical Clustering Average Linkage Consistency Proportion Index.}
#' \item{CPP5}{The Hierarchical Clustering Centroid linkage Consistency Proportion Index.}
#' \item{CPP6}{The Hierarchical Clustering Median Linkage Consistency Proportion Index.}
#' \item{CPP7}{The Hierarchical Clustering Ward's Method Consistency Proportion Index.}
#' \item{timeKNN}{The MLPCA algorithm execution time.}
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
#' # Perform MLPCA imputation
#' result <- MLPCA(data0, data.sample, data.copy, mr, km)
#' # Print the results
#' print(result$RMSE)
#' print(result$CPP1)
#' print(result$Xnew)
#'
#' @seealso \code{\link{princomp}} and \code{\link{svd}} for more information on PCA and SVD.
#' @keywords imputation MLPCA PCA SVD
#' @importFrom stats princomp  kmeans hclust cutree dist
MLPCA <- function(data0, data.sample, data.copy, mr, km) {
    X0 <- data.sample
    n <- nrow(X0); p <- ncol(X0)

   # Record the execution time
   timeMLPCA <- system.time({

    cm0 <- colMeans(X0, na.rm = TRUE)
    data.sample[is.na(data.sample)] <- cm0[ceiling(which(is.na(data.sample)) / n)]
    Xm <- X <- as.matrix(data.sample)
    pca <- princomp(Xm, cor = TRUE)
    PCA <- summary(pca, loadings = TRUE)
    D <- (pca$sdev)^2
    round(D, digits = 3)
    A <- PCA$loadings
    l <- D / sum(D)
    round(l, digits = 3)
    J <- rep(l, times = p); dim(J) <- c(p, p)
    upper.tri(J, diag = TRUE); J[lower.tri(J)] = 0; J
    ll <- matrix(colSums(J), nrow = 1, ncol = p, byrow = FALSE)
    round(ll, digits = 3)
    ww <- which(ll >= 0.7)
    k <- ww[1]
    tol <- 1e-10; nb <- 10; niter <- 0; d <- 1; SS = 1
    ZZ <- matrix(0, n, 1)
    Z <- scale(X, center = TRUE, scale = FALSE)
    R <- cor(Z)
    while ((SS >= tol) & (niter <= nb)) {
        niter <- niter + 1
        Zold <- Z
        R <- cor(Z); A <- svd(Z)$v
        Ak <- matrix(A[, 1:k], p, k)
        for (i in 1:n) {
            M <- is.na(X0[i,])
            job <- which(M == FALSE); jna <- which(M == TRUE)
            piob <- nrow(as.matrix(job)); pina <- nrow(as.matrix(jna))
            while ((piob > 0) & (pina > 0)) {
                Qi <- matrix(0, p, p)
                for (u in 1:piob) {
                    Qi[job[u], u] = 1
                }
                for (v in 1:pina) {
                    Qi[jna[v], v + piob] = 1
                }
                zi <- Z[i,]
                zQi <- zi %*% Qi
                ZQi <- Z %*% Qi
                AQi <- t(t(Ak) %*% Qi)
                ziob = matrix(zQi[, 1:piob], 1, piob)
                zina = matrix(zQi[, piob + (1:pina)], 1, pina)
                Ziob = matrix(ZQi[, 1:piob], n, piob, byrow = FALSE)
                Zina = matrix(ZQi[, piob + (1:pina)], n, pina, byrow = FALSE)
                Aiob = matrix(AQi[1:piob, ], piob, k, byrow = FALSE)
                Aina = matrix(AQi[piob + (1:pina), ], pina, k, byrow = FALSE)
                zinahat = t(Aina %*% t(Aiob) %*% t(ziob))
                ZQi[i, piob + (1:pina)] = zinahat
                Zi = ZQi %*% t(Qi)
                Z = Zi
                pina = 0
            }
        }
        Zrow = Znew = Z
        S1 = sum((Xm[mr] - Zrow[mr])^2)
        B = svd(Z)$u
        Bk = matrix(B[, 1:k], n, k)
        for (j in 1:p) {
            N = is.na(X0[, j])
            iob = which(N == FALSE); ina = which(N == TRUE)
            njob = nrow(as.matrix(iob)); njna = nrow(as.matrix(ina))
            while ((njob > 0) & (njna > 0)) {
                Qj = matrix(0, n, n)
                for (u in 1:njob) {
                    Qj[u, iob[u]] = 1
                }
                for (v in 1:njna) {
                    Qj[v + njob, ina[v]] = 1
                }
                zj = Z[, j]
                zQj = Qj %*% zj
                ZQj = Qj %*% Z
                BQj = t(t(Bk) %*% Qj)
                zjob = matrix(zQj[1:njob, ], njob, 1)
                zjna = matrix(zQj[njob + (1:njna), ], njna, 1)
                Zjob = matrix(ZQj[1:njob, ], njob, p, byrow = FALSE)
                Zjna = matrix(ZQj[njob + (1:njna), ], njna, p, byrow = FALSE)
                Bjob = matrix(BQj[1:njob, ], njob, k, byrow = FALSE)
                Bjna = matrix(t(BQj)[, njob + (1:njna)], njna, k, byrow = FALSE)
                zjnahat = Bjna %*% t(Bjob) %*% zjob
                ZQj[njob + (1:njna), j] = zjnahat
                Zj = t(Qj) %*% ZQj
                Z = Zj; njna = 0
            }
        }
        Zcol = Znew = Z
        S2 = sum((Xm[mr] - Zcol[mr])^2)
        SS = abs(S2 - S1) / S2
    }
    SS; niter
    XMLPCA = Xnew = Znew + matrix(rep(1, n * p), ncol = p) %*% diag(cm0)
    predicteds = XMLPCA[mr]
    actuals = data.copy[mr]
        # Calculate RMSE
    RMSE <- sqrt(base::mean((actuals - predicteds)^2))

    # Calculate MMAE
    MMAE <- base::mean(abs(predicteds - actuals))

    # Calculate RRE
    RRE <- RMSE / (max(actuals) - min(actuals))

    # K-means clustering
    s <- scale(XMLPCA)
    km <- kmeans(s, km)
    I1 <- matrix(0, nrow = n, ncol = 3)
    for (g in 1:n) {
        I1[g, 1] <- g
}
I1[, 2] <- km$cluster
I1[, 3] <- data0[, p + 1]
CPP1 <- IndexCPP(I1)

# Hierarchical clustering
HCdata <- XMLPCA
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
    I3[g, 1] <- g
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
return(list(Xnew = XMLPCA, RMSE = RMSE, MMAE = MMAE, RRE = RRE, CPP1 = CPP1, CPP2 = CPP2, CPP3 = CPP3, CPP4 = CPP4, CPP5 = CPP5, CPP6 = CPP6, CPP7 = CPP7,timeMLPCA = timeMLPCA))
}
