#' Distributed Robust Principal Component Analysis (DRPCA) for Handling Missing Data
#'
#' This function performs DRPCA to handle missing data by dividing the dataset into D blocks,
#' applying the Robust Principal Component Analysis (RPCA) method to each block, and then combining
#' the results. It calculates various evaluation metrics including RMSE, MMAE, RRE, and
#' Generalized Cross-Validation (GCV) using different hierarchical clustering methods.
#'
#' @param data0 The original dataset containing the response variable and features.
#' @param data.sample The dataset used for sampling, which may contain missing values.
#' @param data.copy A copy of the original dataset, used for comparison or validation.
#' @param mr Indices of the rows with missing values that need to be predicted.
#' @param km The number of clusters for k-means clustering.
#' @param D The number of blocks to divide the data into.
#' @return A list containing:
#' \item{XDRPCA}{The imputed dataset.}
#' \item{RMSEDRPCA}{The Root Mean Squared Error.}
#' \item{MAEDRPCA}{The Mean Absolute Error.}
#' \item{REDRPCA}{The Relative Eelative Error.}
#' \item{GCVDRPCA}{Distributed DRPCA Imputation for Generalized Cross-Validation.}
#' \item{timeDRPCA}{The DRPCA algorithm execution time.}
#' @export
#'
#' @examples
#' # Create a sample dataset with missing values
#' set.seed(123)
#' n <- 100
#' p <- 10
#' D <- 2
#' data.sample <- matrix(rnorm(n * p), nrow = n)
#' data.sample[sample(1:(n-10), (p-2))] <- NA
#' data.copy <- data.sample
#' data0 <- data.frame(data.sample, response = rnorm(n))
#' mr <- sample(1:n, 10)  # Sample rows for evaluation
#' km <- 3  # Number of clusters
#' result <- DRPCA(data0, data.sample, data.copy, mr, km, D)
#' #Print the results
#' print(result$XDRPCA)
#' @seealso \code{\link{RPCA}} for the original RPCA function.
#' @keywords imputation DRPCA RPCA PCA SVD
DRPCA <- function(data0, data.sample, data.copy, mr, km, D) {
  # Get the dimensions of the dataset
  n <- nrow(data.sample)  # Number of rows (observations)
  p <- ncol(data.sample)  # Number of columns (variables)

  # Initialize a matrix to store the results
  XX <- matrix(0, n, p)

  # Initialize vectors to store evaluation metrics for each block
  RMSE <- vector("numeric", D)  # Root Mean Squared Error
  MMAE <- vector("numeric", D)  # Mean Absolute Error
  RRE <- vector("numeric", D)  # Relative Root Error

  # Record the execution time
  timeDRPCA <- system.time({
    for (d in 1:D) {
      # Calculate the size of each block
      n_Id <- ceiling(n / D)

      # Randomly select a block
      nn_start <- (d - 1) * n_Id + 1
      nn_end <- min(d * n_Id, n)
      nn <- nn_start:nn_end

      # Extract the current block's data
      dataP_Id <- data.sample[nn, ]
      data0P_Id <- data0[nn, ]
      mr_Id <- mr[nn]

      # Apply the RPCA method to the current block's missing values
      RPCA_result_Id <- RPCA(data0P_Id, dataP_Id, data.copy[nn, ], mr_Id, km)

      # Put the processed data back into the original position
      XX[nn, ] <- RPCA_result_Id$Xnew

      # Record the evaluation metrics
      RMSE[d] <- RPCA_result_Id$RMSE  # Assuming MSE is returned by RPCA
      MMAE[d] <- RPCA_result_Id$MMAE
      RRE[d] <- RPCA_result_Id$RRE
    }

    # Apply clustering to the entire dataset
    s <- scale(XX)
    km_result <- kmeans(s, centers = km)

    # Calculate overall evaluation metrics
    RMSEDRPCA <- base::mean(RMSE)
    MAEDRPCA <- base::mean(MMAE)
    REDRPCA <- base::mean(RRE)

    # Calculate Generalized Cross-Validation (GCV)
    lambdaDRPCA <- svd(cor(XX))$d
    lDRPCA <- lambdaDRPCA / sum(lambdaDRPCA)
    J <- rep(lDRPCA, times = p); dim(J) <- c(p, p)
    upper.tri(J, diag = TRUE); J[lower.tri(J)] <- 0
    etaDRPCA <- matrix(colSums(J), nrow = 1, ncol = p, byrow = FALSE)
    wwDRPCA <- which(etaDRPCA >= 0.7)
    kDRPCA <- wwDRPCA[1]
    lambdaDRPCApk <- lambdaDRPCA[(kDRPCA + 1):p]
    GCVDRPCA <- sum(lambdaDRPCApk) * p / ((p - kDRPCA)^2)
  })

  # Return a list containing the imputed dataset and evaluation metrics
  return(list(XDRPCA = XX, RMSEDRPCA = RMSEDRPCA, MAEDRPCA = MAEDRPCA, REDRPCA = REDRPCA, GCVDRPCA = GCVDRPCA, timeDRPCA = timeDRPCA))
}
