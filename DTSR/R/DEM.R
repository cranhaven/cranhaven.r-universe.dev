#' Distributed EM Imputation (DEM) for Handling Missing Data
#'
#' This function performs DEM to handle missing data by dividing the dataset into D blocks,
#' applying the EM imputation method within each block, and then combining the results.
#' It calculates various evaluation metrics including RMSE, MMAE, RRE, and Consistency
#' Proportion Index (CPP) using different hierarchical clustering methods.
#'
#' @param data0 The original dataset containing the response variable and features.
#' @param data.sample The dataset used for sampling, which may contain missing values.
#' @param data.copy A copy of the original dataset, used for comparison or validation.
#' @param mr Indices of the rows with missing values that need to be predicted.
#' @param km The number of clusters for k-means clustering.
#' @param D The number of blocks to divide the data into.
#' @return A list containing:
#' \item{XDEM}{The imputed dataset.}
#' \item{RMSEDEM}{The Root Mean Squared Error.}
#' \item{MAEDEM}{The Mean Absolute Error.}
#' \item{REDEM}{The Relative Eelative Error.}
#' \item{GCVDEM}{The DEM Imputation for Generalized Cross-Validation.}
#' \item{timeDEM}{The DEM algorithm execution time.}
#' @export
#'
#' @examples
#' # Create a sample dataset with missing values
#' set.seed(123)
#' n <- 100
#' p <- 5
#' data.sample <- matrix(rnorm(n * p), nrow = n)
#' data.sample[sample(1:(n*p), 20)] <- NA
#' data.copy <- data.sample
#' data0 <- data.frame(data.sample, response = rnorm(n))
#' mr <- sample(1:n, 10)  # Sample rows for evaluation
#' km <- 3  # Number of clusters
#' D <- 2  # Number of blocks
#' # Perform DEM imputation
#' result <- DEM(data0, data.sample, data.copy, mr, km, D)
#' # Print the results
#' print(result$XDEM)
#'
#' @seealso \code{\link{EM}} for the original EM function.
#' @keywords imputation DEM EM k-means clustering
#' @importFrom stats kmeans cor

DEM <- function(data0, data.sample, data.copy, mr, km, D) {
  n <- nrow(data.sample)
  p <- ncol(data.sample)

  # Initialize a matrix to store the results
  XX <- matrix(0, n, p)

  # Initialize vectors to store evaluation metrics for each block
  RMSE <- vector("numeric", D)  # Root Mean Squared Error
  MMAE <- vector("numeric", D)  # Mean Absolute Error
  RRE <- vector("numeric", D)  # Relative Root Error

  # Record the execution time
  timeDEM <- system.time({
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

      # Apply the EM method to the current block's missing values
      EM_result_Id <- EM(data0P_Id, dataP_Id, data.copy[nn, ], mr_Id, km)

      # Put the processed data back into the original position
      XX[nn, ] <- EM_result_Id$Xnew

      # Record the evaluation metrics
      RMSE[d] <- EM_result_Id$RMSE
      MMAE[d] <- EM_result_Id$MMAE
      RRE[d] <- EM_result_Id$RRE
    }

    # Apply clustering to the entire dataset
    s <- scale(XX)
    km_result <- kmeans(s, centers = km)

    # Calculate overall evaluation metrics
    RMSEDEM <- base::mean(RMSE)
    MAEDEM <- base::mean(MMAE)
    REDEM <- base::mean(RRE)

    # Calculate Generalized Cross-Validation (GCV)
    lambdaDEM <- svd(cor(XX))$d
    lDEM <- lambdaDEM / sum(lambdaDEM)
    J <- rep(lDEM, times = p); dim(J) <- c(p, p)
    upper.tri(J, diag = TRUE); J[lower.tri(J)] <- 0
    etaDEM <- matrix(colSums(J), nrow = 1, ncol = p, byrow = FALSE)
    wwDEM <- which(etaDEM >= 0.7)
    kDEM <- wwDEM[1]
    lambdaDEMpk <- lambdaDEM[(kDEM + 1):p]
    GCVDEM <- sum(lambdaDEMpk) * p / ((p - kDEM)^2)
  })

  # Return a list containing the imputed dataset and evaluation metrics
  return(list(XDEM = XX, RMSEDEM = RMSEDEM, MAEDEM = MAEDEM, REDEM = REDEM, GCVDEM = GCVDEM, timeDEM = timeDEM))
}
