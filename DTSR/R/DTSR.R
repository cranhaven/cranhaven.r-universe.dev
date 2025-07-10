#' Distributed Trimmed Scores Regression (DTSR) for Handling Missing Data
#'
#' This function performs DTSR to handle missing data by dividing the dataset into D blocks,
#' applying the Trimmed Scores Regression (TSR) method to each block, and then combining
#' the results. It calculates various evaluation metrics including RMSE, MMAE, RRE, and
#' Consistency Proportion Index (CPP) using different hierarchical clustering methods.
#'
#' @param data0 The original dataset containing the response variable and features.
#' @param data.sample The dataset used for sampling, which may contain missing values.
#' @param data.copy A copy of the original dataset, used for comparison or validation.
#' @param mr Indices of the rows with missing values that need to be predicted.
#' @param km The number of clusters for k-means clustering.
#' @param D The number of blocks to divide the data into.
#' @return A list containing:
#' \item{XDTSR}{The imputed dataset.}
#' \item{RMSEDTSR}{The Root Mean Squared Error.}
#' \item{MAEDTSR}{The Mean Absolute Error.}
#' \item{REDTSR}{The Relative Eelative Error.}
#' \item{GCVDTSR}{The DTSR for Generalized Cross-Validation.}
#' \item{timeDTSR}{The DTSR algorithm execution time.}
#' @export
#'
#' @examples
#' # Create a sample matrix with random values and introduce missing values
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
#' # Perform DTSR imputation
#' result <- DTSR(data0, data.sample, data.copy, mr, km,D)
#' # Print the results
#' print(result$XDTSR)
#' @seealso \code{\link{TSR}} for the original TSR function.
#' @keywords imputation DTSR TSR PCA SVD
DTSR <- function(data0, data.sample, data.copy, mr, km, D) {
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
  timeDTSR <- system.time({
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

      # Apply the TSR method to the current block's missing values
      TSR_result_Id <- TSR(data0 = data0P_Id, data.sample = dataP_Id, data.copy = data.copy[nn, ], mr = mr_Id, km)

      # Put the processed data back into the original position
      XX[nn, ] <- TSR_result_Id$Xnew

      # Record the evaluation metrics
      RMSE[d] <- TSR_result_Id$RMSE
      MMAE[d] <- TSR_result_Id$MMAE
      RRE[d] <- TSR_result_Id$RRE
    }

    # Apply clustering to the entire dataset
    s <- scale(XX)
    km_result <- kmeans(s, centers = km)

    # Calculate overall evaluation metrics
    RMSEDTSR <- base::mean(RMSE)
    MAEDTSR <- base::mean(MMAE)
    REDTSR <- base::mean(RRE)

    # Calculate Generalized Cross-Validation (GCV)
    lambdaDTSR <- svd(cor(XX))$d
    lDTSR <- lambdaDTSR / sum(lambdaDTSR)
    J <- rep(lDTSR, times = p); dim(J) <- c(p, p)
    upper.tri(J, diag = TRUE); J[lower.tri(J)] <- 0
    etaDTSR <- matrix(colSums(J), nrow = 1, ncol = p, byrow = FALSE)
    wwDTSR <- which(etaDTSR >= 0.7)
    kDTSR <- wwDTSR[1]
    lambdaDTSRpk <- lambdaDTSR[(kDTSR + 1):p]
    GCVDTSR <- sum(lambdaDTSRpk) * p / ((p - kDTSR)^2)
  })

  # Return a list containing the imputed dataset and evaluation metrics
  return(list(XDTSR = XX, RMSEDTSR = RMSEDTSR, MAEDTSR = MAEDTSR, REDTSR = REDTSR, GCVDTSR = GCVDTSR, timeDTSR = timeDTSR))
}
