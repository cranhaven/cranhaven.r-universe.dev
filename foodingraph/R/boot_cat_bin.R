#' Bootstrap inference on binary and categorical variables
#'
#' For a given dataset, performs a confidence-interval bootstrap of
#' the mutual information or maximal information coefficient (MIC)
#' for each pairwise association.
#' \enumerate{
#'   \item Computes the MI or MIC for each pairwise association.
#'   \item Performs a bootstrap (of \code{boots} samples), and store
#'   each pairwise association
#'   \item Calculate the 1th percentile for each pairwise association
#'   from the bootstrap distribution
#'   \item If the percentile is inferior to the threshold of the
#'   corresponding pairwise variable type, then the MI or MIC is set to 0.
#' }
#'
#' @param obs_data (data.frame or matrix) : a dataset which rows are
#'  observations and columns the variables.
#' @param list_cat_var  : list of the categorical variables of the dataset
#' @param list_bin_var : list of the binary variables of the dataset
#' @param threshold_bin : the threshold to apply to binary pairwise
#' associations
#' @param threshold_cat : the threshold to apply to categorical pairwise
#' associations
#' @param threshold_bin_cat : to apply to a pairwise association between
#'  a binary and a categorical variable
#' @param method : the method to use to compute the adjacency matrix
#' ("mi" or "mic").
#' If "mi", uses mutual information package \code{minet},
#' and Miller-Madow estimator.
#' If "mic", uses maximal information coefficient from \code{minerva}
#' package function \code{cstats()}
#' @param boots : number of bootstraps (default 5000)
#' @param show_progress : if TRUE, prints the percentage of completion to
#'  keep track of the algorithm's progress.
#'  Default is TRUE. Recommended to FALSE for RMarkdown files.
#'
#' @return The inferred adjacency matrix. All bootstrap 1th percentile
#' values of each pairwise association
#' inferior to their predefined thresholds will be set to 0.
#' @export
boot_cat_bin <- function(obs_data,
                         list_cat_var,
                         list_bin_var,
                         threshold_bin,
                         threshold_cat,
                         threshold_bin_cat,
                         method = c("mi", "mic"),
                         boots = 5000,
                         show_progress = TRUE) {

  method <- match.arg(method)
  message(paste("Performing boostrap inference with method : ", method))

  n_col_data <- ncol(obs_data)
  n_row_data <- nrow(obs_data)
  colnames_data <- colnames(obs_data)

  # Compute the adjacency matix for each pairwise association
  # on the original sample
  if (method == "mi") {
    if (!requireNamespace("minet", quietly = TRUE)) {
      stop("Package \"minet\" needed for this function to work.
           Please install it or use \"mic\".")
    }
    adj_matrix <- minet::build.mim(obs_data, estimator = "mi.mm", disc = "none")
  } else if (method == "mic") {
    adj_matrix <- mic_adj_matrix(obs_data)
  }

  colnames(adj_matrix) <- colnames_data
  rownames(adj_matrix) <- colnames_data

  if (show_progress == TRUE) {
    pbar <- txtProgressBar(max = boots, style = 3)
  }

  # Compute the adjacency matrix for each pairwise association
  # of each bootstrap sample
  # The results are temporarily stored in a 3-dim matrix, the 3rd dim
  # is for the bootstrap sample number
  adj_matrix_star <- array(NA, dim = c(n_col_data, n_col_data, boots))
  for (bootno in 1:boots) {
    boot_indices <- sample(1:n_row_data, n_row_data, replace = TRUE)
    boot_sample <- obs_data[boot_indices,]

    if (show_progress == TRUE) {
      setTxtProgressBar(pbar, bootno)
    }

    if (method == "mi") {
      adj_matrix_star[,,bootno] <- minet::build.mim(boot_sample, estimator = "mi.mm",
                                                    disc = "none")
    } else if (method == "mic") {
      adj_matrix_star[,,bootno] <- mic_adj_matrix(boot_sample)
    }
  }

  if (show_progress == TRUE) {
    close(pbar)
  }

  # Compute the 1th percentile for each pairwise association
  # According to the type of pairwise association (e.g. 1 binary variable
  # and 1 ordinal variable), select the threshold accordingly
  # (from bootstrap simulations of binary and ordinal data)
  for (i in 1:n_col_data) {
    for (j in 1:n_col_data) {

      if( i == j ) {
        next
      }

      quantile_ij <- quantile(adj_matrix_star[i,j,], 0.01)

      if (colnames_data[i] %in% list_cat_var &&
          colnames_data[j] %in% list_cat_var) {
        # 2 ordinal variables
        adj_matrix[i,j] <- ifelse(quantile_ij > threshold_cat, adj_matrix[i,j], 0)

      } else if (colnames_data[i] %in% list_bin_var &&
                 colnames_data[j] %in% list_bin_var) {
        # 2 binary variables
        adj_matrix[i,j] <- ifelse(quantile_ij > threshold_bin, adj_matrix[i,j], 0)

      } else {
        # 1 binary variable and 1 ordinal variable
        adj_matrix[i,j] <- ifelse(quantile_ij > threshold_bin_cat, adj_matrix[i,j], 0)
      }
    }
  }

  adj_matrix
}
