#' @title Check Exploratory Factor Analysis Suitability
#' @description Checks if specified features in a dataframe meet criteria for performing exploratory factor analysis (EFA).
#' This function verifies that each feature exists, is numeric, has sufficient variability,
#' and does not have an excessive proportion of missing values. For multiple features, it also
#' assesses the full rank of the correlation matrix and the level of intercorrelation among features.
#' @author E. F. Haghish
#'
#' @param df A dataframe containing the features.
#' @param features A character vector of feature names to be evaluated.
#' @param min_unique An integer specifying the minimum number of unique non-missing
#'   values required for a feature. Default is 5.
#' @param min_intercorrelation A numeric threshold for the minimum acceptable
#'   intercorrelation among features. (Note: this parameter is not used explicitly in the current implementation.) Default is 0.3.
#' @param verbose Logical; if \code{TRUE}, a confirmation message is printed when all
#'   features appear suitable. Default is \code{FALSE}.
#'
#' @return \code{TRUE} if all features are deemed suitable for EFA, and \code{FALSE}
#'   otherwise. In the latter case, messages detailing the issues are printed.
#'
#' @details
#' The function performs several checks:
#' \describe{
#'   \item{Existence}{Verifies that each feature in \code{features} is present in \code{df}.}
#'   \item{Numeric Type}{Checks that each feature is numeric.}
#'   \item{Variability}{Ensures that each feature has at least \code{min_unique} unique non-missing values.}
#'   \item{Missing Values}{Flags features with more than 20\% missing values.}
#' }
#'
#' If more than one feature is provided, the function computes the correlation matrix
#' (using pairwise complete observations) and checks:
#' \describe{
#'   \item{Full Rank}{Whether the correlation matrix is full rank. A rank lower than the
#'     number of features indicates redundancy.}
#'   \item{Intercorrelations}{Identifies features that do not have any correlation (>= 0.4)
#'     with the other features.}
#' }
#'
#' @importFrom stats na.omit cor
#'
#' @examples
#'   # Example: assess feature suitability for EFA using the USJudgeRatings dataset.
#'   # this dataset contains ratings on several aspects of U.S. federal judges' performance.
#'   # Here, we check whether these rating variables are suitable for EFA.
#'   data("USJudgeRatings")
#'   features_to_check <- colnames(USJudgeRatings[,-1])
#'   result <- check_efa(
#'     df = USJudgeRatings,
#'     features = features_to_check,
#'     min_unique = 3,
#'     verbose = TRUE
#'   )
#'
#'   # TRUE indicates the features are suitable.
#'   print(result)
#'
#' @export

check_efa <- function(df,
                      features,
                      min_unique = 5,
                      min_intercorrelation = .3,
                      verbose = FALSE) {

  # Vector to store messages for unsuitable features
  unsuitable_messages <- c()
  minimum_unique_observations <- c()

  # Check if given features are included in the dataframe
  # ====================================================
  for (feature in features) {
    if (!feature %in% names(df)) {
      unsuitable_messages <- c(
        unsuitable_messages,
        paste0("Feature '", feature, "' not found in the dataframe.")
      )
      next
    }

    col_data <- df[[feature]]

    # Check if the column is numeric
    # ====================================================
    if (!is.numeric(col_data)) {
      unsuitable_messages <- c(
        unsuitable_messages,
        paste0("Feature '", feature, "' is not numeric.")
      )
    }

    # Check for sufficient variability (at least 2 unique non-missing values)
    # ====================================================
    if (length(unique(na.omit(col_data))) < min_unique) {
      minimum_unique_observations <- c(minimum_unique_observations, feature)
    }

    # Check for a high proportion of missing values (threshold: 20%)
    # ====================================================
    if (mean(is.na(col_data)) > 0.2) {
      unsuitable_messages <- c(
        unsuitable_messages,
        paste0("Feature '", feature, "' has more than 20% missing values.")
      )
    }
  }

  if (length(minimum_unique_observations) > 0) {
    unsuitable_messages <- c(
      unsuitable_messages,
      paste0("The following features have constant or near-constant values. Make sure it is not categorical!:\n '",
             paste0(minimum_unique_observations, collapse = " "))
    )
  }


  # If more than one feature exists, check the correlation matrix rank
  # ====================================================
  if (length(features) > 1 && all(features %in% names(df))) {
    sub_df <- df[features]
    cor_matrix <- try(cor(sub_df, use = "pairwise.complete.obs"), silent = TRUE)
    if (inherits(cor_matrix, "try-error")) {
      message("Error computing correlation matrix for features. Check for missing values or insufficient variation.")
    } else {
      rank_cor <- qr(cor_matrix)$rank
      if (rank_cor < length(features)) {
        unsuitable_messages <- c(
          unsuitable_messages,
          "The correlation matrix of the features is not full rank; some variables may be redundant."
        )
      }
    }

    # Identify features with low intercorrelations
    #p.mat <- cor.mtest(efDf)

    # Identify items that have low correlations with other items
    N <- abs(cor_matrix) >= .4
    intercorrelations <- rowSums(N) - 1 # -1 because the item is always correlated with itself

    poor_features <- names(intercorrelations[intercorrelations==0])
    if (length(poor_features) > 0) {
      unsuitable_messages <- c(
        unsuitable_messages,
        paste0("The following features have low intercorrelations:\n '", paste0(poor_features, collapse = " "))
      )
    }
  }

  # Print messages if any issues are found
  # ====================================================
  if (length(unsuitable_messages) > 0) {
    #message("The following issues were found with the features:")
    for (msg in unsuitable_messages) {
      message(msg)
    }
    return(FALSE)
  } else {
    if (verbose) cat("All features appear suitable for exploratory factor analysis with the minrank algorithm.\n")
    return(TRUE)
  }
}
