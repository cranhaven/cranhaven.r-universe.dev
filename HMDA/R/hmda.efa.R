#' @importFrom psych fa.diagram fa.sort factor.scores omega
#' @importFrom h2otools capture
#' @title Perform Exploratory Factor Analysis with HMDA
#' @description Performs exploratory factor analysis (EFA) on a specified set
#'   of features from a data frame using the \pkg{psych} package. The function
#'   optionally runs parallel analysis to recommend the number of factors, applies
#'   a rotation method, reverses specified features, and cleans up factor loadings
#'   by zeroing out values below a threshold. It then computes factor scores and
#'   reliability estimates, and finally returns a list containing the EFA results,
#'   cleaned loadings, reliability metrics, and factor correlations.
#'
#' @param df               A data frame containing the items for EFA.
#' @param features         A vector of feature names (or indices) in \code{df} to
#'                         include in the factor analysis.
#' @param algorithm        Character. The factor extraction method to use.
#'                         Default is \code{"minres"}. Other methods supported by
#'                         \pkg{psych} (e.g., "ml", "minchi") may also be used.
#' @param rotation         Character. The rotation method to apply to the factor
#'                         solution. Default is \code{"promax"}.
#' @param parallel.analysis Logical. If \code{TRUE}, runs parallel analysis using
#'                         \code{psych::fa.parallel} to recommend the number of
#'                         factors. Default is \code{TRUE}.
#' @param nfactors         Integer. The number of factors to extract. If \code{NULL}
#'                         and \code{parallel.analysis = TRUE}, the number of
#'                         factors recommended by the parallel analysis is used.
#' @param dict             A data frame dictionary with at least two columns:
#'                         \code{"name"} and \code{"description"}. Used to replace
#'                         feature names with human-readable labels. Default is
#'                         \code{dictionary(df, attribute = "label")}.
#' @param minimum_loadings Numeric. Any factor loading with an absolute value
#'                         lower than this threshold is set to zero. Default is
#'                         \code{0.30}.
#' @param exclude_features Character vector. Features to exclude from the analysis.
#'                         Default is \code{NULL}.
#' @param ignore_binary    Logical. If \code{TRUE}, binary items may be ignored
#'                         in the analysis. Default is \code{TRUE}.
#' @param intercorrelation Numeric. (Unused in current version) Intended to set
#'                         a minimum intercorrelation threshold between items.
#'                         Default is \code{0.3}.
#' @param reverse_features  A vector of feature names for which the scoring
#'                         should be reversed prior to analysis. Default is
#'                         \code{NULL}.
#' @param plot             Logical. If \code{TRUE}, a factor diagram is plotted
#'                         using \code{psych::fa.diagram}. Default is \code{FALSE}.
#' @param factor_names     Character vector. Optional names to assign to the
#'                         extracted factors (i.e., new column names for loadings).
#' @param verbose Logical. If \code{TRUE}, the factor loadings are printed in the console.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{parallel.analysis}{The output from the parallel analysis, if run.}
#'     \item{efa}{The full exploratory factor analysis object returned by
#'                \code{psych::fa}.}
#'     \item{efa_loadings}{A matrix of factor loadings after zeroing out values
#'                below the \code{minimum_loadings} threshold, rounded and sorted.}
#'     \item{efa_reliability}{The reliability results (omega) computed from the
#'                factor scores.}
#'     \item{factor_correlations}{A matrix of factor correlations, rounded to 2
#'                decimal places.}
#'   }
#'
#' @details
#'   This function first checks that the number of factors is either provided
#'   or determined via parallel analysis (if \code{parallel.analysis} is \code{TRUE}).
#'   A helper function \code{trans()} is defined to reverse and standardize item
#'   scores for features specified in \code{reverse_features}. Unwanted features can be
#'   excluded via \code{exclude_features}. The EFA is then performed using
#'   \code{psych::fa()} with the chosen extraction algorithm and rotation method.
#'   Loadings are cleaned by zeroing out values below the \code{minimum_loadings}
#'   threshold, rounded, and sorted. Factor scores are computed with
#'   \code{psych::factor.scores()} and reliability is estimated using the
#'   \code{omega()} function. Finally, factor correlations are extracted from the
#'   EFA object.
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
#   # Running the EFA analysis (with parallel analysis)
#   # ============================================================
#   efa_results <- hmda.efa(df = USJudgeRatings,
#                           features = colnames(USJudgeRatings[,-1]),
#                           parallel.analysis = TRUE,
#                           algorithm = "ml",
#                           rotation = "promax",
#                           minimum_loadings = 0.30,
#                           plot = TRUE)
#
#   # View the sorted factor loadings
#   print(efa_results$efa_loadings)
#
#   # View the reliability analysis
#   print(efa_results$efa_reliability)
#
#   # View the factor correlation matrix
#   print(efa_results$factor_correlations)
#
#
#' @export
#' @author E. F. Haghish

hmda.efa <- function(df,
                     features,
                     algorithm = "minres",
                     rotation = "promax",
                     parallel.analysis = TRUE,
                     nfactors = NULL,
                     dict = dictionary(df, attribute = "label"),
                     minimum_loadings = 0.30,
                     exclude_features = NULL,
                     ignore_binary = TRUE,
                     intercorrelation = 0.3,
                     reverse_features = NULL,
                     plot = FALSE,
                     factor_names = NULL,
                     verbose = TRUE) {

  pa <- NULL

  # Check the data for items not suitable for efa
  # ====================================================
  if (is.null(nfactors) & !parallel.analysis) stop("either run parallel.analysis or specify number of factors in 'nfactor' argument")

  # Function to reverse the variable's direction
  # ====================================================
  trans <- function(x, reverse = FALSE) {
    if (reverse) x <- -x
    x <- (x - min(x, na.rm = T)) / diff(range(x, na.rm = TRUE))
    return(x)
  }

  if (!is.null(reverse_features)) {
    for (i in reverse_features) df[,i] <- trans(df[,i], reverse = TRUE)
  }

  # Exclude unwanted features
  # ====================================================
  if (!is.null(exclude_features)) {
    features <- features[!features %in% exclude_features]
  }

  # Compute number of factors
  # ====================================================
  if (parallel.analysis){
    pa <- capture(psych::fa.parallel(df[, features], fa="fa", fm=algorithm, plot = plot))
    # if (sum(pa$value$fa.values > 0) > pa$value$nfact) {
    #   message(paste(pa$value$nfact, "factors are recommended, but there might be better solutions (for example, up to", sum(pa$value$fa.values > 0), "factors)"))
    # }
    # else message(paste(pa$value$nfact, "factors are recommended"))
    message(paste(pa$value$nfact, "factors are recommended"))
  }

  # Run the exploratory factor analysis
  # ====================================================
  EFAresults <- psych::fa(r=df[, features],
                          nfactors = if (!is.null(nfactors)) nfactors else pa$value$nfact,
                          fm = algorithm,
                          rotate = rotation)



  loadings <- EFAresults$loadings
  #
  # EFAresults <- factanal(~ .,
  #                        data = df[, features],
  #                        factors = if (!is.null(nfactors)) nfactors else pa$value$nfact,
  #                        rotation = rotation,
  #                        na.action = na.exclude)

  loadings <- EFAresults$loadings

  # Add the labels to the table
  if (!is.null(dict)) {
    for (i in 1:length(rownames(loadings))) {
      index <- dict[,1] == rownames(loadings)[i]
      if (sum(index) == 1) rownames(loadings)[i] <- dict[index, 2]
    }
  }

  # tidy up the loadings
  loadings[loadings > - minimum_loadings & loadings < minimum_loadings] <- 0
  if (!is.null(factor_names)) colnames(loadings) <- factor_names
  loadings <- round(loadings, 2)
  loadings <- fa.sort(loadings)

  if (verbose) {
    cat(capture.output(print(loadings)), sep = "\n")
    cat("\n")
  }

  # a better plot would be nice ???
  if (plot) fa.diagram(EFAresults)

  # Factor reliability
  # ====================================================

  # 1. Calculate factor scores
  factor_scores <- factor.scores(df[, features], EFAresults)$scores

  # 2. Compute reliability for each factor
  reliability_results <- omega(factor_scores)
  if (verbose) {
    cat(capture.output(print(reliability_results)), sep = "\n")
    cat("\n")
  }


  # Factor correlation
  # ====================================================
  factor_correlations <- round(EFAresults$Phi,2)

  # Prepare the results
  # ====================================================
  results <- list(
    parallel.analysis = pa,
    efa = EFAresults,
    efa_loadings = loadings,
    efa_reliability = reliability_results,
    factor_correlations = factor_correlations
  )

  return(results)
}

# importantFeatures <- hmda.features(wmshap, method = "mean", cutoff = 0.01)
# importantFeatures <- importantFeatures[importantFeatures %in% colnames(raw)]
# check_efa(raw, importantFeatures)
#pa <- psych::fa.parallel(raw[, importantFeatures], fa="fa", fm="minrank", plot=FALSE)

# importantFeatures <- hmda.features(wmshap, method = "mean", cutoff = 0.008)
# importantFeatures <- hmda.features(wmshap, top_n_features = 30)
# importantFeatures <- hmda.features(wmshap, method = "lowerCI", cutoff = 0.1)

# importantFeatures <- hmda.feature.selection(wmshap, method = "mean", cutoff = 0.005)
# importantFeatures$important
#
# check_efa(df=raw, features = importantFeatures$important, min_intercorrelation = 0.1)

# hmda.efa(df=raw, features = importantFeatures$important, nfactors = 10,
#          minimum_loadings = 0.25,
#          exclude_features = c("ParIn2_1", "Gende1_1", "AlcDe2_2", "AlcDe2_1", "PolA2n10",
#                               "ParIn2_5", "ParJo1_4", "ParIn2_5"), algorithm = "ml")

# importantFeatures <- hmda.feature.selection(wmshap, method = "mean", cutoff = 0.005)
# hmda.efa(df=raw, features = importantFeatures$important, nfactors = 11, algorithm = "minrank",
#          minimum_loadings = 0.3, dict = NULL,
#          exclude_features = c("ParIn2_1", "Gende1_1", "AlcDe2_2", "AlcDe2_1", "PolA2n10",
#                               "ParIn2_5", "ParJo1_4", "ParIn2_5", "Heigh1_1", "GmTy2_01"))
#
