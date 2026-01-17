#' Create a Dataset for Choice Experiment Analysis
#'
#' This function takes a design matrix and generates a dataset for use in choice experiments.
#' It handles blocks, replicates the design for the number of respondents, and assigns respondent IDs.
#'
#' @param design A data frame containing the design matrix for the choice experiment.
#'   It should include at least the columns `Choice.situation` and optionally `Block`.
#'
#' @param respondents The number of respondents to generate data for.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Checks if the `Block` column exists in the input design. If absent, it creates a single block.
#'   \item Calculates the number of choice sets and blocks, and determines the number of sets per block.
#'   \item Replicates the design to account for the specified number of respondents per block.
#'   \item Assigns respondent IDs based on the number of respondents and blocks.
#' }
#'
#' @return A data frame containing the augmented design matrix with additional columns:
#' \describe{
#'   \item{ID}{A unique identifier for each respondent.}
#'   \item{Choice.situation}{The original choice situations, replicated for respondents.}
#'   \item{Other columns}{All original columns in the input `design` are retained.}
#' }
#'
#' @importFrom dplyr arrange slice row_number mutate relocate
#' @export
#'
#' @examples
#' # Example usage:
#' design <- data.frame(
#'   Choice.situation = rep(1:12),
#'   Attribute1 = rnorm(12),
#'   Attribute2 = sample(1:3, 12, replace = TRUE)
#' )
#' result <- createDataset(design, 10)
createDataset <- function(design, respondents) {
  if (!("Block" %in% colnames(design))) design$Block <- 1 # If no Blocks exist, create a variable Blocks to indicate it is only one block

  nsets <- nrow(design)
  nblocks <- max(design$Block)
  setpp <- nsets / nblocks # Choice Sets per respondent

  replications <- respondents / nblocks

  # assign("nsets", nsets, envir = parent.frame())
  # assign("nblocks", nblocks, envir = parent.frame())
  # assign("setpp", setpp, envir = parent.frame())
  # assign("replications", replications, envir = parent.frame())

  ## if replications is non int, assign unevenly

  datadet <- design %>%
    dplyr::arrange(Block, Choice.situation) %>%
    dplyr::slice(rep(dplyr::row_number(), replications)) %>% ## replicate design according to number of replications

    dplyr::mutate(ID = rep(1:respondents, each = setpp)) %>% # create Respondent ID.
    dplyr::relocate(ID, `Choice.situation`) %>%
    as.data.frame()
}
