#' @title
#' Multiple PGS Associations from different Phenotypes
#'
#' @description
#' `multiphenassoc()` takes a distribution of PGS and multiple Phenotypes and eventual confounders.
#' Returns a data frame showing the association results
#'
#' @param df a dataframe with individuals on each row, and at least the following
#' columns:
#'
#'  * one ID column,
#'  * one PGS column, with numerical continuous values following a normal distribution,
#'  * one Phenotype column, can be numeric (Continuous Phenotype), character, boolean or factors (Discrete Phenotype)
#' @param prs_col a character specifying the PGS column name
#' @param phenotype_col a character vector specifying the Phenotype column names
#' @param scale a boolean specifying if scaling of PGS should be done before testing
#' @param covar_col a character vector specifying the covariate column names (facultative)
#' @param verbose a boolean (TRUE by default) to write in the console/log messages.
#' @param log 	a connection, or a character string naming the file to print to.
#' If "" (by default), it prints to the standard output connection, the console unless redirected by sink.
#'
#' @return return a data frame showing the association of the PGS on the Phenotypes
#' with the following columns:
#'
#' * PGS: the name of the PGS
#' * Phenotype: the name of Phenotype
#' * Phenotype_type: either `'Continuous'`, `'Ordered Categorical'`, `'Categorical'` or `'Cases/Controls'`
#' * Stat_method: association function detects what is the phenotype type and what is the best way to analyse it, either `'Linear regression'`, `'Binary logistic regression'`, `'Ordinal logistic regression'` or `'Multinomial logistic regression'`
#' * Covar: list all the covariates used for this association
#' * N_cases: if Phenotype_type is Cases/Controls, gives the number of cases
#' * N_controls: if Phenotype_type is Cases/Controls, gives the number of controls
#' * N: the number of individuals/samples
#' * Effect: if Phenotype_type is Continuous, it represents the Beta coefficient of linear regression; Otherwise, it is the OR of logistic regression
#' * SE: standard error of the Beta coefficient (if Phenotype_type is Continuous)
#' * lower_CI: lower confidence interval of the related Effect (Beta or OR)
#' * upper_CI: upper confidence interval of the related Effect (Beta or OR)
#' * P_value: associated P-value
#'
#' @importFrom stats na.omit
#' @export
multiphenassoc <- function(df = NULL, prs_col = "SCORESUM", phenotype_col = "Phenotype",
                           scale = TRUE, covar_col = NA, verbose= TRUE, log = "") {
  ## Checking inputs
  n_pheno <- length(phenotype_col)
  if (verbose) cat("\n\n------\nMultiple  phenotypes associations (", n_pheno, ") testing:", file = log, append = FALSE)
  # checking inputs (done in assoc that calls df_checker)
  if (n_pheno <= 1) {
    warning("No multiple Phenotypes given, preferably use assoc() function")
  } else if (!Reduce(`|`, class(log) %in% c("character","url","connection"))) {
    stop("Please provide a connection, or a character string naming the file to print to for 'log'")
  }

  ## Creating progress bar
  if (verbose) cat("\n", file = log, append = TRUE)
  progress <- txtProgressBar(min = 0, max = n_pheno, initial = 0, style = 3)

  ## Creating the score table
  scores_table <- data.frame(matrix(nrow = 0, ncol = 11))
  names(scores_table) <- c(
    "PGS", "Phenotype", "Covar", "N_cases", "N_controls",
    "N", "OR", "SE", "lower_CI", "upper_CI", "P_value"
  )


  ## For loop of assoc function
  for (i in 1:n_pheno) {
    scores_table <- rbind(scores_table, assoc(
      df = df, prs_col = prs_col,
      phenotype_col = phenotype_col[i],
      scale = scale, covar_col = covar_col,
      log = log, verbose = verbose
    ))

    if (verbose) cat("\n", file = log, append = TRUE)
    setTxtProgressBar(progress, i)
    if (verbose) cat("\n", file = log, append = TRUE)
  }

  # returning the result
  return(scores_table)
}
