#' @title
#' Multiple PGS Associations from a Data Frame
#'
#' @description
#' `multiassoc()` takes a data frame with distribution(s) of PGS and Phenotype(s),
#' and a table of associations to make from this data frame.
#'
#' Returns a data frame showing the association results
#'
#' @param df a dataframe with individuals on each row, and at least the following
#' columns:
#'
#'  * one ID column,
#'  * one PGS column, with numerical continuous values following a normal distribution,
#'  * one Phenotype column, can be numeric (Continuous Phenotype), character, boolean or factors (Discrete Phenotype)
#' @param assoc_table a dataframe or matrix specifying the associations to
#' make from df, with 2 columns: PGS and Phenotype (in this order)
#' @param scale a boolean specifying if scaling of PGS should be done before testing
#' @param covar_col a character vector specifying the covariate column names (facultative)
#' @param verbose a boolean (TRUE by default) to write in the console/log messages.
#' @param log 	a connection, or a character string naming the file to print to.
#' If "" (by default), it prints to the standard output connection, the console unless redirected by sink.
#' If parallel = TRUE, the log will be incomplete
#' @param parallel a boolean, if TRUE, `multiassoc()` parallelise the association analysis to run it faster (no log available with this option, does not work with Windows machine)
#' If FALSE (default), the association analysis will not be parallelised (useful for debugging process)
#' @param num_cores an integer, if parallel = TRUE (default), `multiassoc()` parallelise the association analysis to run it faster using num_cores as the number of cores.
#' If nothing is provided, it detects the number of cores of the machine and use num_cores-1
#'
#' @return return a data frame showing the association of the PGS(s) on the Phenotype(s)
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
#' * Effect: if Phenotype_type is Continuous, it represents the Beta coefficient of linear regression, OR of logistic regression otherwise
#' * SE: standard error of the related Effect (Beta or OR)
#' * lower_CI: lower confidence interval of the related Effect (Beta or OR)
#' * upper_CI: upper confidence interval of the related Effect (Beta or OR)
#' * P_value: associated P-value
#'
#' @examples
#' assoc_table <- expand.grid(
#'   c("t2d_PGS", "ldl_PGS"),
#'   c("ethnicity","brc","t2d","log_ldl","sbp_cat")
#' )
#' results <- multiassoc(
#'   df = comorbidData,
#'   assoc_table = assoc_table,
#'   covar_col = c("age", "sex", "gen_array"),
#'   parallel = FALSE,
#'   verbose = FALSE
#' )
#' print(results)
#'
#' @importFrom stats na.omit
#' @import parallel
#' @export
multiassoc <- function(df = NULL, assoc_table = NULL, scale = TRUE,
                       covar_col = NA, verbose = TRUE,
                       log = "", parallel = FALSE, num_cores = NA) {
  ## Checking inputs (done in assoc that calls df_checker)
  if (is.null(assoc_table)) {
    stop("Please provide a data frame or a matrix for 'assoc_table' parameter")
  } else if (!Reduce(`|`, class(assoc_table) %in% c("data.frame", "matrix", "array"))) {
    stop("Please provide for 'assoc_table' a data frame or a matrix with 2 columns representing PGS and Phenotype (in this order)")
  } else if (ncol(assoc_table) != 2) {
    stop("Please provide for 'assoc_table' a data frame or a matrix with 2 columns representing PGS and Phenotype (in this order)")
  } else if (!Reduce(`|`, class(log) %in% c("character","url","connection"))) {
    stop("Please provide a connection, or a character string naming the file to print to for 'log'")
  } else if (is.null(parallel)) {
    stop("Please provide a boolean for 'parallel' parameter")
  } else if (!(parallel == TRUE | parallel == FALSE)) {
    stop("Please provide a boolean for 'parallel' parameter")
  } else {
    n_assoc <- nrow(assoc_table)
    if (n_assoc <= 1) {
      warning("No multiple associations given, preferably use assoc() function")
    }
  }
  if (verbose) cat("\n\n------\nMultiple associations (", n_assoc, ") testing:\n", file = log, append = FALSE)

  ## Creating the score table
  scores_table <- data.frame(matrix(nrow = 0, ncol = 11))
  names(scores_table) <- c(
    "PGS", "Phenotype", "Covar", "N_cases", "N_controls",
    "N", "OR", "SE", "lower_CI", "upper_CI", "P_value"
  )

  ## QC assoc table
  names(assoc_table) <- c("PGS", "Phenotype")


  ## Parallele version of the for loop of assoc function
  if (parallel == TRUE) {
    if (is.na(num_cores) | (!Reduce(`|`, class(num_cores) %in% c("numeric")))) {
      num_cores <- detectCores()-1
    } else if (num_cores > detectCores()) {
      warning("num_cores is more than the detected cores")
    }

    if (num_cores > 1 & Sys.info()["sysname"] != "Windows") {

      if (verbose) cat("Using parallelisation, no log available with this option. \nNo of cores:", num_cores, file = log, append = FALSE)

      scores_list <- mclapply(1:n_assoc, function(i) {
        return(assoc(
          df = df, prs_col = as.character(assoc_table[i, 1]),
          phenotype_col = as.character(assoc_table[i, 2]),
          scale = scale, covar_col = covar_col,
          log = "", verbose = FALSE
        ))
      }, mc.cores = num_cores)
      scores_table <- do.call(rbind, scores_list)

    } else {

      if (verbose) if (verbose) cat("No parallelisation, this operation may be slower\n", file = log, append = FALSE)
      ## Creating progress bar
      progress <- txtProgressBar(min = 0, max = n_assoc, initial = 0, style = 3)

      for (i in 1:n_assoc) {
        scores_table <- rbind(scores_table, assoc(
          df = df, prs_col = as.character(assoc_table[i, 1]),
          phenotype_col = as.character(assoc_table[i, 2]),
          scale = scale, covar_col = covar_col,
          log = log, verbose = verbose
        ))

        if (verbose) cat("\n", file = log, append = TRUE)
        setTxtProgressBar(progress, i)
        if (verbose) cat("\n", file = log, append = TRUE)
      }

    }

  } else {

    if (verbose) cat("No parallelisation, this operation may be slower\n", file = log, append = FALSE)
    ## Creating progress bar
    progress <- txtProgressBar(min = 0, max = n_assoc, initial = 0, style = 3)

    for (i in 1:n_assoc) {
      scores_table <- rbind(scores_table, assoc(
        df = df, prs_col = as.character(assoc_table[i, 1]),
        phenotype_col = as.character(assoc_table[i, 2]),
        scale = scale, covar_col = covar_col,
        log = log, verbose = verbose
      ))

      if (verbose) cat("\n", file = log, append = TRUE)
      setTxtProgressBar(progress, i)
      if (verbose) cat("\n", file = log, append = TRUE)
    }

  }

  # returning the result
  return(scores_table)
}
