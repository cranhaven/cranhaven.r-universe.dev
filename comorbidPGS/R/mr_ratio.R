#' @title
#' Mendelian Randomization ratio method with external PGS
#'
#' @description
#' `mr_ratio()` takes a distribution of PGS, an Exposure (Phenotype), an Outcome (Phenotype).
#' Returns a data frame showing the Mendelian Randomization ratio methods using PGS
#'
#' @param df a dataframe with individuals on each row, and at least the following
#' columns:
#'
#'  * one ID column,
#'  * one PGS column, with numerical continuous values following a normal distribution,
#'  * two Phenotype columns (for Exposure and Outcome), can be numeric (Continuous Phenotype), character, boolean or factors (Discrete Phenotype)
#' @param prs_col a character specifying the PGS column name
#' @param exposure_col a character specifying the Exposure (Phenotype) column name
#' @param outcome_col a character specifying the Outcome (Phenotype) column name
#' @param scale a boolean specifying if scaling of PGS should be done before testing
#' @param verbose a boolean (TRUE by default) to write in the console/log messages.
#' @param log a connection, or a character string naming the file to print to.
#' If "" (by default), it prints to the standard output connection, the console unless redirected by sink.
#'
#' @return return a data frame with the Mendelian Randomization association
#' result using the ratio method with the following columns:
#'
#' * PGS: the name of the PGS used
#' * Exposure: the name of Phenotype used as Exposure
#' * Outcome: the name of Phenotype used as Outcome
#' * Method: the MR method used (here Ratio)
#' * N_cases: if Phenotype_type is Cases/Controls, the number of cases
#' * N_controls: if Phenotype_type is Cases/Controls, the number of controls
#' * N: the number of individuals/samples
#' * MR_estimate: the MR estimate (beta) using the ratio method
#' * SE: the associated standard error (second order)
#' * F_stat: the F-statistic of the Exposure ~ PGS association
#'
#' @examples
#' result <- mr_ratio(
#'   df = comorbidData,
#'   prs_col = "ldl_PGS",
#'   exposure_col = "log_ldl",
#'   outcome_col = "bmi",
#'   scale = TRUE
#' )
#' print(result)
#'
#' @importFrom stats na.omit glm binomial lm coef
#' @export
mr_ratio <- function(df = NULL, prs_col = "SCORESUM", exposure_col = NA,
                     outcome_col = NA, scale = TRUE, verbose = TRUE, log = "") {
  ## Checking inputs
  col_names <- df_mr_checker(df, prs_col, exposure_col, outcome_col, scale)
  prs_col <- col_names$prs_col
  exposure_col <- col_names$exposure_col
  outcome_col <- col_names$outcome_col
  if (!is.logical(verbose)) {
    stop("Please provide a logical for 'verbose' (TRUE by default)")
  } else if (!is.logical(scale)) {
    stop("Please provide a logical for 'scale' (TRUE by default)")
  } else if (!Reduce(`|`, class(log) %in% c("character","url","connection"))) {
    stop("Please provide a connection, or a character string naming the file to print to for 'log'")
  }

  if (verbose) cat("\n\n---\nMendelian Randomization Ratio:", file = log, append = TRUE)

  # communicate the columns selected
  if (verbose) cat("\n  PGS --> Exposure -> Outcome: \t", prs_col, "-->", exposure_col, "->", outcome_col, file = log, append = TRUE)


  ## QCing df
  df <- df[, c(prs_col, exposure_col, outcome_col)] # cropping the dataset to 3 columns
  df <- na.omit(df) # excluding rows with NAs
  if (scale) {
    df[, prs_col] <- scale(df[, prs_col]) # scaling if scale = TRUE
  }
  if (nrow(df) < 2) {
    stop("After NA removal, not enough samples/individuals to test")
  }


  ## Doing regression
  # check Exposure continuous or discrete aspect
  exposure_type <- phenotype_type(df = df, phenotype_col = exposure_col)
  if (exposure_type == "Cases/Controls") {
    warning("Binary exposure detected, use caution when interpreting the results (more info: https://mr-dictionary.mrcieu.ac.uk/term/binary-exposure/)")
  }
  # check Outcome continuous or discrete aspect
  outcome_type <- phenotype_type(df = df, phenotype_col = outcome_col)
  if (!(exposure_type %in% c("Cases/Controls", "Continuous"))) {
    stop("Cannot use a Categorical exposure for MR ratio. Please use Cases/Controls or Continuous exposure")
  } else if (!(outcome_type %in% c("Cases/Controls", "Continuous"))) {
    stop("Cannot use a Categorical outcome for MR ratio. Please use Cases/Controls or Continuous outcome")
  }
  if (verbose) cat("\n  Exposure phenotype type: ", exposure_type, "\n  Outcome phenotype type: ", outcome_type, file = log, append = TRUE)

  # create the regression formula based on phenotypes, PGS
  if (outcome_type == "Cases/Controls") {
    outcome_control = levels(factor(df[, outcome_col]))[1] #if the outcome is binary, regress only controls
    df_control <- df[!as.logical(df[, outcome_col]), ]
    # exposure_formula <- paste0("`", exposure_col, "`[`", outcome_col, "`==", outcome_control, "] ~ `", prs_col, "`")
  } else {
    df_control <- df
  }
  exposure_formula <- paste0("`",exposure_col,"` ~ `",prs_col,"`")
  outcome_formula <- paste0("`",outcome_col,"` ~ `",prs_col,"`")

  # doing Exposure regression according to phenotype type
  if (exposure_type == "Cases/Controls") {
    gx <- glm(exposure_formula, family = stats::binomial(link = "logit"), data = df_control)
    fstat <- NA
  } else if (exposure_type == "Continuous") {
    gx <- lm(exposure_formula, data = df_control)
    fstat <- summary(gx)$fstatistic[1]
  }

  # doing Outcome regression according to phenotype type
  sample_size <- nrow(df)
  if (outcome_type == "Cases/Controls") {
    gy <- glm(outcome_formula, family = stats::binomial(link = "logit"), data = df)
    controls <- sum(df[, outcome_col] == outcome_control)
    cases <- sample_size - controls
  } else if (outcome_type == "Continuous") {
    gy <- lm(outcome_formula, data = df)
    cases <- NA
    controls <- NA
  }

  ## Wrapping up the results in a table
  bx <- coef(gx)[2]
  bx_se <- summary(gx)$coefficients[2,2]
  by <- coef(gy)[2]
  by_se <- summary(gy)$coefficients[2,2]

  beta <- by/bx
  se <- sqrt(by_se^2/bx^2 + by^2*bx_se^2/bx^4) #using second order to be more precise

  # creating the mr_table
  mr_table <- data.frame(
    "PGS" = prs_col,
    "Exposure" = exposure_col,
    "Outcome" = outcome_col,
    "Method" = "Ratio",
    "N_cases" = cases, "N_controls" = controls,
    "N" = sample_size, "MR_estimate" = beta, "SE" = se,
    "F_stat" = fstat
  )

  # returning the result
  return(mr_table)
}
