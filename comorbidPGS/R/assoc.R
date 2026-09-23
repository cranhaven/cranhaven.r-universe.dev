#' @title
#' Association of a PGS distribution with a Phenotype
#'
#' @description
#' `assoc()` takes a distribution of PGS, a Phenotype and eventual Confounders.
#' Returns a data frame showing the association of PGS on the Phenotype
#'
#' @param df a dataframe with individuals on each row, and at least the following
#' columns:
#'
#'  * one ID column,
#'  * one PGS column, with numerical continuous values following a normal distribution,
#'  * one Phenotype column, can be numeric (Continuous Phenotype), character, boolean or factors (Discrete Phenotype)
#' @param prs_col a character specifying the PGS column name
#' @param phenotype_col a character specifying the Phenotype column name
#' @param scale a boolean specifying if scaling of PGS should be done before testing
#' @param covar_col a character vector specifying the covariate column names (facultative)
#' @param verbose a boolean (TRUE by default) to write in the console/log messages.
#' @param log a connection, or a character string naming the file to print to.
#' If "" (by default), it prints to the standard output connection, the console unless redirected by sink.
#'
#' @return return a data frame showing the association of the PGS on the Phenotype
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
#' @examples
#' results <- assoc(
#'   df = comorbidData,
#'   prs_col = "ldl_PGS",
#'   phenotype_col = "log_ldl",
#'   scale = TRUE,
#'   covar_col = c("age", "sex", "gen_array")
#' )
#' print(results)
#'
#' @importFrom stats na.omit glm binomial lm coef confint median pnorm
#' @importFrom MASS polr
#' @importFrom nnet multinom
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
assoc <- function(df = NULL, prs_col = "SCORESUM", phenotype_col = "Phenotype",
                  scale = TRUE, covar_col = NA, verbose = TRUE, log = "") {
  ## Checking inputs
  col_names <- df_checker(df, prs_col, phenotype_col, scale, covar_col)
  prs_col <- col_names$prs_col
  phenotype_col <- col_names$phenotype_col
  if (!is.logical(verbose)) {
    stop("Please provide a logical for 'verbose' (TRUE by default)")
  } else if (!is.logical(scale)) {
    stop("Please provide a logical for 'scale' (TRUE by default)")
  } else if (!Reduce(`|`, class(log) %in% c("character","url","connection"))) {
    stop("Please provide a connection, or a character string naming the file to print to for 'log'")
  }

  if (verbose) cat("\n\n---\nAssociation testing:", file = log, append = TRUE)

  # communicate the columns selected
  if (verbose) cat("\n  PGS: ", prs_col, file = log, append = TRUE)
  if (verbose) cat("\n  Phenotype: ", phenotype_col, file = log, append = TRUE)
  if (verbose) cat("\n  Covariate: ", covar_col, file = log, append = TRUE)


  ## QCing df
  if (is.na(covar_col[1])) {
    df <- df[, c(prs_col, phenotype_col)] # cropping the dataset to only 2 columns
  } else {
    df <- df[, c(prs_col, phenotype_col, covar_col)] # cropping the dataset to 2+(covar length) columns
  }
  df <- na.omit(df) # excluding rows with NAs
  if (scale) {
    df[, prs_col] <- scale(df[, prs_col]) # scaling if scale = TRUE
  }
  if (nrow(df) < 2) {
    stop("After NA removal, not enough samples/individuals to test")
  }


  ## Doing regression
  # check Phenotype continuous or discrete aspect
  phenotype_type <- phenotype_type(df = df, phenotype_col = phenotype_col)
  if (verbose) cat("\n  Phenotype type: ", phenotype_type, file = log, append = TRUE)

  # create the regression formula based on phenotype, PGS and covariate(s)
  if (length(covar_col) > 1 & !is.na(covar_col[1])) {
    # create the regression formula
    regress_formula <- paste0("`",phenotype_col,"` ~ `",prs_col,"`")
    for (covar in covar_col) {
      regress_formula <- paste0(regress_formula, " + `",covar,"`")
    }
  } else {
    # create the regression formula
    regress_formula <- paste0("`",phenotype_col,"` ~ `",prs_col,"`")
  }
  if (verbose) cat("\n   ", regress_formula, file = log, append = TRUE)

  # doing regression according to phenotype type
  if (phenotype_type == "Cases/Controls") {
    stat_method <- 'Binary logistic regression'
    regress <- glm(regress_formula, family = stats::binomial(link = "logit"), data = df)
  } else if (phenotype_type == "Ordered Categorical") {
    stat_method <- 'Ordinal logistic regression'
    regress <- polr(regress_formula, method = c("logistic"), data = df, Hess = TRUE)
  } else if (phenotype_type == "Categorical") {
    stat_method <- 'Multinomial logistic regression'
    regress <- multinom(regress_formula, data = df, Hess = TRUE)
  } else if (phenotype_type == "Continuous") {
    stat_method <- 'Linear regression'
    regress <- lm(regress_formula, data = df)
  }
  if (verbose) cat("\n   Using a ",stat_method, file = log, append = TRUE)


  ## Wrapping up the results in a table
  #collecting common sample size
  if (phenotype_type == "Cases/Controls") {
    cases <- sum(as.logical(df[, phenotype_col]) == TRUE)
    controls <- sum(as.logical(df[, phenotype_col]) == FALSE)
    if (verbose) cat("\n  Cases: ", cases, file = log, append = TRUE)
    if (verbose) cat("\n  Controls: ", controls, file = log, append = TRUE)
  } else if (phenotype_type == "Categorical") {
    phen_ref <- levels(df[, phenotype_col])[1]
    cases <- c()
    controls <- c()
    phenotype_name <- c()
    sample_size <- c()
    for (value in levels(df[, phenotype_col])[-1]) {
      tmp_cases <- sum(df[,phenotype_col] == value)
      tmp_controls <- sum(df[,phenotype_col] == phen_ref)
      cases <- c(cases, tmp_cases)
      controls <- c(controls, tmp_controls)
      phenotype_name <- c(phenotype_name, paste(phenotype_col, phen_ref, "~", value))
      sample_size <- c(sample_size, tmp_cases+tmp_controls)
    }
  } else {
    cases <- NA
    controls <- NA
  }
  if (!exists("phenotype_name")) {
    phenotype_name <- phenotype_col
  }
  if (!exists("sample_size")) {
    sample_size <- nrow(df)
  }
  if (verbose) cat("\n  Sample Size: ", sample_size, file = log, append = TRUE)

  # linear regression or binary log regression or ordinal log regression
  if (phenotype_type == "Categorical") {
    beta <- coef(regress)[,2]
    beta_se <- summary(regress)$standard.error[,2]
    z <- beta/beta_se

    beta_or <- exp(beta)
    se <- NA
    p_val <- (1-stats::pnorm(abs(z), 0, 1))*2

    lower_ci <- exp(beta-1.96*beta_se)
    upper_ci <- exp(beta+1.96*beta_se)
  } else if (phenotype_type == "Ordered Categorical") {
    ctable <- coef(summary(regress))

    # adding pval
    ptemp <- (1-stats::pnorm(abs(ctable[, "t value"]), 0, 1))*2
    ctable <- cbind(ctable, "p value" = ptemp)

    beta <- ctable[1, "Value"]
    beta_se <- ctable[1, "Std. Error"]

    beta_or <- exp(beta)
    se <- NA
    p_val <- ctable[1, "p value"]

    lower_ci <- exp(beta-1.96*beta_se)
    upper_ci <- exp(beta+1.96*beta_se)

  } else { # for binary logistic and linear regression, same format
    ctable <- coef(summary(regress))

    beta <- ctable[2, 1]
    beta_se <- ctable[2, 2]

    beta_or <- ifelse((phenotype_type == "Continuous"), beta, exp(beta))
    se <- ifelse((phenotype_type == "Continuous"), beta_se, NA)
    p_val <- ctable[2, 4]

    lower_ci <- ifelse((phenotype_type == "Continuous"),  beta-1.96*beta_se, exp(beta-1.96*beta_se))
    upper_ci <- ifelse((phenotype_type == "Continuous"),  beta+1.96*beta_se, exp(beta+1.96*beta_se))
  }

  if (phenotype_type == "Continuous") {
    if (verbose) cat("\n   Beta ( SE ): ", beta_or, " (", se, ")", file = log, append = TRUE)
  } else {
    if (verbose) cat("\n   OR [ 95% CI ]: ", beta_or, " [", lower_ci, "-", upper_ci, "]", file = log, append = TRUE)
  }
  if (verbose) cat("\n   P-value: ", p_val, "\n", file = log, append = TRUE)

  # creating the score_table
  score_table <- data.frame(
    "PGS" = prs_col, "Phenotype" = phenotype_name,
    "Phenotype_type" = phenotype_type,
    "Statistical_method" = stat_method,
    "Covar" = paste(covar_col, collapse = "+"),
    "N_cases" = cases, "N_controls" = controls,
    "N" = sample_size, "Effect" = beta_or, "SE" = se,
    "lower_CI" = lower_ci, "upper_CI" = upper_ci,
    "P_value" = p_val
  )

  # returning the result
  return(score_table)
}
