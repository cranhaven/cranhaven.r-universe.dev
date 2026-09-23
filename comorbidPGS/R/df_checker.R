#' @title
#' Check the format of the data frame
#'
#' @param df a dataframe
#'
#' @return proper prs_col and phenotype_col
#'
#' @importFrom stats shapiro.test
#'
#' @noRd
df_checker <- function(df = NULL, prs_col = NA, phenotype_col = NA, scale = NA,
                       covar_col = NA) {
  ## Checking df object
  if (is.null(df)) {
    stop("Please provide for 'df' a data frame (with at least 3 columns:
    ID, PGS and a continuous or discrete Phenotype)")
  } else if (!Reduce(`|`, class(df) %in% c("data.frame"))) {
    stop("Please provide for 'df' a data frame (with at least 3 columns:
    ID, PGS and a continuous or discrete Phenotype)")
  } else if (ncol(df) < 3) {
    stop("Please provide for 'df' a data frame with at least 3 columns:
    ID, PGS and a continuous or discrete Phenotype")
  } else if (!is.logical(scale)) {
    stop("Please provide a logical for 'scale' (TRUE by default)")
  } else if (nrow(unique(df)) != nrow(df)) {
    warning("Duplicate(s) found in df! Removing them")
    df <- unique(df)
  }



  ## Checking what is in the data frame df
  # if no SCORESUM column found, assume 2nd column is PGS
  if (is.null(prs_col)) {
    stop("Missing prs_col")
  } else if (is.na(prs_col)) {
    stop("Missing prs_col")
  } else if (!prs_col %in% names(df)) {
    stop("prs_col not found in df")
  } else if (!Reduce(`|`, class(df[, prs_col]) %in% c("numeric", "integer", "double"))) {
    stop("Please provide numeric values in the PGS column")
  }
  # if no Phenotype column found, assume 3rd column is PGS
  if (is.null(phenotype_col)) {
    stop("Missing phenotype_col")
  } else if (is.na(phenotype_col)) {
    stop("Missing phenotype_col")
  } else if (!phenotype_col %in% names(df)) {
    stop("phenotype_col not found in df")
  }
  # if no Covariate column found, assume covar_col is NA
  if (is.null(covar_col[1]) | is.na(covar_col[1])) {
    covar_col <- NA
  } else {
    for (covar in covar_col) {
      if (!covar %in% names(df)) {
        stop(paste("Wrong covar_col provided,", covar, "does not exist"))
      }
    }
  }

  return(list("prs_col" = prs_col, "phenotype_col" = phenotype_col))
}

df_mr_checker <- function(df = NULL, prs_col = NA, exposure_col = NA,
                          outcome_col = NA, scale = NA) {
  ## Checking df object
  if (is.null(df)) {
    stop("Please provide for 'df' a data frame (with at least 4 columns:
    ID, PGS and two continuous or discrete Phenotype)")
  } else if (!Reduce(`|`, class(df) %in% c("data.frame"))) {
    stop("Please provide for 'df' a data frame (with at least 4 columns:
    ID, PGS and two continuous or discrete Phenotype)")
  } else if (ncol(df) < 4) {
    stop("Please provide for 'df' a data frame with at least 4 columns:
    ID, PGS and two continuous or discrete Phenotype")
  } else if (!is.logical(scale)) {
    stop("Please provide a logical for 'scale' (TRUE by default)")
  } else if (nrow(unique(df)) != nrow(df)) {
    warning("Duplicate(s) found in df! Removing them")
    df <- unique(df)
  }

  ## Checking what is in the data frame df
  # checking PGS column
  if (is.null(prs_col)) {
    stop("Missing prs_col")
  } else if (is.na(prs_col)) {
    stop("Missing prs_col")
  } else if (!prs_col %in% names(df)) {
    stop("prs_col not found in df")
  } else if (!Reduce(`|`, class(df[, prs_col]) %in% c("numeric", "integer", "double"))) {
    stop("Please provide numeric values in the PGS column")
  }
  # checking Exposure column
  if (is.null(exposure_col)) {
    stop("Missing exposure_col")
  } else if (is.na(exposure_col)) {
    stop("Missing exposure_col")
  } else if (!exposure_col %in% names(df)) {
    stop("exposure_col not found in df")
  }
  # checking Outcome column
  if (is.null(outcome_col)) {
    stop("Missing outcome_col")
  } else if (is.na(outcome_col)) {
    stop("Missing outcome_col")
  } else if (!outcome_col %in% names(df)) {
    stop("outcome_col not found in df")
  }

  return(list("prs_col" = prs_col,
              "exposure_col" = exposure_col,
              "outcome_col" = outcome_col))
}


normal_distribution_checker <- function(x) {
  #check first if the variable follow normal distribution
  #if we have n > 5000, we need to run shapiro multiple times
  n_pheno <- sum(!is.na(x))
  normal <- TRUE
  if (n_pheno>5000) {
    st <- 0
    for (i in 1:50) {
      t <- stats::shapiro.test(x[sample(1:n_pheno, 5000, replace = TRUE)])$p.value
      st <- st+(t>0.05)
    }
    if (st == 0) {
      normal <- FALSE
    }
  } else {
    t <- stats::shapiro.test(x[sample(1:n_pheno, 5000, replace = TRUE)])$p.value
    if (t <= 0.05) {
      normal <- FALSE
    }
  }

  return(normal)
}


phenotype_type <- function(df = NULL, phenotype_col = "Phenotype") {
  values <- unique(df[, phenotype_col])
  phenotype_type <- "unknown"
  if (length(values) < 2) {
    stop(paste("Phenotype column", phenotype_col, "have less than 2 valuess"))
  } else if (Reduce(`|`, class(df[, phenotype_col]) == "logical") | length(values) == 2) {
    phenotype_type <- "Cases/Controls"
  } else if (Reduce(`|`, class(df[, phenotype_col]) %in% c("character", "factor")) & length(values) > 2) {
    #it is a categorical phenotype, now checking if it is ordered
    if (is.ordered(df[, phenotype_col])) {
      phenotype_type <- "Ordered Categorical"
    } else {
      phenotype_type <- "Categorical"
    }
    df[, phenotype_col] <- as.factor(df[, phenotype_col])
  } else if (Reduce(`|`, class(df[, phenotype_col]) %in% c("numeric", "integer", "double"))) {
    phenotype_type <- "Continuous"
    df[, phenotype_col] <- as.numeric(df[, phenotype_col])
    #remove numbering values for missing data: "-99"
    df[which(df[, phenotype_col] == -9), phenotype_col] <- NA
    #check first if the variable follow normal distribution
    #if we have n > 5000, we need to run shapiro multiple times
    if (!normal_distribution_checker(df[, phenotype_col])) {
      warning(paste("Phenotype column", phenotype_col, "is continuous and not normal, please normalise prior association"))
    }
  } else if (phenotype_type == "unknown") {
    stop(paste("Unable to identify phenotype type for", phenotype_col, "Please provide a Continuous, Categorical or Cases/Controls Phenotype"))
  }

  return(phenotype_type)
}

