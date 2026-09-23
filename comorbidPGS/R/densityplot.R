#' @title
#' Density Plot from a PGS Association
#'
#' @description
#' `densityplot()` takes a distribution of PGS, a Phenotype and eventual Confounders.
#' Returns a plot with density of PGS in x by Categories of the Phenotype
#'
#' @param df a dataframe with individuals on each row, and at least the following
#' columns:
#'
#'  * one ID column,
#'  * one PGS column, with numerical continuous values following a normal distribution,
#'  * one Phenotype column, can be numeric (Continuous Phenotype), character, boolean or factors (Discrete Phenotype)
#' @param prs_col a character specifying the PGS column name
#' @param phenotype_col a character specifying the Phenotype column name
#' @param scale a boolean specifying if scaling of PGS should be done before plotting
#' @param threshold a facultative numeric specifying for Continuous Phenotype
#' the Threshold to consider individuals as Cases/Controls as following:
#'
#'  * Phenotype > Threshold = Case
#'  * Phenotype < Threshold = Control
#'
#' @return return a ggplot object (ggplot2)
#' @importFrom stats na.omit
#' @import ggplot2
#' @export
densityplot <- function(df = NULL, prs_col = "SCORESUM", phenotype_col =
                          "Phenotype", scale = TRUE, threshold = NA) {
  ## Checking inputs
  col_names <- df_checker(df, prs_col, phenotype_col, scale)
  prs_col <- col_names$prs_col
  phenotype_col <- col_names$phenotype_col

  ## Taking only subset of df
  df <- df[, c(prs_col, phenotype_col)]
  names(df) <- c("PGS", "Phenotype")
  df <- na.omit(df)
  if (scale) {
    df[, "PGS"] <- scale(df[, "PGS"]) # scaling if scale = TRUE
  }

  ## Making plot based on the category of Phenotype
  phenotype_type <- phenotype_type(df = df, phenotype_col = "Phenotype")
  if (phenotype_type %in% c("Cases/Controls", "Categorical", "Ordered Categorical")) {
    p <- ggplot(df, aes(.data$PGS, fill = as.factor(.data$Phenotype))) +
      geom_density(alpha = 0.4) +
      labs(x = prs_col, y = "Density", fill = phenotype_col) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 11),
        axis.text.x.bottom = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y.left = element_text(size = 11)
      )
  } else if (!is.na(threshold) & Reduce(`|`, class(threshold) %in% c("integer", "numeric", "double"))) {
    df$Categorical_Pheno <- df$Phenotype > threshold
    p <- ggplot(df, aes(.data$PGS, fill = as.factor(.data$Categorical_Pheno))) +
      geom_density(alpha = 0.4) +
      theme_minimal() +
      labs(x = prs_col, y = "Density", fill = phenotype_col) +
      theme(
        axis.title.x = element_text(size = 11),
        axis.text.x.bottom = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y.left = element_text(size = 11)
      )
  } else {
    warning("Phenotype is continuous and 'threshold' is not a number, ignoring the parameter")

    p <- ggplot(df, aes(.data$PGS)) +
      geom_density(alpha = 0.4) +
      theme_minimal() +
      labs(x = prs_col, y = "Density") +
      theme(
        axis.title.x = element_text(size = 11),
        axis.text.x.bottom = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y.left = element_text(size = 11)
      )
  }

  return(p)
}
