#' @title
#' Centiles Plot from a PGS Association
#'
#' @description
#' `centileplot()` takes a distribution of PGS, a Phenotype and eventual Confounders.
#' Returns a plot (ggplot2 object) with centiles (or deciles if not enough individuals)
#' of PGS in x and Prevalence/Median/Mean of the Phenotype in y
#'
#' @param df a dataframe with individuals on each row, and at least the following
#' columns:
#'
#'  * one ID column,
#'  * one PGS column, with numerical continuous values following a normal distribution,
#'  * one Phenotype column, can be numeric (Continuous Phenotype), character, boolean or factors (Discrete Phenotype)
#' @param prs_col a character specifying the PGS column name
#' @param phenotype_col a character specifying the Phenotype column name
#' @param decile a boolean specifying if centiles or deciles should be used
#' @param continuous_metric a facultative character specifying what metric to
#' use for continuous Phenotype, only three options: `NA`, `"median"` or `"mean"`
#'
#' @return return a figure of results in the format ggplot2 object
#' @importFrom stats na.omit
#' @import ggplot2
#' @export
centileplot <- function(df = NULL, prs_col = "SCORESUM",
                        phenotype_col = "Phenotype",
                        decile = FALSE, continuous_metric = NA) {
  ## Checking inputs
  col_names <- df_checker(df, prs_col, phenotype_col, scale = FALSE)
  prs_col <- col_names$prs_col
  phenotype_col <- col_names$phenotype_col
  if (!continuous_metric %in% c(NA, "median", "mean")) {
    stop("'continuous_metric' parameter only accepts three values: NA, 'median' or 'mean'")
  }

  ## Taking only subset of df
  df <- df[, c(prs_col, phenotype_col)]
  names(df) <- c("PGS", "Phenotype")
  df <- na.omit(df)
  if (nrow(df) < 10000) {
    warning("The dataset has less than 10,000 individuals, centiles plot may not look good! Use the argument decile = TRUE to adapt to small datasets")
  }

  ## Making centiles plot based on the category of Phenotype
  phenotype_type <- phenotype_type(df = df, phenotype_col = "Phenotype")

  df$centile <- with(df, cut(PGS, breaks = quantile(PGS, probs = seq(0, 1, by = 0.01), na.rm = TRUE), include.lowest = TRUE, dig.lab = 10))
  df$centile <- as.factor(df$centile)
  levels(df$centile) <- seq(1, 100, by = 1)
  if (decile) {
    df$centile <- cut(as.numeric(df$centile), breaks = seq(0, 100, by = 10))
    df$centile <- as.factor(df$centile)
    levels(df$centile) <- seq(1, 10, by = 1)
  }

  preval <- data.frame(matrix(nrow = 0, ncol = 4))

  if (phenotype_type %in% c("Ordered Categorical","Categorical")) {
    stop("Cannot use Categorical Phenotype for centiles plot. Please use Cases/Controls or Continuous Phenotype")
  } else if (phenotype_type == "Cases/Controls") {
    names(preval) <- c("centile", "prevalence", "n_cases", "n")

    # create prevalence of cases group by centiles
    for (val in levels(df$centile)) {
      n_cases <- sum(df$centile == val & as.logical(df$Phenotype) == TRUE, na.rm = TRUE)
      n_controls <- sum(df$centile == val & as.logical(df$Phenotype) == FALSE, na.rm = TRUE)
      n <- n_cases + n_controls
      preval <- rbind(preval, data.frame(
        "centile" = val,
        "n_cases" = n_cases,
        "prevalence" = n_cases / n,
        "n" = n
      ))
    }

    p <- ggplot(preval, aes(
      x = as.numeric(.data$centile),
      y = as.numeric(.data$prevalence) * 100,
      color = as.numeric(.data$prevalence)
    )) +
      geom_point(show.legend = FALSE) +
      xlim(1, ifelse(decile, 10, 100)) +
      labs(
        x = paste(ifelse(decile, "Deciles of", "Centiles of"), prs_col),
        y = paste("Prevalence of", phenotype_col)
      ) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 11),
        axis.text.x.bottom = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y.left = element_text(size = 11)
      )
  } else if (phenotype_type == "Continuous") {
    if (is.na(continuous_metric)) {
      continuous_metric <- "mean"
    }

    names(preval) <- c("centile", "median", "mean", "n")

    # create median of continuous phenotype group by centiles
    for (val in levels(df$centile)) {
      med <- median(df[which(df$centile == val), "Phenotype"], na.rm = TRUE)
      mean <- mean(df[which(df$centile == val), "Phenotype"], na.rm = TRUE)
      n <- nrow(df[which(df$centile == val), ])
      preval <- rbind(preval, data.frame(
        "centile" = val,
        "median" = med,
        "mean" = mean,
        "n" = n
      ))
    }

    p <- ggplot(preval, aes(
      x = as.numeric(.data$centile),
      y = as.numeric(get(continuous_metric)),
      color = as.numeric(get(continuous_metric))
    )) +
      geom_point(show.legend = FALSE) +
      xlim(1, ifelse(decile, 10, 100)) +
      labs(
        x = paste(ifelse(decile, "Deciles of", "Centiles of"), prs_col),
        y = paste(continuous_metric, phenotype_col)
      ) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = 11),
        axis.text.x.bottom = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.y.left = element_text(size = 11)
      )
  }

  return(p)
}
