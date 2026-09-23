#' @title
#' Deciles BoxPlot from a PGS Association with a Continuous Phenotype
#'
#' @description
#' `decileboxplot()` takes a distribution of PGS, a Continuous Phenotype.
#' Returns a plot with deciles of PGS in x and Boxplot of the Phenotype in y
#'
#' @param df a dataframe with individuals on each row, and at least the following
#' columns:
#'
#'  * one ID column,
#'  * one PGS column, with numerical continuous values following a normal distribution,
#'  * one Phenotype column, can be numeric (Continuous Phenotype), character, boolean or factors (Discrete Phenotype)
#' @param prs_col a character specifying the PGS column name
#' @param phenotype_col a character specifying the Continuous Phenotype column name
#'
#' @return return a ggplot object (ggplot2)
#' @importFrom stats na.omit
#' @import ggplot2
#' @export
decileboxplot <- function(df = NULL, prs_col = "SCORESUM", phenotype_col =
                            "Phenotype") {
  ## Checking inputs
  col_names <- df_checker(df, prs_col, phenotype_col, scale = FALSE)
  prs_col <- col_names$prs_col
  phenotype_col <- col_names$phenotype_col

  ## Taking only subset of df
  df <- df[, c(prs_col, phenotype_col)]
  names(df) <- c("PGS", "Phenotype")
  df <- na.omit(df)
  if (nrow(df) < 1000) {
    warning("The dataset has less than 1,000 individuals, deciles boxplot might not look good!")
  }

  ## Making centiles then deciles, ifelse using phenotype_type
  phenotype_type <- phenotype_type(df = df, phenotype_col = "Phenotype")

  df$centile <- with(df, cut(PGS, breaks = quantile(PGS, probs = seq(0, 1, by = 0.01), na.rm = TRUE), include.lowest = TRUE, dig.lab = 10))
  df$centile <- as.factor(df$centile)
  levels(df$centile) <- seq(1, 100, by = 1)
  df$decile <- cut(as.numeric(df$centile), breaks = seq(0, 100, by = 10))

  if (phenotype_type %in% c("Categorical", "Cases/Controls", "Ordered Categorical")) {
    stop("Cannot use a Categorical or a Cases/Controls phenotype for deciles boxplot. Please use Continuous Phenotype")
  } else if (phenotype_type == "Continuous") {
    p <- ggplot(df, aes(
      x = as.factor(.data$decile),
      y = as.numeric(.data$Phenotype),
      fill = mean(as.numeric(.data$Phenotype), na.rm = TRUE)
    )) +
      geom_boxplot(show.legend = FALSE, alpha = 0.4) +
      labs(
        x = paste("Deciles of", prs_col),
        y = phenotype_col
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
