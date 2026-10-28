#' Gini decomposition components
#'
#' This function calculates the Gini correlation between two distributions.
#' @param .data A data frame, or data frame extension (e.g. a tibble)
#' @param ... One or more unquoted expressions separated by commas indicating income sources to consider in the decomposition. Variable names can be used as if they were positions in the data frame.
#' @param .by A column to group the calculations by.
#' @param .wgt an optional vector of weights to apply in computation. Should be NULL or a numeric vector.
#' @return A tibble containing three components (for each source) of income decomposition.
#' @importFrom stats weighted.mean
#' @importFrom tidyr pivot_longer
#' @noRd
#'





gini.source.decomp.comp <- function(.data, ..., .by = NULL, .wgt = NULL) {
  sources <- dplyr::select(.data, ..., {{.by}})

  sources$Total_Income <- sources %>%
    dplyr::select(-{{.by}}) %>%
    rowSums()


  if(ncol(dplyr::select(.data, {{.wgt}})) == 0)
    sources$W <- 1
  else
    sources$W  <- dplyr::pull(.data, {{.wgt}})


  decomp_components <- sources %>%
    pivot_longer(cols = - c(Total_Income, W, {{.by}}), names_to = "income_source", values_to = "valeur_revenu") %>%
    dplyr::group_by(dplyr::across({{.by}}), income_source) %>%
    dplyr::summarise(Share = stats::weighted.mean(valeur_revenu, w = W) / weighted.mean(Total_Income, w = W),
              Gini = gini_wtd_ord(valeur_revenu, weights = W),
              Gini_corr = gini_corr(valeur_revenu, Total_Income, weights = W),
              .groups = "drop")


  return(decomp_components)

}
