#' Gini decomposition by income sources
#'
#' This function provides a decomposition of Gini index by income sources based on the approach of Lerman and Yitzhaki (1985) <doi:10.2307/1928447> .
#' It provides a set of indicators :
#' - `income_source`: Column indicating each income source passed into the function call.
#' - `Share`: Column indicating the share of the income source to the total income.
#' - `Gini`: Column showing the Gini index for each income source.
#' - `Gini_corr`: Column showing the Gini correlation between the income source and the total income.
#' - `Absolute_Contribution`: Column showing the  absolute contribution of each income source to the global Gini index.
#' - `Relative_Contribution`: Column indicating the relative contribution of each income source to the global Gini index.
#'
#' @param .data A data frame, or data frame extension (e.g. a tibble)
#' @param ... One or more unquoted expressions separated by commas indicating income sources to consider in the decomposition. Variable names can be used as if they were positions in the data frame.
#' @param .by A column to group the calculations by.
#' @param .wgt an optional vector of weights to apply in computation. Should be NULL or a numeric vector.
#' @returns An object of class `data.frame` containing all the calculated indicators. The data.frame is grouped by the columns passed into `.by` argument.
#'
#' @export
#' @examples
#'
#' sample_income_data %>%
#'   gini_decomp_source(wage, self_employment_rev, farming_rev, other_rev)
#'
#' gini_decomp_source(sample_income_data, 3:6, .by = region, .wgt = sample_wgt)


gini_decomp_source <- function(.data, ..., .by = NULL, .wgt = NULL) {

  decomp_components <- gini.source.decomp.comp(.data, ..., .by = {{.by}}, .wgt = {{.wgt}})

  decomp_results <- decomp_components %>%
    dplyr::group_by(dplyr::across({{.by}})) %>%
    dplyr::mutate(Absolute_Contribution = Share * Gini * Gini_corr,
                  Relative_Contribution = Absolute_Contribution / sum(Absolute_Contribution, na.rm = T))

  return(decomp_results)

}


#' Gini income elasticity
#'
#' This function computes the elasticity of Gini index associated with a percentage change in the mean income (for each income source).
#' It provides a set of indicators :
#' - `income_source`: Column indicating each income source passed into the function call.
#' - `Share`: Column indicating the share of the income source to the total income.
#' - `Gini`: Column showing the Gini index for each income source.
#' - `Gini_corr`: Column showing the Gini correlation between the income source and the total income.
#' - `Elasticity`: Column indicating the elasticity of Gini index associated with a percentage change in the mean income source.
#' - `Marginal_Impact`: Column indicating the marginal impact of a change in the mean income source on the overall Gini index.

#' @param .data A data frame, or data frame extension (e.g. a tibble)
#' @param ... One or more unquoted expressions separated by commas indicating income sources to consider in the decomposition. Variable names can be used as if they were positions in the data frame.
#' @param .by A column to group the calculations by.
#' @param .wgt an optional vector of weights to apply in computation. Should be NULL or a numeric vector.
#' @returns An object of class `data.frame` containing all the calculated indicators. The data.frame is grouped by the columns passed into `.by` argument.
#' @export
#' @examples
#'
#' sample_income_data %>%
#'   gini_income_elasticity(wage, self_employment_rev, farming_rev, other_rev,
#'   .by = region)
#'
#' gini_income_elasticity(sample_income_data, 3:6, .by = region, .wgt = sample_wgt)



gini_income_elasticity <- function(.data, ..., .by = NULL, .wgt = NULL) {

  decomp_components <- gini.source.decomp.comp(.data, ..., .by = {{.by}}, .wgt = {{.wgt}})

  gini_income_elasticity <- decomp_components %>%
    dplyr::group_by(dplyr::across({{.by}})) %>%
    dplyr::mutate(Elasticity = Gini * Gini_corr / sum(Share * Gini * Gini_corr, na.rm = T),
                  Marginal_Impact = Share * (Elasticity - 1))

  return(gini_income_elasticity)
}


#' Growth-redistribution impacts on social welfare function.
#'
#' This function provides a Growth-redistribution decomposition of effects (for each income source) on social welfare function defined by Amartya Sen (1970, ISBN:978-0-444-85127-7).
#'
#' It provides a set of indicators :
#' - `income_source`: Column indicating each income source passed into the function call.
#' - `Share`: Column indicating the share of the income source to the total income.
#' - `Gini`: Column showing the Gini index for each income source.
#' - `Gini_corr`: Column showing the Gini correlation between the income source and the total income.
#' - `Growth_Effect`: Column indicating the effect of growth in the income source on the Social Welfare Function.
#' - `Redistribution_Effect`: Column indicating the effect of redistribution of the income source on the Social Welfare Function.
#' - `Total_Variation`: Column adding up both effects to calculate the overall effect of each income source on the Social Welfare Function.

#' @param .data A data frame, or data frame extension (e.g. a tibble)
#' @param ... One or more unquoted expressions separated by commas indicating income sources to consider in the decomposition. Variable names can be used as if they were positions in the data frame.
#' @param .by A column to group the calculations by.
#' @param .wgt an optional vector of weights to apply in computation. Should be NULL or a numeric vector.
#' @returns An object of class `data.frame` containing all the calculated indicators. The data.frame is grouped by the columns passed into `.by` argument.
#' @export
#' @examples
#'
#' sample_income_data %>%
#'   social_welfare_impact(wage, self_employment_rev, farming_rev, other_rev,
#'   .wgt = sample_wgt)
#'
#' social_welfare_impact(sample_income_data, 3:6, .by = region, .wgt = sample_wgt)


social_welfare_impact <- function(.data, ..., .by = NULL, .wgt = NULL) {

  decomp_components <- gini.source.decomp.comp(.data, ..., .by = {{.by}}, .wgt = {{.wgt}})

  social_welfare_impact <- decomp_components %>%
    dplyr::group_by(dplyr::across({{.by}})) %>%
    dplyr::mutate(Growth_Effect = Share / (1 - sum(Share * Gini * Gini_corr, na.rm = T)),
                  Redistribution_Effect = - Share * Gini * Gini_corr / (1 - sum(Share * Gini * Gini_corr, na.rm = T)),
                  Total_Variation = Growth_Effect + Redistribution_Effect)

  return(social_welfare_impact)
}
