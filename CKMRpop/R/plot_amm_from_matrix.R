#' plot an ancestry matrix (or multiple such matrices) from its (their) matrix form
#'
#' For illustration purposes, if you want to simply plot an ancestry
#' matrix (or several) to show particular values, then this is the
#' handy function for you.
#' @param X input tibble with a factor or character column `ID` that gives
#' the "name" of the ancestry matrix that will be used if you want to facet
#' over the values in `ID`. And also `X` must have a list column `anc_match_matrix` each
#' element of which is a logical ancestry match matrix.  `X` may have a list column
#' of tibbles called `psa_tibs` that says which cells are the primary shared ancestors.
#' @return `plot_amm_from_matrix()` returns a ggplot object: each facet is an image of the
#' ancestry match matrix.  It is facet-wrapped over the values in the ID column of `X`.
#' @export
#' @examples
#' # get some input: all the 2-generation AMMs in `example_amms`
#' X <- example_amms[stringr::str_detect(names(example_amms), "2gen$")] %>%
#'     tibble::enframe(name = "ID", value = "anc_match_matrix")
#'
#' # plot those
#' g <- plot_amm_from_matrix(X) +
#'     ggplot2::facet_wrap(~ ID)
#'
plot_amm_from_matrix <- function(X) {
  X2 <- X %>%
    select(ID, anc_match_matrix) %>%
    mutate(amm_as_tib = map(anc_match_matrix, amm2tibble)) %>%
    unnest(amm_as_tib) %>%
    mutate(
      ind_1 = factor(ancestor_abbrvs(max(x))[x], levels = ancestor_abbrvs(max(x))),
      ind_2 = factor(ancestor_abbrvs(max(y))[y], levels = ancestor_abbrvs(max(y)))
    ) %>%
    mutate(amm = as.character(amm))


  basic_amm_plot(X2)
}
