#' Find ancestors and relative of each sampled member of a pedigree
#'
#' This is a fairly general function that can be applied to pedigree output
#' from any simulation program.  It calls a function written in C++ to
#' do all the recursive pedigree searching.
#' @param P a tibble that gives the pedigree.  It must have the columns `kid`, `pa`, `ma`.
#' These columns must be character vectors of the unique IDs for each individual in
#' the pedigree.
#' Every individual in the pedigree must appear exactly once in the `kid` column.  When founders
#' appear in the pedigree, their `pa` and `ma` entries must be "0" (i.e. character zero).
#' @param S a vector of the IDs of the sampled individuals.  Obviously, all members of S must appear
#' in P.
#' @param n number of generations back from the sampled individuals to return in each
#' individual's ancestor vector. 0 = self, 1 =  back to and including the parents,
#' 2 = back to and including the grandparents, and so on.
#' @return A tibble with three columns:
#' - `sample_id`: the ID names of the sampled individuals
#' - `ancestors`: a list column.  Each element is a vector of the ids of the ancestors of the
#' sampled individual in the 2^(n+1) - 1 positions.  The first is the sampled individual, the second
#' is pa, third is ma, fourth is pa's pa, fifth is pa's ma, sixth is ma's pa, and so forth.
#' - `relatives`: a list column. Each element is a vector of the ids of the individuals that are _sampled_ relatives
#' within the n generations.  The first element is the sampled individual itself, and the remaining
#' ones are all the relatives of that individual whom were also sampled.
#' @keywords internal
#' @export
#' @examples
#' # get some input variables
#' P <- three_pops_with_mig_slurped_results$pedigree
#' S <- three_pops_with_mig_slurped_results$samples$ID
#' n <- 2
#' result <- find_ancestors_and_relatives_of_samples(P, S, n)
#' result
find_ancestors_and_relatives_of_samples <- function(P, S, n) {

  DFS_input <- prepare_for_dfs(P, S)

  ra <- rcpp_ancestors_and_relatives(DFS_input, n)

  # get the sample names as the first element in each of the AV vectors:
  sample_ids = lapply(ra$AV, function(x) x[1]) %>%
    unlist()

  tibble(
    sample_id = sample_ids,
    ancestors = ra$AV,
    relatives = ra$REL
  )
}
