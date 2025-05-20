
#' downsample the number of individuals sampled
#'
#' This discards individuals from the sample, randomly, until
#' the desired number of samples is achieved, then it
#' returns only those pairs in which both members are part of
#' the retained samples.
#' @param S the tibble of samples with columns at least of `ID` and `samp_years_list`. Typically
#' this will be what is returned in the `samples` component from `slurp_spip()`.
#' @param P the tibble of pairs. Typically this will be what has been returned from
#' `compile_related_pairs()`.
#' @param n The desired number of individuals (or instances, really, see below) to
#' retain in the sample.
#' @return This returns a list with two components as follows:
#'    - `ds_samples`: A tibble like `S` except having randomly removed individuals
#'      so as to only have n left.
#'    - `ds_pairs`: A tibble like `P` except having removed any pairs that
#'    include individuals that were not retained in the sample.
#' @export
#' @examples
#' # prepare some input
#' S <- three_pops_with_mig_slurped_results$samples
#' P <- compile_related_pairs(three_pops_with_mig_slurped_results$samples)
#' result <- downsample_pairs(S, P, n = 500)
#'
#' # print the result
#' result
downsample_pairs <- function(S, P, n) {

  # here is how we do it when assuming that we want to downsample
  # sample episodes, rather than individuals (I think this should be
  # the default).

  # first, we need to unnest the samp_years of each individual
  S2 <- S %>%
    select(ID, samp_years_list) %>%
    unnest(samp_years_list)

  if(nrow(S2) < n) {
    stop(paste0("Sorry, you only have ", nrow(S2), " sampling instances. Not enough to downsample them to n = ", n))
  }

  # now we need to downsample them
  S3 <- S2 %>%
    sample_n(n)

  # make a sample list to return
  Sret <- S3 %>%
    group_by(ID) %>%
    summarise(samp_years_list = map(samp_years_list, function(x) x)) %>%
    ungroup() %>%
    left_join(S %>% select(-samp_years_list), by = "ID")

  # now, only retain the pairs that have sampling instances that they should
  P2 <- P %>%
    unnest(samp_years_list_1) %>%
    unnest(samp_years_list_2) %>%
    filter(
      id_1 %in% S3$ID,
      samp_years_list_1 %in% S3$samp_years_list,
      id_2 %in% S3$ID,
      samp_years_list_2 %in% S3$samp_years_list
    )

  # finally, nest up the samp_years_lists on those again
  P3 <- P2 %>%
    nest(tmp_1 = c(samp_years_list_1)) %>%
    nest(tmp_2 = c(samp_years_list_2)) %>%
    mutate(
      samp_years_list_1 = map(tmp_1, function(x) x$samp_years_list_1),
      samp_years_list_2 = map(tmp_2, function(x) x$samp_years_list_2)
    )

  # now, get the columns in the same order
  P4 <- P3[, names(P)]

  # finally, reset the connected components
  P5 <- P4 %>%
    select(-conn_comp) %>%
    relpair_conn_comps()

  # and return that
  list(
    ds_samples = Sret,
    ds_pairs = P5
  )
}
