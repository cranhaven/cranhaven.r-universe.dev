#' compile pairwise relationships from the samples
#'
#' Run this on some of the output from `slurp_spip()`.
#' @param S a tibble. In the context of this package this tibble is
#' typically going to often be the
#' `samples` component of the output slurped up from spip with `slurp_spip()`.
#' More generally, it is a tibble that must have the columns:
#' - `ID`: the id of the sample
#' - `ancestors`: a list column of the ancestor vectors of each individual
#' - `relatives`: a list column of the vectors of individual samples (including self)
#' that each individual is related to.
#' @return a tibble with columns `id_1` and `id_2` for each pair.  Any additional
#' columns outside of `relatives` will be joined with `_1` and
#' `_2` suffixes.  In a typical run slurped up from spip this leads to the following
#' columns:
#'    - `id_1`: the id of the first sample of the pair,
#'    - `id_2`: the id of the 2nd sample of the pair,
#'    - `conn_comp`: the index of the connected component to which the pair belongs,
#'    - `dom_relat`: the dominant relationship that the pair shares,
#'    - `max_hit`: the number of shared ancestors at the level of the dominant relationship
#'    - `dr_hits`: a list column of two-vectors---the number of shared ancestors at the level
#'      of the dominant relationship in the upper and lower quadrants, respectively of the
#'      ancestry match matrix. If the relationship is symmetrical, the two values are the same.
#'    - `upper_member`: for non-symmetrical relationships, a 1 or a 2 indicating which member of the
#'      pair is the one that is typically older (i.e. the uncle in an uncle-nephew relationship), or
#'      NA if the relationship is symmetrical.
#'    - `times_encountered`: the number of times this pair was encountered when processing the
#'      output of the depth first search algorithm that found these pairs.  Not typically used for
#'      downstream analyses.
#'    - `primary_shared_ancestors`: a list columns of two-vectors.  The first element of each is the
#'      the position in the ancestry vector of id_1's primary shared ancestor.  The second element is
#'      the same for id_2.
#'    - `psa_tibs`: like `primary_shared_ancestor` but a list column of tibbles.
#'    - `pop_pre_1`, `pop_post_1`, `pop_dur_1`: the population from which the id_1 individual
#'      was sampled during the prekill, postkill, or during-reproduction sampling episodes,
#'      respectively.  NA for episodes in which the individual was not sampled
#'    - `pop_pre_2`, `pop_post_2`, `pop_dur_2`: same as above for the id_2 individual.
#'    - `sex_1`: sex of the id_1 individual,
#'    - `sex_2`: sex of the id_2 individual,
#'    - `born_year_1`: birth year of the id_1 individual,
#'    - `born_year_2`: birth year of the id_2 individual,
#'    - `samp_years_list_pre_1`: list column of years during which the id_1 individual was sampled
#'      during the prekill episode.
#'    - `samp_years_list_dur_1`: list column of years during which the id_1 individual was sampled
#'      during reproduction.
#'    - `samp_years_list_post_1`: list column of years during which the id_1 individual was sampled
#'      during the postkill episode.
#'    - `samp_years_list_1`: by default this column is identical to `samp_years_list_post_1` and is the
#'      column used in downstream plotting by some functions.  If you want to use a different column,
#'      for example `samp_years_list_pre_1` for the downstream plotting, then set the value of
#'      `samp_years_list_1` to the same values,
#'    - `samp_years_list_pre_2`, `samp_years_list_dur_2`, `samp_years_list_post_2`, `samp_years_list_2`: same
#'      as above but for individual with id_2,
#'    - `born_pop_1`: index of population in which id_1 was born,
#'    - `ancestors_1`: ancestry vector of id_1,
#'    - `born_pop_2`: index of population in which id_2 was born,
#'    - `ancestors_2`: ancestry vector of id_2,
#'    - `anc_match_matrix`: the ancestry match matrix (a logical matrix) for the pair.
#' @export
#' @examples
#' C <- compile_related_pairs(three_pops_with_mig_slurped_results$samples)
compile_related_pairs <- function(S) {

  # toss the sampled indivs with no relatives
  S2 <- S %>%
    filter(map_int(relatives, length) > 1)

  # keep just the columns that we will join in later
  S2_joiner <- S2 %>%
    select(-relatives)

  # find all pairs, in a lexicographically sorted order
  pairs_mat <- lapply(S2$relatives, function(x) {
    t(combn(sort(x), 2))
  }) %>%
    do.call(rbind, .)
  colnames(pairs_mat) <- c("id_1", "id_2")

  pairs_tib_1 <- as_tibble(pairs_mat)

  # count how many times each pair occurs. I think that all those that
  # occur only once will not end up being relatives. They just happen to both
  # be related someone else, but they are not closely enough related to count.
  # So, I will just record that now and verify.  Ultimately, we can probably toss them
  S2_1 <- S2_joiner
  names(S2_1) <- paste0(names(S2_1), "_1")
  S2_2 <- S2_joiner
  names(S2_2) <- paste0(names(S2_2), "_2")

  # join those together and compute the ancestor-matching matrix (the anc_match_matrix).
  # and, at the end, determine the "primary shared ancestors" (see the paper for an explanation).
  pairs_tib_2 <- pairs_tib_1 %>%
    count(id_1, id_2) %>%
    filter(n > 1) %>%  # toss out the ones only seen once because they are not true relationships
    left_join(S2_1, by = c("id_1" = "ID_1")) %>%
    left_join(S2_2, by = c("id_2" = "ID_2")) %>%
    mutate(
      anc_match_matrix = map2(
        .x = ancestors_1,
        .y = ancestors_2,
        .f = function(x, y) outer(x, y, "==")
      )
    )  %>%
    mutate(no_relat = map_lgl(.x = anc_match_matrix, .f = function(x) all(x == FALSE))) %>%
    filter(no_relat == FALSE) %>%
    select(-no_relat) %>%
    mutate(
      primary_shared_ancestors = map(.x = anc_match_matrix, .f = primary_ancestor_pairs)
    )  %>%
    mutate(psa_tibs = map(  # also save those primary ancestors in tibble form
      .x = primary_shared_ancestors,
      .f = function(m) {
        tibble(
          prim_anc_1 = map_int(m, 1),
          prim_anc_2 = map_int(m, 2)
        )
      }
    ))

    # now, we want categorize the "dominant relationship" of each pair.  This is done by
    # cycling over (within a function) relationships from Self to PO to Sib to Aunt, etc
    # and returning the first one that qualifies.  We also return how many hits are within
    # each of those relationships zones. Then we pull the list elements type and hits into
    # their own columns
    pairs_tib_3 <- pairs_tib_2 %>%
      mutate(
        dr = map(
          .x = anc_match_matrix,
          .f = function(x) {cat_dom_relat(x)}
        ),
        dom_relat = map_chr(dr, function(x) x$type),
        dr_hits = map(dr, function(x) x$hits)
      ) %>%
      select(-dr) %>%
      mutate(
        upper_member = map2_int(
          .x = dom_relat,
          .y = dr_hits,
          .f = function(x, y) {
            if(x %in% c("Se", "Si", "FC"))
              return(NA_integer_)
            else if(y[1] == y[2])
              return(0L)
            else if(y[1] > y[2])
              return(1L)
            else if(y[1] < y[2])
              return(2L)
            else
              return(-999L)  # flags that things got through here and should not have
          }
        ),
        max_hit = map_int(
          .x = dr_hits,
          .f = function(x) max(x)
        )
      )

    # we will return that for now, after adding conn_comps to it
    pairs_tib_3 %>%
      rename(times_encountered = n) %>%
      select(
        id_1,
        id_2,
        dom_relat,
        max_hit,
        dr_hits,
        upper_member,
        times_encountered,
        primary_shared_ancestors,
        psa_tibs,
        starts_with("pop_"),
        starts_with("sex_"),
        starts_with("born_year_"),
        starts_with("samp_years_list"),
        everything()
      ) %>%
      relpair_conn_comps()

}
