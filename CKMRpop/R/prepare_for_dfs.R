#' Prepare input for the DFS relative-finding algorithm
#'
#' Uses the pedigree to convert individual IDs into indexes and
#' compile information to make the data structures for the DFS
#' algorithm to find relatives.
#' @param ped the pedigree.  A tibble with columns `kid`, `pa`, and `ma`
#' @param samp_vec A character vector with the names of the individuals that were sampled
#' @export
#' @keywords internal
prepare_for_dfs <- function(ped, samp_vec) {

  # figure out who the founders are.  These are the individuals whose parents are "0"
  founders <- ped %>%
    filter(pa == "0" | ma == "0") %>%
    pull(kid)

  # get all the unique names of the individuals.  Order doesn't matter, but we want
  # this to make a unique idx for each individual.  Remove "0" after the fact
  levs <- unique(c(ped$pa, ped$ma, ped$kid))
  levs <- levs[levs != "0"]

  # get the total number of individuals/nodes
  N <- length(levs)

  # get the total number of samples
  S <- length(samp_vec)

  # count up the number of children of each parent
  num_offs_pa <- ped %>%
    filter(pa != "0") %>%
    count(pa) %>%
    rename(parent = pa)
  num_offs_ma <- ped %>%
    filter(ma != "0") %>%
    count(ma) %>%
    rename(parent = ma)

  n_down <- bind_rows(
    num_offs_pa,
    num_offs_ma
  ) %>%
    rename(n_down = n)

  # here is a function for returning the 0-based index of a name
  idx0 <- function(name) {
    ret <- as.integer(factor(name, levels = levs)) - 1L
    ret[is.na(ret)] <- -1L
    ret
  }

  # now, we start adding columns to ped.  These columns we will
  # eventually break off into a matrix.
  ped2 <- ped %>%
    mutate(
      kid_idx = idx0(kid),
      pa_idx = idx0(pa),
      ma_idx = idx0(ma),
      sampled = as.integer(kid %in% samp_vec)
    ) %>%
    left_join(n_down, by = c("kid" = "parent")) %>%
    mutate(n_down = ifelse(is.na(n_down), 0L, n_down))

  # make the node matrix from that
  node_matrix <- ped2 %>%
    select(kid_idx:n_down) %>%
    as.data.frame() %>%
    as.matrix()

  # Now we need to make the down_matrix, counting children of every mother and father.
  # First, get a useful tibble.
  parents <- bind_rows(
    ped %>% filter(pa != "0") %>% select(pa, kid) %>% rename(parent = pa),
    ped %>% filter(ma != "0") %>% select(ma, kid) %>% rename(parent = ma)
  ) %>%
   #arrange(parent, kid) %>%  # sorting this is not necessary and it takes a LONG time fog big pedigrees
    group_by(parent) %>%
    mutate(
      child_num = (1:n()) - 1L,
    ) %>%
    ungroup() %>%
    mutate(
      parent_idx = idx0(parent),
      kid_idx = idx0(kid)
    ) %>%
    select(
      parent,
      kid,
      parent_idx,
      child_num,
      kid_idx
    )

  # then tweeze the down_matrix off of that.
  down_matrix <- parents %>%
    select(parent_idx, child_num, kid_idx) %>%
    as.data.frame() %>%
    as.matrix()

  # Get the names vec.  This must just be levs.
  names_vec <- levs

  # get a vector of the 0-based indexes of the samples:
  sample_vec = sort(idx0(samp_vec))

  # and, finally, return all that
  list(
    N = N,
    S = S,
    node_matrix = node_matrix,
    down_matrix = down_matrix,
    sample_vec = sample_vec,
    names_vec = names_vec
  )

}
