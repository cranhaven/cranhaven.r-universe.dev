#' Join Multiple Species Delimitation Methods Outputs
#'
#' @description
#' `delim_join()` returns a [tbl_df][tibble::tbl_df] of species delimitation
#' outputs whose partitions are consistent across different methods.
#'
#' @param delim A [list][base::list] or [data.frame][base::data.frame] of multiple
#' species delimitation methods outputs.
#'
#' @details
#' `delim_join()` is a helper function to join multiple lists or columns of species
#' delimitation outputs into a single [tbl_df][tibble::tbl_df] while keeping consistent
#' identifications across multiple methods. Species delimitation outputs are in general a
#' list or data frame of sample labels and its species partitions (Species 1, Species 2, etc.). These
#' partition names may be or not the same across two or more methods. `delim_join()` standardizes
#' partition names across two or more species delimitation outputs while keeping its underlying structure intact.
#'
#' @return
#' an object of class [tbl_df][tibble::tbl_df].
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins.
#'
#' @examples
#' 
#' \donttest{
#' ## run GMYC
#' gmyc_res <- splits::gmyc(ape::as.phylo(geophagus_beast), method = "single")
#'
#' # create a tibble
#' gmyc_df <- gmyc_tbl(gmyc_res)
#'
#' ## run bGMYC
#' bgmyc_res <- bGMYC::bgmyc.singlephy(ape::as.phylo(geophagus_beast),
#'   mcmc = 11000,
#'   burnin = 1000,
#'   thinning = 100,
#'   t1 = 2,
#'   t2 = ape::Ntip(ape::as.phylo(geophagus_beast)),
#'   start = c(1, 0.5, 50)
#' )
#'
#' # create a tibble
#' bgmyc_df <- bgmyc_tbl(bgmyc_res, ppcutoff = 0.05)
#'
#' ## LocMin
#'
#' # create a distance matrix
#' mat <- ape::dist.dna(geophagus, model = "raw", pairwise.deletion = TRUE)
#'
#' # estimate local minima from `mat`
#' locmin_res <- spider::localMinima(mat)
#'
#' # create a tibble
#' locmin_df <- locmin_tbl(mat,
#'   threshold = locmin_res$localMinima[1],
#'   haps = ape::as.phylo(geophagus_beast)$tip.label
#' )
#'
#' # join delimitations
#' all_delims <- delim_join(list(gmyc_df, bgmyc_df, locmin_df))
#'
#' # check
#' all_delims
#' 
#' }
#'
#' @export
delim_join <- function(delim) {
  if (methods::is(delim, "list")) {
    # check delimitations
    if (isTRUE(delimtools::check_delim(delim))) {
      # reduce list to wide format
      delim <- delim |>
        purrr::reduce(dplyr::full_join, by = "labels")
    }
  }

  if (methods::is(delim, "data.frame")) {
    # check for NA values
    if (anyNA(delim)) {
      cli::cli_abort(c("Missing values found across columns.",
        "x" = "You've supplied an input with missing values.",
        "i" = "Please provide a numeric value or remove rows with NAs"
      ))
    }
  }

  # reduce from wide to long format
  dlong <- delim |>
    tidyr::pivot_longer(
      cols = -labels,
      names_to = "method",
      values_to = "delims"
    ) |>
    tidyr::unite("delims", "method":"delims", sep = "") |>
    dplyr::group_by(.data$delims)

  # get group names
  group_names <- dplyr::group_keys(dlong) |> dplyr::pull()

  # turn into a list
  dlist <- dlong |>
    dplyr::group_split() |>
    purrr::set_names(group_names) |>
    purrr::map(dplyr::select, -.data$delims) |>
    purrr::map(unlist, use.names = FALSE)

  # loop 1
  ff <- list()
  for (i in seq_along(dlist)) { #
    ff[[i]] <- which(dlist %in% dlist[i]) #
  } #
  sff <- lapply(ff, sort)
  dff <- sff[!duplicated(sff)]

  # loop 2
  new.labs <- paste0("sp", rep(1:length(dff)))
  dd <- vector(mode = "character", length = length(dlist))
  for (i in 1:length(dff)) { #
    dd[dff[[i]]] <- new.labs[i] #
  } #

  # names
  names(dlist) <- paste(gsub("[0-9]+", "", names(dlist)), dd, sep = "-")

  # join
  delim_df <- tibble::tibble(
    gr = rep(names(dlist), sapply(dlist, length)),
    labels = unlist(dlist)
  ) |>
    dplyr::mutate(method = stringr::str_remove(.data$gr, "-sp[0-9]+")) |>
    tidyr::pivot_wider(id_cols = "labels", names_from = "method", values_from = "gr") |>
    dplyr::mutate(dplyr::across(.cols = -labels, .fns = ~ stringr::str_split_fixed(., "-", n = 2)[, 2]))

  return(delim_df)
}
