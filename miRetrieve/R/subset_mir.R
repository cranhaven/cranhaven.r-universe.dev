#' Subset data frame for specific miRNA names
#'
#' Subset data frame for specific miRNA names only.
#'
#' Subset data frame for specific miRNA names only.
#'
#' @param df Data frame containing a miRNA names.
#' @param mir.retain Character vector. Vector specifying which miRNA names to keep.
#' miRNA names in `mir.retain` must match miRNA names in `col.mir` in `df`.
#' @param col.mir Symbol. Column containing miRNA names.
#'
#' @return Data frame containing only specified miRNA names.
#' If no miRNA name in `mir.retain` matches a miRNA name in `col.mir`, `subset_mir()` stops
#' with a warning saying *"No miRNA name in 'mir.retain' matches a miRNA name in 'col.mir'.
#' Could not filter for miRNA name."*.
#'
#' @seealso [get_mir()], [subset_mir_threshold()]
#'
#' @family subset functions
#'
#' @importFrom magrittr %>%
#'
#' @export
subset_mir <- function(df,
                       mir.retain,
                       col.mir = miRNA) {
  # Etracts all miRNA names in df
  vec_mir <- df %>%
    dplyr::select({{col.mir}}) %>%
    dplyr::pull() %>%
    unique()
  # Test if miRNA can be extracted
  no_shared_mir <- get_shared_mir_vec(vec_mir, mir.retain) %>%
    length()

  if(no_shared_mir == 0) {
    stop("No miRNA name in 'mir.retain' matches a miRNA name in 'col.mir'.
            Could not filter for miRNA name.")
  }

  df <- df %>%
    dplyr::filter({{col.mir}} %in% mir.retain)

  return(df)
}

#' Subset data frame for miRNA names exceeding a threshold
#'
#' Subset data frame for miRNA names whose frequency exceeds a threshold.
#'
#' Subset data frame for miRNA names whose frequency exceeds a threshold.
#' This threshold can either
#' be an absolute value, e.g. 3, or a float between 0 and 1, e.g. 0.2.
#' If `threshold` is an absolute value, `subset_mir_threshold()` retains
#' miRNA names mentioned in at least `threshold` abstracts.
#' If `threshold` is a float between 0 and 1, `subset_mir_threshold()` retains
#' miRNA names mentioned in at least `threshold` abstracts
#' of all abstracts in `df`.
#'
#' @param df Data frame containing miRNA names and a PubMed-IDs.
#' @param threshold Integer or float. If `threshold` >= 1, retains
#' miRNA names in at least `threshold` abstracts.
#' If `threshold` is between 0 and 1, retains miRNA names mentioned
#' in at least `threshold` abstracts of all abstracts in `df`.
#' @param col.mir Symbol. Column containing miRNA names.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#'
#' @return Data frame, subset for miRNA names whose frequency exceeds a
#' threshold.
#'
#' @seealso [get_mir()], [subset_mir()]
#'
#' @family subset functions
#'
#' @importFrom magrittr %>%
#'
#' @export
subset_mir_threshold <- function(df,
                                 threshold = 1,
                                 col.mir = miRNA,
                                 col.pmid = PMID) {

  if(threshold < 0){
    stop("Threshold must be equal or greater than 0.")
  }

  n_row <- df %>%
    dplyr::select({{col.pmid}}) %>%
    dplyr::pull() %>%
    unique() %>%
    length()

  df_count <- df %>%
    dplyr::add_count({{col.mir}}) %>%
    dplyr::mutate(total = n_row) %>%
    dplyr::mutate(perc = n / n_row)

  if(threshold < 0.9999 & threshold > 0.0000001) {
    df_count <- df_count %>%
      dplyr::filter(perc >= threshold) %>%
      dplyr::select(-perc, -total, -n)

    return(df_count)
  } else {
    df_count <- df_count %>%
      dplyr::filter(n >= threshold) %>%
      dplyr::select(-perc, -total, -n)

    return(df_count)
  }
}
