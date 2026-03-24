#' Extract sequences of length k within clusters
#'
#' For each cluster, extract all sequence of length \code{k} from the ordered observations grouped by individual
#' IDs. Returns a list of sequences per cluster.
#'
#' @param dt A \code{data.table} or data.frame containing the data in a long format.
#' @param cl_col Name of the column containing cluster labels.
#' @param id_col Name of the column identifying individual trajectories (e.g. patient ID).
#' @param event_col Name of the column containing ordered events (e.g. diagnoses, prescriptions).
#' @param aos_col Name of the column containing age at onset.
#' @param cens Code indicating censoring.
#' @param k Integer specifying the sequence length (recomended 2).
#'
#' @return A named list of data frames, each containing sequences of length \code{k} observed in a given cluster.
#' @references Delord M, Douiri A (2025) <doi:10.1186/s12874-025-02476-7>
#' @author Marc Delord
#' @seealso \code{\link[arulesSequences]{cspade}} in the \pkg{arulesSequences} package for sequential pattern
#' mining using the SPADE algorithm.
#' @importFrom data.table setDT as.data.table :=
#' @importFrom utils combn
#' @importFrom rlang enquo as_name
#' @importFrom stats median
#' @importFrom dplyr distinct all_of
#' @importFrom dplyr filter mutate group_by summarise count left_join select arrange
#' @keywords Censored state matrix
#' @concept Sequence analysis
#' @export
get_cluster_sequences <- function(dt, cl_col = "cl", id_col = "link_id", event_col = "reg",
                                  aos_col =  "aos" ,
                                  cens = 'cens',
                                  k = 2) {
  # Set as a data.table for faster computation
  setDT(dt)
  # Suppress the censoring as an event
  dt <- dt %>%
    filter( ! .data[[event_col]] == cens )
  # Compute the median age at onset
  dt_aos <- dt %>%
    group_by( .data[[cl_col]] , .data[[event_col]] ) %>%
    summarise(med = median( .data[[aos_col]] ), .groups = "drop") %>%
    left_join(
      dt %>%
        distinct( .data[[id_col]] , .keep_all = TRUE) %>%
        count( .data[[cl_col]] , name = "n"),
      by = cl_col
    )

  cl_values <- unique(dt[[cl_col]])
  seq_list <- vector("list", length(cl_values))
  nseq <- integer(length(cl_values))
  names(seq_list) <- cl_values

  for (i in seq_along(cl_values)) {
    cl_val <- cl_values[i]
    uids <- unique(dt[get(cl_col) == cl_val, get( id_col )])
    dt_sub <- dt[get(id_col) %in% uids & !is.na(get(event_col)),
                 .(link_id = get(id_col), reg = get(event_col))]

    combs <- dt_sub[, if (.N >= k) as.data.table(t(combn(reg, k))), by = link_id][, !"link_id"]

    seq_list[[i]] <- combs
    nseq[i] <- length(uids)
  }


 ret <- list(sequences = seq_list, n_by_cluster = nseq , dt_aos = dt_aos )
 attr(ret, "call") <- match.call()
 return(ret)
}
