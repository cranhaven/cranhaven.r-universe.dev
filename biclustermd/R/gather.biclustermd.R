#' Gather a biclustermd object
#'
#' @param data a \code{biclustermd} object to gather.
#' @param key unused; included for consistency with \code{tidyr} generic
#' @param value unused; included for consistency with \code{tidyr} generic
#' @param ... unused; included for consistency with \code{tidyr} generic
#' @param na.rm unused; included for consistency with \code{tidyr} generic
#' @param convert unused; included for consistency with \code{tidyr} generic
#' @param factor_key unused; included for consistency with \code{tidyr} generic
#'
#' @return A data frame containing the row names and column names of both the
#'   two-way table of data biclustered and the cell-average matrix.
#'
#' @importFrom dplyr arrange group_by inner_join select ungroup
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#'
#' @export
#'
#'
#' @examples
#' data("synthetic")
#'
#' bc <- biclustermd(synthetic, col_clusters = 3, row_clusters = 2,
#'                 miss_val = mean(synthetic, na.rm = TRUE),
#'                 miss_val_sd = sd(synthetic, na.rm = TRUE),
#'                 col_min_num = 2, row_min_num = 2,
#'                 col_num_to_move = 1, row_num_to_move = 1,
#'                 max.iter = 10)
#' gather(bc)
#'
#' # bicluster 6 is in the top right-hand corner here:
#' autoplot(bc)
#'
#' # bicluster 3 is in the bottom right-hand corner here:
#' autoplot(bc)
#'
gather.biclustermd <- function(data, key = NULL, value = NULL, ..., na.rm = FALSE,
                               convert = FALSE, factor_key = FALSE) {

  # if statements shouldn't be necessary but keep as safeguard
  if(is.null(rownames(data$data))) {
    row_name_map <- data.frame(
      row_name = as.character(seq_len(nrow(data$data))),
      row_no = seq_len(nrow(data$data))
    ) %>%
      mutate(row_name = formatC(row_no, width = nchar(max(row_no)), flag = '0')) %>%
      mutate(row_name = paste0("R", row_name))

    rownames(data$data) <- row_name_map$row_name
  }

  if(is.null(colnames(data$data))) {
    col_name_map <- data.frame(
      col_name = as.character(seq_len(ncol(data$data))),
      col_no = seq_len(ncol(data$data))
    ) %>%
      mutate(col_name = formatC(col_no, width = nchar(max(col_no)), flag = '0')) %>%
      mutate(col_name = paste0("C", col_name))

    colnames(data$data) <- col_name_map$col_name
  }

  dat <- as.data.frame(data$data)
  bc <- data[-1]
  q <- bc$Q
  p <- bc$P
  rownames(q) <- rownames(dat)
  rownames(p) <- colnames(dat)
  rnames <- rownames(dat)

  dat$row_name <- rnames
  dat <- dat[, c(ncol(dat), 1:(ncol(dat) - 1))]
  colnames(dat)[1] <- "row_name"

  dat <- dat %>%
    gather(col_name, value, -row_name)
  dat$row_cluster <- unlist(
    lapply(1:nrow(dat), function(n) {
      which(q[rownames(q) == dat$row_name[n], ] == 1)
    })
  )
  dat$col_cluster <- unlist(
    lapply(1:nrow(dat), function(n) {
      which(p[rownames(p) == dat$col_name[n], ] == 1)
    })
  )

  bicluster_no_tbl <- unique(dat[, c('row_cluster', 'col_cluster')])
  bicluster_no_tbl$bicluster_no <- seq_along(bicluster_no_tbl$row_cluster)

  # bicluster_no_tbl <- dat %>%
  #   select(row_cluster, col_cluster) %>%
  #   arrange(row_cluster, col_cluster) %>%
  #   distinct() %>%
  #   mutate(bicluster_no = row_number())

  dat <- dat %>%
    inner_join(bicluster_no_tbl, by = c('row_cluster', 'col_cluster')) %>%
    arrange(row_cluster, col_cluster) %>%
    select(row_name, col_name, row_cluster, col_cluster, bicluster_no, value) %>%
    data.frame()

  # prior to dplyr 0.8.99.9002
  # dat <- dat %>%
  #   arrange(row_cluster, col_cluster) %>%
  #   group_by(row_cluster, col_cluster) %>%
  #   mutate(bicluster_no = group_indices()) %>%
  #   ungroup() %>%
  #   select(row_name, col_name, row_cluster, col_cluster, bicluster_no, value) %>%
  #   data.frame()
  dat
}

