#' Convert a \code{biclustermd} object to a \code{Biclust} object
#'
#' @param object The \code{biclustermd} object to convert to a \code{Biclust} object
#'
#' @export
#'
#' @importFrom biclust BiclustResult
#' @importFrom dplyr summarise
#' @importFrom tidyr spread
#'
#' @return Returns an object of class \code{Biclust}.
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
#' bc
#'
#' as.Biclust(bc)
#'
#' # biclust::drawHeatmap won't work since it doesn't exclude NAs
#' \dontrun{biclust::drawHeatmap(synthetic, as.Biclust(bc), 6)}
#'
#' # bicluster 6 is in the top right-hand corner here:
#' autoplot(bc)
#' # compare with bicust::drawHeatmap2:
#' biclust::drawHeatmap2(synthetic, as.Biclust(bc), 6)
#'
#' # bicluster 3 is in the bottom right-hand corner here:
#' autoplot(bc)
#' # compare with bicust::drawHeatmap2:
#' biclust::drawHeatmap2(synthetic, as.Biclust(bc), 3)

as.Biclust <- function(object) {

  RowxNumber <- gather(object) %>%
    mutate(in_bicluster = !is.na(value)) %>%
    select(row_name, bicluster_no, in_bicluster) %>%
    group_by(row_name, bicluster_no) %>%
    summarise(in_bicluster = any(in_bicluster)) %>%
    ungroup() %>%
    arrange(bicluster_no) %>%
    mutate(bicluster_no = paste0("BC", bicluster_no)) %>%
    spread(bicluster_no, in_bicluster) %>%
    arrange(row_name)
  RowxNumber <- as.matrix(RowxNumber[, -1])
  colnames(RowxNumber) <- NULL
  RowxNumber[is.na(RowxNumber)] <- FALSE

  NumberxCol <- gather(object) %>%
    mutate(in_bicluster = !is.na(value)) %>%
    select(col_name, bicluster_no, in_bicluster) %>%
    group_by(col_name, bicluster_no) %>%
    summarise(in_bicluster = any(in_bicluster)) %>%
    ungroup() %>%
    arrange(bicluster_no) %>%
    mutate(bicluster_no = paste0("BC", bicluster_no)) %>%
    spread(bicluster_no, in_bicluster) %>%
    arrange(col_name)
  NumberxCol
  NumberxCol <- as.matrix(NumberxCol[, -1])
  colnames(NumberxCol) <- NULL
  NumberxCol[is.na(NumberxCol)] <- FALSE
  NumberxCol <- t(NumberxCol)

  Number <- ncol(object$P) * ncol(object$Q)

  BiclustResult(
    object$params,
    RowxNumber,
    NumberxCol,
    Number,
    list(NA)
  )

}
