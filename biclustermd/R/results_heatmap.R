#' Make a heatmap of sparse biclustering results
#'
#' @param x A \code{biclustermd} object.
#' @param reorder A logical. If TRUE, heatmap will be sorted according to the cell-average matrix, \code{A}.
#' @param col_clusts A vector of column cluster indices to display. If NULL (default), all are displayed.
#' @param row_clusts A vector of row cluster indices to display. If NULL (default), all are displayed.
#' @param cell_alpha A scalar defining the transparency of shading over a cell and by default this equals 1/5.
#'   The color corresponds to the cell mean.
#' @param transform_colors If equals `TRUE` then the data is scaled by
#'     `c` and run through a standard normal cdf before plotting. If `FALSE` (default), raw data
#'     values are used in the heat map.
#' @param c Value to scale the data by before running it through a standard normal CDF.
#'     Default is 1/6.
#' @param ... Arguments to be passed to `geom_vline()` and `geom_hline()`.

#' @importFrom tidyr gather
#' @importFrom dplyr left_join group_by ungroup
#' @importFrom ggplot2 ggplot aes geom_rect geom_tile geom_hline geom_vline scale_fill_gradientn theme_bw theme
#' @importFrom grDevices rainbow
#' @return An object of class ggplot.

results_heatmap <- function (x, reorder = FALSE, transform_colors = FALSE, c = 1/6,
                          cell_alpha = 1/5, col_clusts = NULL, row_clusts = NULL,
                          ...) {
  bc <- x
  P <- bc$P
  Q <- bc$Q

  if(reorder == TRUE) {
    P <- reorder_biclust(bc)[[1]]
    Q <- reorder_biclust(bc)[[2]]
  }

  if(is.null(col_clusts)) {
    col_clusts <- c(1:ncol(P))
  }

  if(is.null(row_clusts)) {
    row_clusts <- c(1:ncol(Q))
  }

  cols <- as.integer(part_matrix_to_vector(P))
  rows <- as.integer(part_matrix_to_vector(Q))

  p_list <- lapply(1:ncol(P), function(x) which(cols == x))
  p_list <- p_list[sapply(p_list, length) != 0]
  p_list <- p_list[col_clusts]

  q_list <- lapply(1:ncol(Q), function(x) which(rows == x))
  q_list <- q_list[sapply(q_list, length) != 0]
  q_list <- q_list[row_clusts]

  col_ord <- unlist(p_list)
  p_num <- rep(seq_along(unlist(lapply(p_list, length), use.names = FALSE)),
               unlist(lapply(p_list, length), use.names = FALSE))

  row_ord <- unlist(q_list)
  q_num <- rep(seq_along(unlist(lapply(q_list, length), use.names = FALSE)),
               unlist(lapply(q_list, length), use.names = FALSE))

  ord_dat <- x$data[row_ord, col_ord]
  ord_dat <- as.data.frame(ord_dat)
  ord_dat$rows <- rownames(ord_dat)
  ord_dat$row_clust <- q_num

  col_clust_ind <- data.frame(cols = factor(colnames(ord_dat)[-c(ncol(ord_dat), ncol(ord_dat) - 1)]))
  col_clust_ind$col_clust <- rep(seq_along(p_list), unlist(lapply(p_list, length)))

  melted <- ord_dat %>%
    gather(cols, value, -rows, -row_clust) %>%
    mutate(cols = factor(cols)) %>%
    mutate(trans_data = pnorm(c * value)) %>%
    left_join(col_clust_ind, by = c("cols")) %>%
    group_by(row_clust, col_clust) %>%
    mutate(cell_mean = mean(value, na.rm = TRUE)) %>%
    mutate(cell_mean_trans = mean(trans_data, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(rows = factor(rows, levels = unique(rows))) %>%
    mutate(cols = factor(cols, levels = unique(cols)))

  vline_coords <- c(0.5, cumsum(sapply(p_list, length)) + 0.5)
  vline_coords <- data.frame(v = vline_coords)
  hline_coords <- c(0.5, cumsum(sapply(q_list, length)) + 0.5)
  hline_coords <- data.frame(h = hline_coords)

  res_list <- list(data = melted, vlines = vline_coords, hlines = hline_coords)

  coord_grid <- expand.grid(1:(length(res_list$hlines$h) - 1), 1:(length(res_list$vlines$v) - 1)) %>% arrange(Var1)
  rect_coords <- function(x, res_list) {
    r <- coord_grid[x, 1]
    c <- coord_grid[x, 2]

    xmn <- res_list$vlines$v[c]
    xmx <- res_list$vlines$v[c+1]
    ymn <- res_list$hlines$h[r]
    ymx <- res_list$hlines$h[r+1]
    fv <- mean(res_list$data$cell_mean[res_list$data$col_clust == c & res_list$data$row_clust == r], na.rm = TRUE)
    fv_trans <- mean(res_list$data$cell_mean_trans[res_list$data$col_clust == c & res_list$data$row_clust == r], na.rm = TRUE)

    c(xmn, xmx, ymn, ymx, fv, fv_trans)
  }

  dfc <- data.frame(do.call(rbind, lapply(1:nrow(coord_grid), function(x) rect_coords(x, res_list))))
  names(dfc) <- c("xmn", "xmx", "ymn", "ymx", "cell_mean", "cell_mean_trans")

  res_list$rect_coords <- dfc

  if (transform_colors == TRUE) {
    gg <- ggplot() +
      geom_tile(data = res_list$data, aes(y = rows, x = cols, fill = trans_data)) +
      geom_vline(data = res_list$vlines, aes(xintercept = v), ...) +
      geom_hline(data = res_list$hlines, aes(yintercept = h), ...) +
      geom_rect(data = res_list$rect_coords,
                aes(xmax = xmx, xmin = xmn, ymax = ymx, ymin = ymn, fill = cell_mean_trans),
                alpha = cell_alpha) +
      theme_bw() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  }
  else if (transform_colors == FALSE) {
    gg <- ggplot() +
      geom_tile(data = res_list$data, aes(y = rows, x = cols, fill = value)) +
      geom_vline(data = res_list$vlines, aes(xintercept = v), ...) +
      geom_hline(data = res_list$hlines, aes(yintercept = h), ...) +
      geom_rect(data = res_list$rect_coords,
                aes(xmax = xmx, xmin = xmn, ymax = ymx, ymin = ymn, fill = cell_mean),
                alpha = cell_alpha) +
      theme_bw() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  }
  return(gg)
}

