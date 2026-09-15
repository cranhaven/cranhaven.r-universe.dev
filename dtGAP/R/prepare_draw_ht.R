#' Compute Row and Column Proportions for Layout
#' @noRd
heat_prop <- function(sorted_dat) {
  mat <- sorted_dat$sorted_test_matrix
  n_r <- nrow(mat)
  n_c <- ncol(mat)
  col_prop <- n_c / (n_r + n_c)
  row_prop <- n_r / (n_r + n_c)

  return(list(col = col_prop, row = row_prop))
}


#' Compute Layout Dimensions for Tree + Heatmap Plot
#'
#' @description
#' Determines panel widths and heights based on page dimensions, margin, and proportions.
#'
#' @param sorted_dat List returned by \code{sorted_mat()}. Must contain \code{sorted_test_matrix}.
#' @param margin Numeric. Margin around the drawing area (mm).
#' @param total_w Numeric. Total width of page (mm).
#' @param total_h Numeric. Total height of page (mm).
#' @param tree_p Numeric. Proportion of total width allocated to the tree panel.
#'
#' @return A list with:
#'   \item{tree_w}{Width for tree panel.}
#'   \item{heatmap_w}{Width for heatmap panel.}
#'   \item{total_draw_h}{Total drawable height after margin.}
#'   \item{row_h}{Height allocated to rows.}
#'   \item{col_h}{Height allocated to columns.}
#'   \item{tree_h}{Height for tree panel (same as row_h).}
#'   \item{offset_h}{Adjustment applied to ensure minimum column height.}
#'   \item{margin}{Margin passed through.}
#'
#' @export
#' @keywords internal

compute_layout <- function(sorted_dat,
                           margin = 20,
                           total_w = 297,
                           total_h = 210,
                           tree_p = 0.3) {
  tree_w <- total_w * tree_p
  heatmap_w <- total_w - tree_w
  row_prop <- heat_prop(sorted_dat)$row
  col_prop <- heat_prop(sorted_dat)$col


  total_draw_h <- total_h - margin
  row_h <- total_draw_h * row_prop
  col_h <- total_draw_h * col_prop


  offset_h <- 0
  if (any(col_h < 10)) {
    # handle too small col_ht
    offset_h <- 10 - pmin(col_h, 10)
    col_h <- pmax(col_h, 10)
  }
  tree_h <- row_h

  return(
    list(
      tree_w = tree_w,
      heatmap_w = heatmap_w,
      total_draw_h = total_draw_h,
      offset_h = offset_h,
      row_h = row_h,
      col_h = col_h,
      tree_h = tree_h,
      margin = margin
    )
  )
}

#' Build Split Factor for Heatmap Rows
#'
#' @description
#' Extracts leaf node IDs from tree data and aligns them to the rows of the
#' sorted proximity matrix to form a split factor.
#'
#' @param sorted_dat List from \code{sorted_mat()}
#' @param tree_res List from \code{compute_tree()}
#'
#' @return A factor indicating leaf grouping for each row in \code{row_pro_mat_sorted}.
#' @export
#' @keywords internal
get_split_vec <- function(sorted_dat, tree_res) {
  R_mat <- sorted_dat$sorted_test_matrix
  node_ids <- sorted_dat$node_ids

  plot_data <- tree_res$plot_data
  leaf_nodes <- plot_data %>%
    filter(kids == 0) %>%
    arrange(desc(y)) %>%
    pull(id) %>%
    as.character()

  split_vec <- factor(node_ids[match(rownames(R_mat), names(node_ids))], levels = leaf_nodes)

  return(split_vec)
}


#' Generate a Bundle of Legends for Heatmap Components
#'
#' @description
#' Creates and packs multiple legends (feature types, class labels, membership proportions,
#' raw values, and proximity metrics) into a single legend bundle for ComplexHeatmap.
#'
#' @param sorted_dat List. Output of \code{sorted_mat()}, containing \code{sorted_test_matrix} and
#'   \code{row_pro_mat_sorted}.
#' @param task Character. Type of task: "classification" or "regression".
#' @param show Character. Which subset: "all", "train" or "test".
#' @param type_cols Named vector of colors for feature type categories.
#' @param label_cols Named vector or function of colors for class label categories.
#' @param prop_cols Function. Color mapping function for membership proportion.
#' @param col_mat Function. Color mapping function for raw data values.
#' @param col_Col_Proximity Function. Color mapping function for column proximity.
#' @param col_Row_Proximity Function. Color mapping function for row proximity.
#' @param direction Character. Layout direction of packed legends, either "vertical" or "horizontal".
#'
#' @return A \code{ComplexHeatmap} packed Legend object containing all specified legends.
#'
#' @export
#' @keywords internal
generate_legend_bundle <- function(sorted_dat,
                                   task = c("classification", "regression"),
                                   show = c("all", "train", "test"),
                                   type_cols = NULL,
                                   label_cols,
                                   prop_cols = NULL,
                                   col_mat,
                                   col_Col_Proximity = NULL,
                                   col_Row_Proximity = NULL,
                                   direction = c("vertical", "horizontal")) {
  direction <- match.arg(direction)
  task <- match.arg(task)

  dat <- sorted_dat$dat_sorted
  sorted_test_matrix <- sorted_dat$sorted_test_matrix

  feat <- intersect(colnames(dat), colnames(sorted_test_matrix))
  numeric_feats <- feat[sapply(dat[feat], is.numeric)]
  categorical_feats <- feat[sapply(dat[feat], function(x)
    is.factor(x) || is.character(x))]

  mat_cont <- sorted_test_matrix[, numeric_feats, drop = FALSE]
  storage.mode(mat_cont) <- "numeric"

  row_pro_mat_sorted <- sorted_dat$row_pro_mat_sorted

  default_defs <- list()

  if (!is.null(type_cols) && show == "all") {
    default_defs$Type <- list(
      at = names(type_cols),
      legend_gp = gpar(fill = type_cols),
      title = "Type",
      ncol = length(type_cols)
    )
  }


  if (task == "classification") {
    default_defs$Class <- list(
      at = names(label_cols),
      legend_gp = gpar(fill = label_cols),
      title = "Class Labels",
      ncol = length(label_cols)
    )
    if (!is.null(prop_cols)) {
      default_defs$Membership <- list(title = "Class Membership",
                                      col_fun = prop_cols,
                                      at = c(0, 0.5, 1))
    }
  }


  if (task == "regression") {
    if (inherits(label_cols, "function")) {
      Predicted <- dat$y_hat
      True <- dat$target
      min_val <- min(c(Predicted, True), na.rm = TRUE)
      max_val <- max(c(Predicted, True), na.rm = TRUE)
      mid_val <- round((min_val + max_val) / 2, 1)

      default_defs$True <- list(
        title = "True/Predicted Value",
        col_fun = label_cols,
        at = c(round(min_val, 0), mid_val, round(max_val, 0))
      )
    }
  }

  # Raw values legend (always needed)
  default_defs$RawValues <- list(
    title = "Raw Data Values",
    col_fun = col_mat,
    at = c(round(min(mat_cont, na.rm = TRUE), 0), round((
      min(mat_cont, na.rm = TRUE) + max(mat_cont, na.rm = TRUE)
    ) / 2, 1), round(max(mat_cont, na.rm = TRUE), 0))
  )

  # Proximity legends
  if (!is.null(col_Col_Proximity)) {
    default_defs$ColProximity <- list(title   = "Proximity for Columns",
                                      col_fun = col_Col_Proximity,
                                      at      = c(-1, 0, 1))
  }

  if (!is.null(col_Row_Proximity)) {
    default_defs$RowProximity <- list(
      title = "Proximity for Rows",
      col_fun = col_Row_Proximity,
      at = c(round(
        min(row_pro_mat_sorted, na.rm = TRUE), 0
      ), round((
        min(row_pro_mat_sorted, na.rm = TRUE) + max(row_pro_mat_sorted, na.rm = TRUE)
      ) / 2, 1), round(
        max(row_pro_mat_sorted, na.rm = TRUE), 0
      ))
    )
  }

  common_legend_params <- list(
    legend_width = unit(1.5, "cm"),
    direction = "horizontal",
    title_position = "topleft",
    border = "black",
    title_gp = gpar(fontsize = 7, fontface = "bold"),
    labels_gp = gpar(fontsize = 7)
  )

  indiv <- lapply(default_defs, function(def) {
    args <- modifyList(common_legend_params, def)
    do.call(ComplexHeatmap::Legend, args)
  })

  legend_bundle <- do.call(ComplexHeatmap::packLegend, c(
    indiv,
    list(
      direction = direction,
      column_gap = unit(3, "cm"),
      row_gap = unit(3, "mm")
    )
  ))

  return(legend_bundle)
}
