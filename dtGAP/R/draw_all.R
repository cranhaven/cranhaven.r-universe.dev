#' Draw Tree Panel in Current Viewport
#'
#' Renders tree branches, branch labels, and node labels inside the currently
#' active grid viewport. Extracted from \code{draw_all()} for reuse by
#' \code{compare_dtGAP()}.
#'
#' @param prepare_tree A list with \code{plot_data}, \code{branches}, and
#'   \code{branch_labels} (output of \code{prepare_tree()}).
#' @param tree_w Numeric. Width of the tree viewport in mm.
#' @param tree_h Numeric. Height of the tree viewport in mm.
#'
#' @noRd
draw_tree_panel <- function(prepare_tree, tree_w, tree_h) {
  plot_data <- prepare_tree$plot_data
  branches <- prepare_tree$branches
  branch_labels <- prepare_tree$branch_labels

  for (i in seq_len(nrow(branches))) {
    x0 <- branches$x_start[i] * tree_w
    y0 <- branches$y_start[i] * tree_h
    x1 <- branches$x_end[i] * tree_w
    y1 <- branches$y_end[i] * tree_h

    grid.segments(
      x0 = unit(x0, "mm"), y0 = unit(y0, "mm"),
      x1 = unit(x0, "mm"), y1 = unit(y1, "mm"),
      gp = gpar(col = "black")
    )
    grid.segments(
      x0 = unit(x0, "mm"), y0 = unit(y1, "mm"),
      x1 = unit(x1, "mm"), y1 = unit(y1, "mm"),
      gp = gpar(col = "black")
    )
  }

  for (i in seq_len(nrow(branch_labels))) {
    x <- branch_labels$x_par[i] * tree_w
    y <- branch_labels$y[i] * tree_h
    lab <- branch_labels$breaks_clean[i]

    if (!is.na(lab) && lab != "NA") {
      tg <- textGrob(label = lab, gp = gpar(fontsize = 7))
      w <- convertWidth(grobWidth(tg) + unit(1, "mm"), "mm", valueOnly = TRUE)
      h <- convertHeight(grobHeight(tg) + unit(1, "mm"), "mm", valueOnly = TRUE)

      grid.rect(
        x = unit(x, "mm"), y = unit(y, "mm"),
        width = unit(w, "mm"), height = unit(h, "mm"),
        gp = gpar(fill = "white", col = NA)
      )
      grid.text(
        label = lab, x = unit(x, "mm"), y = unit(y, "mm"),
        just = "center", gp = gpar(col = "gray20", fontsize = 7)
      )
    }
  }

  for (i in seq_len(nrow(plot_data))) {
    x0 <- plot_data$x[i] * tree_w
    y0 <- plot_data$y[i] * tree_h
    lab <- plot_data$node_label[i]

    tg <- textGrob(lab, gp = gpar(fontsize = 8))
    w <- convertWidth(grobWidth(tg) + unit(2, "mm"), "mm", valueOnly = TRUE)
    h <- convertHeight(grobHeight(tg) + unit(1, "mm"), "mm", valueOnly = TRUE)

    grid.rect(
      x = unit(x0, "mm"), y = unit(y0, "mm"),
      width = unit(w, "mm"), height = unit(h, "mm"),
      just = "centre", gp = gpar(fill = "white", col = "black")
    )
    grid.text(
      label = lab, x = unit(x0, "mm"), y = unit(y0, "mm"),
      just = "centre", gp = gpar(col = "black", fontsize = 8)
    )
  }
}


#' Draw Full Visualization: Decision Tree with Heatmap and Evaluation
#'
#' This function creates a full-page layout consisting of a decision tree plot,
#' a heatmap, and optional evaluation results. It is designed for use in
#' reporting classification or clustering trees with additional visual indicators.
#'
#' @param prepare_tree A list returned from a tree preparation function,
#'   containing `plot_data` and `branches` for tree structure.
#' @param heat A `grob` object representing the heatmap visualization. Usually generated with `grid.grabExpr(draw(...))`
#' @param total_w Total width of the drawing in mm. Default is 297 (A4 landscape width).
#' @param total_h Total height of the drawing in mm. Default is 210 (A4 landscape height).
#' @param layout A list specifying layout parameters: `tree_w`, `tree_h`, `margin`, and `offset_h`.
#' @param x_eval_start X-axis starting position (in mm) for evaluation text. Default is 15.
#' @param y_eval_start Y-axis starting position (in mm) for evaluation text. If NULL, it will be computed automatically.
#' @param eval_text Font size for the evaluation text. Default is 6.
#' @param eval_res A list with evaluation result text from `eval_tree()` (including `data_info`, `train_metrics`, and `test_metrics`).
#' @param print_eval Logical, whether to show evaluation results. Default is TRUE.
#' @param show_row_prox Logical, whether to show row proximity.
#' @param show_col_prox Logical, whether to show column proximity.
#'
#' @return Draws the full visualization to the current graphics device.
#'   Called for its side effect; returns \code{invisible(NULL)}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # See dtGAP() for a full end-to-end example
#' # that internally calls draw_all().
#' }
draw_all <- function(prepare_tree,
                     heat,
                     total_w = 297,
                     total_h = 210,
                     layout,
                     x_eval_start = 15,
                     y_eval_start = NULL,
                     eval_text = 7,
                     eval_res = NULL,
                     print_eval = TRUE,
                     show_col_prox = TRUE,
                     show_row_prox = TRUE) {
  plot_data <- prepare_tree$plot_data
  tree_w <- layout$tree_w
  tree_h <- layout$tree_h
  total_draw_h <- layout$total_draw_h
  col_h <- layout$col_h
  margin <- layout$margin
  offset_h <- layout$offset_h

  if (is.null(y_eval_start)) {
    root_node <- plot_data$y[is.na(plot_data$parent)]
    root_node_y <- root_node * tree_h

    if (root_node_y < 100) {
      # tree is short, place near top margin
      y_eval_start <- total_h - 10
    } else {
      # place 15 mm above that node
      y_eval_start <- margin / 2 + root_node_y - 10
    }
  }


  grid.newpage()

  pushViewport(viewport(
    width  = unit(total_w, "mm"),
    height = unit(total_h, "mm"),
    x      = unit(0, "mm"),
    y      = unit(0, "mm"),
    just   = c("left", "bottom"),
    name   = "page"
  ))

  pushViewport(viewport(
    x      = unit(20, "mm"),
    y      = unit(margin / 2, "mm"),
    width  = unit(tree_w, "mm"),
    height = unit(tree_h, "mm"),
    just   = c("left", "bottom"),
    name   = "tree_area"
  ))

  draw_tree_panel(prepare_tree, tree_w, tree_h)

  upViewport()
  heatmap_x <- if (show_row_prox)
    unit(tree_w + 20, "mm")
  else
    unit(tree_w / 2 - 10, "mm")
  heatmap_y <- if (show_col_prox)
    unit((offset_h / 2), "mm")
  else
    unit(margin / 2, "mm")
  heatmap_h <- if (show_col_prox)
    unit(total_h, "mm")
  else
    unit(total_h - col_h, "mm")


  pushViewport(
    viewport(
      x      = heatmap_x,
      y      = heatmap_y,
      height = heatmap_h,
      just   = c("left", "bottom"),
      name   = "heatmap_area"
    )
  )

  grid.draw(heat)

  upViewport()
  seekViewport("page")
  if (print_eval && !is.null(eval_res)) {
    draw_indicator(
      eval_res$data_info,
      x_eval = x_eval_start,
      y_eval = y_eval_start,
      size = eval_text,
      just = c("left", "top")
    )
    if (!is.null(eval_res$train_metrics)) {
      draw_indicator(
        eval_res$train_metrics,
        x_eval = x_eval_start,
        y_eval = (y_eval_start - 10),
        size = eval_text,
        just = c("left", "top")
      )
    }
    if (!is.null(eval_res$test_metrics) &&
        is.null(eval_res$train_metrics)) {
      draw_indicator(
        eval_res$test_metrics,
        x_eval = x_eval_start,
        y_eval = (y_eval_start - 10),
        size = eval_text,
        just = c("left", "top")
      )
    }
    if (!is.null(eval_res$test_metrics) &&
        !is.null(eval_res$train_metrics)) {
      draw_indicator(
        eval_res$test_metrics,
        (x_eval <- x_eval_start + 45),
        y_eval = (y_eval_start - 10),
        size = eval_text,
        just = c("left", "top")
      )
    }
  }
  upViewport()
}
