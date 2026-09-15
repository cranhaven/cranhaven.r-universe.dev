#' Draw Main Heatmap with Annotations
#'
#' @description
#' Combines sorted data matrix and provided annotations into a single Heatmap.
#'
#' @param sorted_dat List from \code{sorted_mat()}, containing \code{sorted_test_matrix}.
#' @param split_vec Factor defining row splits in the heatmap.
#' @param pred_ha A \code{rowAnnotation} for predictions (from \code{prediction_annotation}).
#' @param row_prop_ha A \code{rowAnnotation} for row proximity (from \code{row_prop_anno}).
#' @param layout List with \code{row_h} and \code{col_h} in mm.
#' @param palette RColorBrewer palette for heatmap values (default "Blues").
#' @param n_colors Number of colors for heatmap (default 9).
#' @param show_row_names Logical. Whether to display row names in the heatmap (default TRUE).
#' @param row_names_gp \code{gpar} settings for row name font (default fontsize=5).
#' @param show_row_prox Logical, whether to show the right annotation for row proximity.
#' @param raw_value_col User-defined colors for raw data values.
#'
#' @return A configured \code{Heatmap} object.
#'
#' @export
#' @keywords internal

make_main_heatmap <- function(sorted_dat,
                              split_vec,
                              pred_ha,
                              row_prop_ha,
                              layout,
                              palette = "Blues",
                              n_colors = 9,
                              show_row_names = TRUE,
                              row_names_gp = gpar(fontsize = 5),
                              show_row_prox = TRUE,
                              raw_value_col = NULL) {
  dat <- sorted_dat$dat_sorted
  sorted_test_matrix <- sorted_dat$sorted_test_matrix

  feat <- intersect(colnames(dat), colnames(sorted_test_matrix))
  numeric_feats <- feat[sapply(dat[feat], is.numeric)]
  categorical_feats <- feat[sapply(dat[feat], function(x)
    is.factor(x) || is.character(x))]

  mat_cont <- sorted_test_matrix[, intersect(colnames(sorted_test_matrix), numeric_feats), drop = FALSE]
  storage.mode(mat_cont) <- "numeric"

  pal_colors <- brewer.pal(n_colors, palette)

  if (!is.null(raw_value_col)) {
    cols <- raw_value_col
    stops <- seq(min(mat_cont, na.rm = TRUE),
                 max(mat_cont, na.rm = TRUE),
                 length.out = length(cols))
    col_mat <- colorRamp2(stops, cols)
  } else {
    col_mat <- colorRamp2(seq(
      min(mat_cont, na.rm = TRUE),
      max(mat_cont, na.rm = TRUE),
      length.out = n_colors
    ), pal_colors)
  }
  row_h <- layout$row_h
  col_h <- layout$col_h

  right_anno <- if (show_row_prox)
    row_prop_ha
  else
    NULL
  ht <- Heatmap(
    mat_cont,
    col = col_mat,
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    split = split_vec,
    row_title = NULL,
    show_row_dend = FALSE,
    show_column_dend = FALSE,
    show_row_names = show_row_names,
    row_names_gp = row_names_gp,
    show_column_names = FALSE,
    column_names_side = "top",
    border = TRUE,
    show_heatmap_legend = FALSE,
    left_annotation = pred_ha,
    right_annotation = right_anno,
    width = unit(col_h, "mm"),
    height = unit(row_h, "mm")
  )

  return(list(heatmap = ht, palettes = list(col_mat = col_mat)))
}

#' Create Column Heatmap with Variable Importance
#'
#' @description
#' Constructs a ComplexHeatmap object displaying feature-feature correlations
#' with optional variable importance barplots and split-variable highlighting.
#'
#' @param fit A fitted partykit tree object used to extract split variables.
#' @param sorted_dat List from \code{sorted_mat()}
#' @param var_imp Named numeric vector of variable importance scores.
#' @param layout List with layout dimensions
#' @param include_var_imp Logical; include importance barplot if TRUE (default TRUE).
#' @param col_var_imp Color for importance bars (default "orange").
#' @param var_bar_width Numeric width of bars (default 0.8).
#' @param var_fontsize Font size for importance text (default 5).
#' @param split_var_bg Background color for split variable names (default "darkgreen").
#' @param split_var_fontsize Font size for split variable names (default 5).
#' @param palette RColorBrewer palette for correlation heatmap (default "RdBu").
#' @param n_colors Number of colors in correlation scale (default 11).
#' @param show_col_prox Logical, whether to show column proximity.
#'
#' @return A \code{Heatmap} object from ComplexHeatmap.
#'
#' @export
#' @keywords internal

col_ht <- function(fit,
                   sorted_dat,
                   var_imp,
                   layout,
                   include_var_imp = TRUE,
                   col_var_imp = "orange",
                   var_bar_width = 0.8,
                   var_fontsize = 5,
                   split_var_bg = "darkgreen",
                   split_var_fontsize = 5,
                   palette = "RdBu",
                   n_colors = 11,
                   show_col_prox = TRUE) {
  if (is.null(sorted_dat$col_pro_mat_sorted)) {
    return(list(heatmap = NULL, palettes = list()))
  }

  col_h <- layout$col_h
  col_pro_mat_sorted <- sorted_dat$col_pro_mat_sorted

  if (include_var_imp) {
    features <- sorted_dat$sorted_col_names
    ordered_importance <- setNames(ifelse(features %in% names(var_imp), var_imp[features], 0), features)
    selected_var_imp <- features %in% names(var_imp)
    fontsize_var <- ifelse(selected_var_imp, var_fontsize, 0)
    bgcolor_var <- ifelse(selected_var_imp, col_var_imp, "transparent")

    var_ha <- rowAnnotation(
      Importance = anno_barplot(
        ordered_importance,
        bar_width = var_bar_width,
        gp        = gpar(fill = bgcolor_var, col = "white"),
        border    = FALSE,
        axis      = FALSE,
        width     = unit(0.5, "mm")
      ),
      `%` = anno_text(
        sprintf("%.0f%%", ordered_importance * 100),
        gp    = gpar(fontsize = fontsize_var),
        just  = "left"
      ),
      show_annotation_name = FALSE
    )
  } else {
    var_ha <- NULL
  }


  split_var <- unique(na.omit(ggparty(fit)$data$splitvar))
  features <- sorted_dat$sorted_col_names
  fontcolors <- ifelse(features %in% split_var, "white", "black")
  fontfaces <- ifelse(features %in% split_var, "bold", "plain")
  fontbg <- ifelse(features %in% split_var, split_var_bg, "transparent")


  if (!palette %in% rownames(brewer.pal.info))
    stop("Palette not found: ", palette)
  pal_colors <- rev(brewer.pal(n_colors, palette)) # red : Positive correlation / blue : Negative correlation
  col_Col_Proximity <- colorRamp2(seq(-1, 1, length.out = n_colors), pal_colors)
  palettes <- list()
  if (show_col_prox) {
    palettes$col_Col_Proximity <- col_Col_Proximity
  }

  ht <- Heatmap(
    col_pro_mat_sorted,
    name = "Col Proximity",
    col = col_Col_Proximity,
    row_title = NULL,
    column_title = NULL,
    show_row_names = TRUE,
    row_names_side = "left",
    row_names_gp = gpar(
      fill = fontbg,
      fontsize = split_var_fontsize,
      fontface = fontfaces,
      col = fontcolors
    ),
    show_column_names = FALSE,
    show_column_dend = FALSE,
    show_row_dend = FALSE,
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    right_annotation = var_ha,
    border = TRUE,
    show_heatmap_legend = FALSE,
    width = unit(col_h, "mm"),
    height = unit(col_h, "mm")
  )

  return(list(heatmap = ht, palettes = palettes))
}


#' Annotate Row Proximity on Heatmap
#'
#' @description
#' Creates a ComplexHeatmap row annotation showing supervised proximity
#' for each data sample, grouped by a split vector.
#'
#' @param sorted_dat List from \code{sorted_mat()}
#' @param layout List with layout dimensions
#' @param split_vec Factor dividing columns of the proximity matrix into groups.
#' @param palette RColorBrewer palette name for color scale (default "OrRd").
#' @param n_colors Number of discrete colors to generate (default 9).
#' @param border Logical; draw cell borders if TRUE (default TRUE).
#' @param gap_mm Unit object for gap between annotation blocks (default \code{unit(1, "mm")} ).
#' @param show_row_prox Logical, whether to show row proximity.
#'
#' @return A \code{rowAnnotation} object for use in a ComplexHeatmap.
#'
#' @export
#' @keywords internal

row_prop_anno <- function(sorted_dat,
                          layout,
                          split_vec,
                          palette = "Spectral",
                          n_colors = 11,
                          border = TRUE,
                          gap_mm = unit(1, "mm"),
                          show_row_prox = TRUE) {
  if (is.null(sorted_dat$row_pro_mat_sorted)) {
    return(list(annotation = NULL, palettes = list()))
  }
  R_mat <- sorted_dat$row_pro_mat_sorted
  row_h <- layout$row_h


  split_list <- split(seq_len(ncol(R_mat)), split_vec)
  split_matrices <- lapply(split_list, function(cols)
    R_mat[, cols, drop = FALSE])

  if (!palette %in% rownames(brewer.pal.info))
    stop("Palette not found: ", palette)
  pal_colors <- rev(brewer.pal(n_colors, palette))

  col_Row_Proximity <- colorRamp2(seq(min(R_mat, na.rm = TRUE), max(R_mat, na.rm = TRUE), length.out = n_colors), pal_colors)

  palettes <- list()
  if (show_row_prox) {
    palettes$col_Row_Proximity <- col_Row_Proximity
  }
  ann_list <- lapply(names(split_matrices), function(g) {
    anno_simple(
      split_matrices[[g]],
      col    = col_Row_Proximity,
      border = border,
      which  = "row"
    )
  })
  names(ann_list) <- names(split_matrices)


  ann_width <- sapply(split_matrices, function(sub_mat) {
    (row_h) * ncol(sub_mat) / ncol(R_mat)
  })


  ra <- do.call(rowAnnotation, c(
    ann_list,
    list(
      annotation_width       = unit(ann_width, "mm"),
      show_annotation_name   = FALSE,
      gap                    = gap_mm
    )
  ))

  return(list(annotation = ra, palettes = palettes))
}


#' Annotate Predsictions Information
#'
#' @description
#' Creates row annotations showing predicted vs true class labels and probabilities,
#' with optional data_type coloring.
#'
#' @param sorted_dat List from \code{sorted_mat()}, containing \code{dat_sorted} with predictions.
#' @param target_lab Name of true label column in \code{dat_sorted}.
#' @param task Character. Type of task: "classification" or "regression".
#' @param label_map Optional named vector to map raw labels to new labels.
#' @param label_map_colors Optional named vector of colors for mapped labels.
#' @param type_palette RColorBrewer palette for data_type (default "Dark2").
#' @param label_palette Function or vector of colors for true and predicted value (default OrRd).
#' @param n_label_color Number of colors for probability palette (default 9).
#' @param prop_palette Function or vector of colors for probability gradient (default gray).
#' @param n_prop_colors Number of colors for probability palette (default 11).
#' @param gap_mm Unit for gap between annotations (default \code{unit(1, "mm")} ).
#'
#' @return A \code{rowAnnotation} object for predictions and truth.
#'
#' @export
#' @keywords internal

prediction_annotation <- function(sorted_dat,
                                  target_lab,
                                  task = c("classification", "regression"),
                                  label_map = NULL,
                                  label_map_colors = NULL,
                                  type_palette = "Dark2",
                                  label_palette = "OrRd",
                                  n_label_color = 9,
                                  prop_palette = gray,
                                  n_prop_colors = 11,
                                  gap_mm = unit(1, "mm")) {
  task <- match.arg(task)

  dat_sorted <- sorted_dat$dat_sorted
  raw_pred <- dat_sorted[["y_hat"]]
  raw_true <- dat_sorted[[target_lab]]

  ann_cols <- list()
  palettes <- list()

  if ("data_type" %in% names(dat_sorted)) {
    dtypes <- unique(dat_sorted$data_type)
    if (!type_palette %in% rownames(RColorBrewer::brewer.pal.info)) {
      stop("Palette '", type_palette, "' not found in RColorBrewer")
    }
    type_cols <- RColorBrewer::brewer.pal(8, type_palette)[1:length(dtypes)]
    names(type_cols) <- dtypes
    ann_cols$Type <- type_cols
    palettes$type_cols <- type_cols
  }

  if (task == "classification") {
    dat_sorted$Predicted <- if (!is.null(label_map)) {
      label_map[as.character(raw_pred)]
    } else {
      as.character(raw_pred)
    }
    dat_sorted$True <- if (!is.null(label_map)) {
      label_map[as.character(raw_true)]
    } else {
      as.character(raw_true)
    }

    lvls <- unique(dat_sorted$True)
    if (!is.null(label_map_colors) &&
        all(lvls %in% names(label_map_colors))) {
      label_cols <- label_map_colors[lvls]
    } else {
      label_cols <- RColorBrewer::brewer.pal(9, "Set1")[1:length(lvls)]
      names(label_cols) <- lvls
    }
    palettes$label_cols <- label_cols

    ann_cols$Predicted <- label_cols
    ann_cols$True <- label_cols


    prob_cols <- unique(as.character(raw_pred))
    prob_raw <- as.matrix(dat_sorted[, prob_cols, drop = FALSE])
    rownames(prob_raw) <- dat_sorted$Sample
    if (!is.null(label_map)) {
      colnames(prob_raw) <- label_map[colnames(prob_raw)]
    }

    vals <- seq(0, 1, length.out = n_prop_colors)
    prop_cols <- colorRamp2(vals, rev(prop_palette(vals)))
    ann_cols$Prob <- prop_cols
    palettes$prop_cols <- prop_cols
  } else if (task == "regression") {
    dat_sorted$Predicted <- raw_pred
    dat_sorted$True <- raw_true

    min_val <- min(c(raw_pred, raw_true), na.rm = TRUE)
    max_val <- max(c(raw_pred, raw_true), na.rm = TRUE)

    pal_colors <- brewer.pal(n_label_color, label_palette)
    label_cols <- colorRamp2(seq(min_val, max_val, length.out = n_label_color),
                             pal_colors)

    ann_cols$Predicted <- label_cols
    ann_cols$True <- label_cols

    palettes$label_cols <- label_cols
  }

  ra <- rowAnnotation(
    Type = if ("data_type" %in% names(dat_sorted))
      dat_sorted$data_type
    else
      NULL,
    Predicted = dat_sorted$Predicted,
    True = dat_sorted$True,
    Prob = if (task == "classification")
      prob_raw
    else
      NULL,
    col = ann_cols,
    gap = gap_mm,
    show_annotation_name = if (task == "classification")
      c(Prob = FALSE)
    else
      NULL,
    annotation_name_gp = gpar(fontsize = 5, fontface = "bold"),
    annotation_name_side = "top",
    show_legend = FALSE,
    border = TRUE
  )

  return(list(annotation = ra, palettes = palettes))
}
