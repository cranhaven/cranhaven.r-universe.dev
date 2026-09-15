#' Visualize a Single Tree from a Conditional Random Forest
#'
#' Fits a \code{partykit::cforest} and visualizes one of its individual trees
#' using the full dtGAP pipeline (decision tree + heatmap + evaluation).
#'
#' @param x Character. Name or label of the dataset.
#' @param target_lab Character. Name of the target column.
#' @param show Character. Which subset to show: \code{"all"}, \code{"train"},
#'   or \code{"test"}.
#' @param tree_index Integer. Which tree to extract (1-based). Default is 1.
#' @param ntree Integer. Number of trees in the forest (default 500).
#' @param mtry Integer or NULL. Number of variables randomly sampled at each
#'   split. If NULL, uses the \code{cforest} default.
#' @param rf_control A \code{ctree_control} object or NULL.
#' @param data_train Data frame. Training data.
#' @param data_test Data frame. Test data.
#' @param data_all Data frame. Full dataset.
#' @param test_size Numeric. Proportion for test split (default 0.3).
#' @param task Character. \code{"classification"} or \code{"regression"}.
#' @param trans_type Character. Transformation type.
#' @param col_proximity Character. Correlation method.
#' @param linkage_method Character. Linkage method.
#' @param seriate_method Character. Seriation method.
#' @param cRGAR_w Integer. Window size for RGAR.
#' @param sort_by_data_type Logical. Preserve data_type grouping.
#' @param custom_layout Optional custom node positions.
#' @param panel_space Numeric. Vertical spacing.
#' @param margin Numeric. Margin in mm.
#' @param total_w Numeric. Page width in mm.
#' @param total_h Numeric. Page height in mm.
#' @param tree_p Numeric. Tree panel proportion.
#' @param include_var_imp Logical. Show importance barplot.
#' @param col_var_imp Color for importance bars.
#' @param var_imp_bar_width Numeric. Bar width.
#' @param var_imp_fontsize Numeric. Font size for importance.
#' @param split_var_bg Background for split variable names.
#' @param split_var_fontsize Font size for split variable names.
#' @param Col_Prox_palette Palette for correlation heatmap.
#' @param Col_Prox_n_colors Number of correlation colors.
#' @param label_map Named vector for label mapping.
#' @param label_map_colors Named vector of mapped label colors.
#' @param type_palette Palette for data_type.
#' @param label_palette Palette for labels.
#' @param n_label_color Number of label colors.
#' @param pred_ha_gap Gap between annotations.
#' @param prop_palette Probability gradient palette.
#' @param n_prop_colors Number of probability colors.
#' @param Row_Prox_palette Palette for row proximity.
#' @param Row_Prox_n_colors Number of row proximity colors.
#' @param row_border Draw cell borders.
#' @param row_gap Gap between annotation blocks.
#' @param sorted_dat_palette Palette for heatmap.
#' @param sorted_dat_n_colors Number of heatmap colors.
#' @param show_row_names Show row names.
#' @param row_names_gp Font settings for row names.
#' @param show_row_prox Show row proximity.
#' @param show_col_prox Show column proximity.
#' @param raw_value_col Colors for raw data values.
#' @param lgd_direction Legend direction.
#' @param x_eval_start Eval text x position.
#' @param y_eval_start Eval text y position.
#' @param eval_text Eval text font size.
#' @param print_eval Show evaluation results.
#' @param simple_metrics Use simple metrics.
#'
#' @return Draws the dtGAP visualization for the selected tree to the current
#'   graphics device. Called for its side effect; returns invisibly.
#'
#' @export
#'
#' @examples
#' \donttest{
#' rf_dtGAP(
#'   data_train = train_covid,
#'   data_test = test_covid,
#'   target_lab = "Outcome",
#'   show = "test",
#'   tree_index = 1,
#'   ntree = 50,
#'   print_eval = FALSE
#' )
#' }
rf_dtGAP <- function(x = NULL,
                     target_lab = NULL,
                     show = c("all", "train", "test"),
                     tree_index = 1L,
                     ntree = 500L,
                     mtry = NULL,
                     rf_control = NULL,
                     data_train = NULL,
                     data_test = NULL,
                     data_all = NULL,
                     test_size = 0.3,
                     task = c("classification", "regression"),
                     trans_type = c("normalize", "scale", "percentize", "none"),
                     col_proximity = c("pearson", "spearman", "kendall"),
                     linkage_method = c("CT", "SG", "CP"),
                     seriate_method = "TSP",
                     cRGAR_w = 5,
                     sort_by_data_type = TRUE,
                     custom_layout = NULL,
                     panel_space = 0.001,
                     margin = 20,
                     total_w = 297,
                     total_h = 210,
                     tree_p = 0.3,
                     include_var_imp = TRUE,
                     col_var_imp = "orange",
                     var_imp_bar_width = 0.8,
                     var_imp_fontsize = 5,
                     split_var_bg = "darkgreen",
                     split_var_fontsize = 5,
                     Col_Prox_palette = "RdBu",
                     Col_Prox_n_colors = 11,
                     label_map = NULL,
                     label_map_colors = NULL,
                     type_palette = "Dark2",
                     label_palette = "OrRd",
                     n_label_color = 9,
                     pred_ha_gap = unit(1, "mm"),
                     prop_palette = gray,
                     n_prop_colors = 11,
                     Row_Prox_palette = "Spectral",
                     Row_Prox_n_colors = 11,
                     row_border = TRUE,
                     row_gap = unit(1, "mm"),
                     sorted_dat_palette = "Blues",
                     sorted_dat_n_colors = 9,
                     show_row_names = TRUE,
                     row_names_gp = gpar(fontsize = 5),
                     show_row_prox = TRUE,
                     show_col_prox = TRUE,
                     raw_value_col = NULL,
                     lgd_direction = c("vertical", "horizontal"),
                     x_eval_start = 15,
                     y_eval_start = NULL,
                     eval_text = 7,
                     print_eval = TRUE,
                     simple_metrics = FALSE) {
  show <- match.arg(show)
  task <- match.arg(task)
  trans_type <- match.arg(trans_type)
  col_proximity <- match.arg(col_proximity)
  linkage_method <- match.arg(linkage_method)
  lgd_direction <- match.arg(lgd_direction)

  if (tree_index < 1 || tree_index > ntree) {
    stop("`tree_index` must be between 1 and `ntree` (", ntree, ").")
  }

  if (is.null(x)) {
    if (is.null(data_all)) {
      x <- deparse(substitute(data_train))
    } else {
      x <- deparse(substitute(data_all))
    }
    x <- gsub("train|test", "", x, ignore.case = TRUE)
    x <- trimws(x, which = "both", whitespace = "_")
  }
  x <- paste0(x, " (Tree ", tree_index, "/", ntree, ")")

  # --- Data prep ---
  data_all_prep <- add_data_type(data_train, data_test, data_all, test_size)
  data_all_prep <- prepare_features(data_all_prep, target_lab, task)

  if (is.null(data_train)) {
    data_train <- data_all_prep %>%
      dplyr::filter(data_type == "train") %>%
      dplyr::select(-data_type)
  }
  if (is.null(data_test)) {
    data_test <- data_all_prep %>%
      dplyr::filter(data_type == "test") %>%
      dplyr::select(-data_type)
  }

  data <- switch(show,
    all   = data_all_prep,
    train = data_train,
    test  = data_test
  )

  # --- Train forest + extract single tree ---
  rf_result <- train_rf(
    data_train = data_train,
    target_lab = target_lab,
    task = task,
    ntree = ntree,
    mtry = mtry,
    control = rf_control
  )
  fit <- partykit::gettree(rf_result$forest, tree = tree_index)
  var_imp <- rf_result$var_imp
  model <- "cforest"

  # --- Standard dtGAP pipeline ---
  tree_res <- compute_tree(
    fit = fit, model = model, show = show,
    data = data, target_lab = target_lab,
    task = task, custom_layout = custom_layout,
    panel_space = panel_space
  )

  sorted_dat <- sorted_mat(
    tree_res = tree_res, target_lab = target_lab,
    show = show, trans_type = trans_type,
    col_proximity = col_proximity,
    linkage_method = linkage_method,
    seriate_method = seriate_method,
    w = cRGAR_w, sort_by_data_type = sort_by_data_type
  )

  layout <- compute_layout(
    sorted_dat = sorted_dat, margin = margin,
    total_w = total_w, total_h = total_h, tree_p = tree_p
  )

  col_ht_res <- col_ht(
    fit = fit, sorted_dat = sorted_dat, var_imp = var_imp,
    layout = layout, include_var_imp = include_var_imp,
    col_var_imp = col_var_imp, var_bar_width = var_imp_bar_width,
    var_fontsize = var_imp_fontsize, split_var_bg = split_var_bg,
    split_var_fontsize = split_var_fontsize,
    palette = Col_Prox_palette, n_colors = Col_Prox_n_colors,
    show_col_prox = show_col_prox
  )

  split_vec <- get_split_vec(sorted_dat = sorted_dat, tree_res = tree_res)
  pred_ha_res <- prediction_annotation(
    sorted_dat = sorted_dat, target_lab = target_lab,
    task = task, label_map = label_map,
    label_map_colors = label_map_colors,
    type_palette = type_palette, label_palette = label_palette,
    n_label_color = n_label_color, prop_palette = prop_palette,
    n_prop_colors = n_prop_colors, gap_mm = pred_ha_gap
  )

  row_prop_ha_res <- row_prop_anno(
    sorted_dat = sorted_dat, layout = layout,
    split_vec = split_vec, palette = Row_Prox_palette,
    n_colors = Row_Prox_n_colors, border = row_border,
    gap_mm = row_gap, show_row_prox = show_row_prox
  )
  main_ht_res <- make_main_heatmap(
    sorted_dat = sorted_dat, split_vec = split_vec,
    pred_ha = pred_ha_res$annotation,
    row_prop_ha = row_prop_ha_res$annotation,
    layout = layout, palette = sorted_dat_palette,
    n_colors = sorted_dat_n_colors,
    show_row_names = show_row_names,
    row_names_gp = row_names_gp,
    show_row_prox = show_row_prox,
    raw_value_col = raw_value_col
  )

  pals <- c(
    col_ht_res$palettes, pred_ha_res$palettes,
    row_prop_ha_res$palettes, main_ht_res$palettes
  )
  if (!show_row_prox) pals$col_Row_Proximity <- NULL
  if (!show_col_prox) pals$col_Col_Proximity <- NULL

  legends <- generate_legend_bundle(
    sorted_dat = sorted_dat, task = task, show = show,
    type_cols = pals$type_cols, label_cols = pals$label_cols,
    prop_cols = pals$prop_cols, col_mat = pals$col_mat,
    col_Col_Proximity = pals$col_Col_Proximity,
    col_Row_Proximity = pals$col_Row_Proximity,
    direction = lgd_direction
  )

  combined_heatmap <- if (show_col_prox)
    col_ht_res$heatmap %v% main_ht_res$heatmap
  else
    main_ht_res$heatmap

  heat <- grid.grabExpr(
    draw(combined_heatmap, heatmap_legend_list = legends,
         auto_adjust = FALSE, ht_gap = unit(0, "mm"), newpage = FALSE)
  )

  tree_viz <- prepare_tree(tree_res = tree_res, model = model)
  eval_res <- eval_tree(
    x = x, fit = fit, task = task, tree_res = tree_res,
    target_lab = target_lab, sorted_dat = sorted_dat,
    show = show, model = model, col_proximity = col_proximity,
    linkage_method = linkage_method, seriate_method = seriate_method,
    simple_metrics = simple_metrics
  )
  draw_all(
    prepare_tree = tree_viz, heat = heat,
    total_w = total_w, total_h = total_h, layout = layout,
    x_eval_start = x_eval_start, y_eval_start = y_eval_start,
    eval_text = eval_text, eval_res = eval_res,
    print_eval = print_eval, show_col_prox = show_col_prox,
    show_row_prox = show_row_prox
  )
}
