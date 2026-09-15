#' Decision Tree Generalized Association Plots (dtGAP)
#'
#'
#' The `dtGAP` function enhances decision tree visualization by incorporating the strengths of Generalized Association Plots (GAP).
#' While decision trees are valued for their interpretability, they often overlook deeper data structures. In contrast, GAP is effective for revealing complex associations but is typically limited to unsupervised settings.
#' dtGAP bridges this gap by introducing matrix-based visualizations—such as the confusion matrix map, decision tree matrix map, and predicted class membership map—based on supervised correlation and distance metrics.
#' This offers a more comprehensive and interpretable representation of decision-making processes in tree-based models.
#'
#' @param x Character. Name or label of the dataset.
#' @param target_lab Character. Name of the target column. Required.
#' @param show Character. Which subset to return: "all", "train" or "test" .
#' @param model Character. Which implementation to use: one of "rpart", "party", "C50", or "caret".
#'   Ignored when \code{fit} is provided.
#' @param control List or control object. Optional control parameters passed to the chosen tree function.
#'   Ignored when \code{fit} is provided.
#' @param fit Optional pre-built tree model object. Supported classes: \code{party},
#'   \code{rpart}, \code{C5.0}, or \code{train} (caret). When supplied, \code{model}
#'   and \code{control} are ignored; the model type is auto-detected.
#' @param user_var_imp Optional named numeric vector of variable importance scores.
#'   Only used when \code{fit} is provided. If NULL, importance is extracted
#'   automatically (or the importance barplot is suppressed if extraction fails).
#' @param data_train Data frame. Training data. Required if show == "train" or when splitting from all.
#' @param data_test Data frame. Test data. Required if show == "test" or when splitting from all.
#' @param data_all Data frame. Full dataset. If provided and show == "all", used directly; otherwise split into train/test.
#' @param test_size Numeric. Proportion of data to assign to testing set when splitting data_all (default 0.3).
#' @param task Character. Type of task: "classification" or "regression".
#' @param trans_type Character. One of "percentize","normalize","scale","none" passed to scale_norm().
#' @param col_proximity Character. Correlation method: "pearson","spearman","kendall".
#' @param linkage_method Character. Linkage for supervised distance: "CT","SG","CP".
#' @param seriate_method Character. Seriation method for distance objects; see
#'   `seriation::list_seriation_methods("dist")` for all supported options. Default: `"TSP"`.
#' @param cRGAR_w Integer. Window size for RGAR calculation.
#' @param select_vars Character vector or NULL. If provided, only these variables
#'   are displayed in the heatmap panels. The tree is always fit on ALL variables;
#'   this parameter is display-only. Names must match feature column names.
#' @param sort_by_data_type Logical. If TRUE, preserves data_type grouping within nodes.
#' @param custom_layout Optional data.frame with custom node positions (columns: id, x, y).
#' @param panel_space Numeric. Vertical spacing between panels in layout.
#' @param margin Numeric. Margin around the drawing area (mm).
#' @param total_w Numeric. Total width of page (mm).
#' @param total_h Numeric. Total height of page (mm).
#' @param tree_p Numeric. Proportion of total width allocated to the tree panel.
#' @param include_var_imp Logical; include importance barplot if TRUE (default TRUE).
#' @param col_var_imp Color for importance bars (default "orange").
#' @param var_imp_bar_width Numeric width of bars (default 0.8).
#' @param var_imp_fontsize Font size for importance text (default 5).
#' @param split_var_bg Background color for split variable names (default "darkgreen").
#' @param split_var_fontsize Font size for split variable names (default 5).
#' @param Col_Prox_palette RColorBrewer palette for correlation heatmap (default "RdBu").
#' @param Col_Prox_n_colors Number of colors in correlation scale (default 11).
#' @param label_map Optional named vector to map raw labels to new labels.
#' @param label_map_colors Optional named vector of colors for mapped labels.
#' @param type_palette RColorBrewer palette for data_type (default "Dark2").
#' @param label_palette Function or vector of colors for true and predicted value (default OrRd).
#' @param n_label_color Number of colors for label palette (default 9).
#' @param prop_palette Function or vector of colors for probability gradient (default gray).
#' @param n_prop_colors Number of colors for probability palette (default 11).
#' @param pred_ha_gap Unit for gap between annotations (default \code{unit(1, "mm")} ).
#' @param Row_Prox_palette RColorBrewer palette name for row proximity color scale (default "Spectral").
#' @param Row_Prox_n_colors Number of discrete colors for row proximity (default 11).
#' @param row_border Logical; draw cell borders if TRUE (default TRUE).
#' @param row_gap Unit object for gap between annotation blocks (default \code{unit(1, "mm")} ).
#' @param sorted_dat_palette RColorBrewer palette for heatmap values (default "Blues").
#' @param sorted_dat_n_colors Number of colors for heatmap (default 9).
#' @param show_row_names Logical. Whether to display row names in the heatmap (default TRUE).
#' @param row_names_gp \code{gpar} settings for row name font (default fontsize=5).
#' @param raw_value_col User-defined colors for raw data values.
#' @param show_row_prox Logical, whether to show row proximity.
#' @param show_col_prox Logical, whether to show column proximity.
#' @param lgd_direction Character. Layout direction of packed legends, either "vertical" or "horizontal".
#' @param x_eval_start X-axis starting position (in mm) for evaluation text. Default is 15.
#' @param y_eval_start Y-axis starting position (in mm) for evaluation text. If NULL, it will be computed automatically.
#' @param eval_text Font size for the evaluation text. Default is 7.
#' @param print_eval Logical, whether to show evaluation results. Default is TRUE.
#' @param simple_metrics Logical. If TRUE, use simple metric summary instead of full confusion matrix. Default is FALSE.
#' @param interactive Logical. If TRUE, launches an interactive Shiny app via
#'   \code{InteractiveComplexHeatmap::htShiny()} showing the heatmap panels with
#'   hover/click/zoom. The tree panel is omitted in interactive mode (limitation
#'   of InteractiveComplexHeatmap). Requires the \pkg{InteractiveComplexHeatmap}
#'   package (from Bioconductor). Default is FALSE.
#' @return Draws the full dtGAP visualization (decision tree + heatmap + evaluation)
#'   to the current graphics device. Called for its side effect; returns invisibly.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Case 1: test_covid
#' dtGAP(
#'   data_train = train_covid,
#'   data_test = test_covid,
#'   target_lab = "Outcome", show = "test",
#'   label_map = c("0" = "Survival", "1" = "Death"),
#'   label_map_colors = c(
#'     "Survival" = "#50046d", "Death" = "#fcc47f"
#'   ),
#'   raw_value_col = colorRampPalette(
#'     c("#33286b", "#26828e", "#75d054", "#fae51f")
#'   )(9)
#' )
#' # Case 2: Psychosis_Disorder
#' dtGAP(
#'   data_all = Psychosis_Disorder,
#'   model = "party", show = "all",
#'   trans_type = "none", target_lab = "UNIQID"
#' )
#' }
dtGAP <- function(x = NULL,
                  target_lab = NULL,
                  show = c("all", "train", "test"),
                  model = c("rpart", "party", "C50", "caret"),
                  control = NULL,
                  fit = NULL,
                  user_var_imp = NULL,
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
                  select_vars = NULL,
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
                  simple_metrics = FALSE,
                  interactive = FALSE) {
  model <- match.arg(model)
  show <- match.arg(show)
  task <- match.arg(task)
  trans_type <- match.arg(trans_type)
  col_proximity <- match.arg(col_proximity)
  linkage_method <- match.arg(linkage_method)
  lgd_direction <- match.arg(lgd_direction)
  valid_methods <- seriation::list_seriation_methods("dist")
  seriate_method <- as.character(seriate_method)
  if (length(seriate_method) != 1 ||
      !seriate_method %in% valid_methods) {
    stop(
      "`seriate_method` must be one of: ",
      paste(valid_methods, collapse = ", "),
      "; not '",
      seriate_method,
      "'."
    )
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


  data_all <- add_data_type(data_train, data_test, data_all, test_size)
  data_all <- prepare_features(data_all, target_lab, task)

  if (is.null(data_train)) {
    data_train <- data_all %>%
      filter(data_type == "train") %>%
      select(-data_type)
  }
  if (is.null(data_test)) {
    data_test <- data_all %>%
      filter(data_type == "test") %>%
      select(-data_type)
  }

  data <- switch(show,
                 all   = data_all,
                 train = data_train,
                 test  = data_test)


  if (!is.null(fit)) {
    fit_raw <- fit
    detected_model <- detect_model_type(fit)
    model <- detected_model
    fit <- convert_to_party(fit_raw, detected_model)
    if (!is.null(user_var_imp)) {
      var_imp <- round(user_var_imp / sum(user_var_imp), 2)
    } else {
      var_imp <- extract_var_imp(fit_raw, detected_model)
      if (is.null(var_imp)) include_var_imp <- FALSE
    }
  } else {
    train_result <- train_tree(
      data_train = data_train,
      data = data,
      target_lab = target_lab,
      model = model,
      task = task,
      control = control
    )
    fit <- train_result$fit
    var_imp <- train_result$var_imp
  }


  tree_res <- compute_tree(
    fit = fit,
    model = model,
    show = show,
    data = data,
    target_lab = target_lab,
    task = task,
    custom_layout = custom_layout,
    panel_space = panel_space
  )


  sorted_dat <- sorted_mat(
    tree_res = tree_res,
    target_lab = target_lab,
    show = show,
    trans_type = trans_type,
    col_proximity = col_proximity,
    linkage_method = linkage_method,
    seriate_method = seriate_method,
    w = cRGAR_w,
    sort_by_data_type = sort_by_data_type
  )

  # --- select_vars: display-only variable filtering ---
  if (!is.null(select_vars)) {
    if (!is.character(select_vars) || length(select_vars) == 0) {
      stop("`select_vars` must be a non-empty character vector or NULL.")
    }
    feat_cols <- colnames(sorted_dat$sorted_test_matrix)
    bad <- setdiff(select_vars, feat_cols)
    if (length(bad) > 0) {
      stop("These `select_vars` are not in the feature columns: ",
           paste(bad, collapse = ", "))
    }
    available <- intersect(select_vars, feat_cols)
    sorted_dat$sorted_test_matrix <- sorted_dat$sorted_test_matrix[, available, drop = FALSE]
    sorted_dat$sorted_col_names <- available
    if (!is.null(sorted_dat$col_pro_mat_sorted)) {
      sorted_dat$col_pro_mat_sorted <- sorted_dat$col_pro_mat_sorted[available, available, drop = FALSE]
    }
    var_imp <- var_imp[intersect(names(var_imp), select_vars)]
    if (length(var_imp) > 0) var_imp <- round(var_imp / sum(var_imp), 2)
  }

  layout <- compute_layout(
    sorted_dat = sorted_dat,
    margin = margin,
    total_w = total_w,
    total_h = total_h,
    tree_p = tree_p
  )


  col_ht_res <- col_ht(
    fit = fit,
    sorted_dat = sorted_dat,
    var_imp = var_imp,
    layout = layout,
    include_var_imp = include_var_imp,
    col_var_imp = col_var_imp,
    var_bar_width = var_imp_bar_width,
    var_fontsize = var_imp_fontsize,
    split_var_bg = split_var_bg,
    split_var_fontsize = split_var_fontsize,
    palette = Col_Prox_palette,
    n_colors = Col_Prox_n_colors,
    show_col_prox = show_col_prox
  )


  split_vec <- get_split_vec(sorted_dat = sorted_dat, tree_res = tree_res)
  pred_ha_res <- prediction_annotation(
    sorted_dat = sorted_dat,
    target_lab = target_lab,
    task = task,
    label_map = label_map,
    label_map_colors = label_map_colors,
    type_palette = type_palette,
    label_palette = label_palette,
    n_label_color = n_label_color,
    prop_palette = prop_palette,
    n_prop_colors = n_prop_colors,
    gap_mm = pred_ha_gap
  )

  row_prop_ha_res <- row_prop_anno(
    sorted_dat = sorted_dat,
    layout = layout,
    split_vec = split_vec,
    palette = Row_Prox_palette,
    n_colors = Row_Prox_n_colors,
    border = row_border,
    gap_mm = row_gap,
    show_row_prox = show_row_prox
  )
  main_ht_res <- make_main_heatmap(
    sorted_dat = sorted_dat,
    split_vec = split_vec,
    pred_ha = pred_ha_res$annotation,
    row_prop_ha = row_prop_ha_res$annotation,
    layout = layout,
    palette = sorted_dat_palette,
    n_colors = sorted_dat_n_colors,
    show_row_names = show_row_names,
    row_names_gp = row_names_gp,
    show_row_prox = show_row_prox,
    raw_value_col = raw_value_col
  )


  pals <- c(
    col_ht_res$palettes,
    pred_ha_res$palettes,
    row_prop_ha_res$palettes,
    main_ht_res$palettes
  )
  if (!show_row_prox) {
    pals$col_Row_Proximity <- NULL
  }
  if (!show_col_prox) {
    pals$col_Col_Proximity <- NULL
  }
  legends <- generate_legend_bundle(
    sorted_dat = sorted_dat,
    task = task,
    show = show,
    type_cols = pals$type_cols,
    label_cols = pals$label_cols,
    prop_cols = pals$prop_cols,
    col_mat = pals$col_mat,
    col_Col_Proximity = pals$col_Col_Proximity,
    col_Row_Proximity = pals$col_Row_Proximity,
    direction = lgd_direction
  )


  combined_heatmap <- if (show_col_prox)
    col_ht_res$heatmap %v% main_ht_res$heatmap
  else
    main_ht_res$heatmap

  if (interactive) {
    if (!requireNamespace("InteractiveComplexHeatmap", quietly = TRUE)) {
      stop("Install InteractiveComplexHeatmap for interactive mode: ",
           "BiocManager::install('InteractiveComplexHeatmap')")
    }
    InteractiveComplexHeatmap::htShiny(
      combined_heatmap,
      heatmap_legend_list = legends
    )
    return(invisible(NULL))
  }

  heat <- grid.grabExpr(
    draw(
      combined_heatmap,
      heatmap_legend_list = legends,
      auto_adjust = FALSE,
      ht_gap = unit(0, "mm"),
      newpage = FALSE
    )
  )

  tree_viz <- prepare_tree(tree_res = tree_res, model = model)
  eval_res <- eval_tree(
    x = x,
    fit = fit,
    task = task,
    tree_res = tree_res,
    target_lab = target_lab,
    sorted_dat = sorted_dat,
    show = show,
    model = model,
    col_proximity = col_proximity,
    linkage_method = linkage_method,
    seriate_method = seriate_method,
    simple_metrics = simple_metrics
  )
  draw_all(
    prepare_tree = tree_viz,
    heat = heat,
    total_w = total_w,
    total_h = total_h,
    layout = layout,
    x_eval_start = x_eval_start,
    y_eval_start = y_eval_start,
    eval_text = eval_text,
    eval_res = eval_res,
    print_eval = print_eval,
    show_col_prox = show_col_prox,
    show_row_prox = show_row_prox
  )
}
