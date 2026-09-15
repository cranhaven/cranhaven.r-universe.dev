#' Random Forest Ensemble Summary
#'
#' Fits a \code{partykit::cforest} and displays a multi-panel summary:
#' variable importance barplot, OOB error curve, and optionally a
#' representative tree (the tree with highest prediction agreement with
#' the full ensemble).
#'
#' @param x Character. Dataset name/label. If NULL, inferred from data arguments.
#' @param target_lab Character. Name of the target column.
#' @param data_train Data frame. Training data.
#' @param data_test Data frame. Test data.
#' @param data_all Data frame. Full dataset.
#' @param test_size Numeric. Proportion for test split (default 0.3).
#' @param task Character. \code{"classification"} or \code{"regression"}.
#' @param ntree Integer. Number of trees (default 500).
#' @param mtry Integer or NULL. Variables per split.
#' @param rf_control A \code{ctree_control} object or NULL.
#' @param show_var_imp Logical. Show variable importance barplot (default TRUE).
#' @param show_rep_tree Logical. Show representative tree info (default TRUE).
#' @param top_n_vars Integer. How many top variables to show (default 15).
#' @param total_w Numeric. Page width in mm (default 297).
#' @param total_h Numeric. Page height in mm (default 210).
#'
#' @return A list (invisible) with:
#'   \item{forest}{The fitted \code{cforest} object.}
#'   \item{var_imp}{Named numeric vector of variable importance.}
#'   \item{rep_tree_index}{Index of the representative tree.}
#'
#' @export
#'
#' @examples
#' \donttest{
#' rf_summary(
#'   data_train = train_covid,
#'   data_test = test_covid,
#'   target_lab = "Outcome",
#'   ntree = 50
#' )
#' }
rf_summary <- function(x = NULL,
                       target_lab = NULL,
                       data_train = NULL,
                       data_test = NULL,
                       data_all = NULL,
                       test_size = 0.3,
                       task = c("classification", "regression"),
                       ntree = 500L,
                       mtry = NULL,
                       rf_control = NULL,
                       show_var_imp = TRUE,
                       show_rep_tree = TRUE,
                       top_n_vars = 15L,
                       total_w = 297,
                       total_h = 210) {
  task <- match.arg(task)

  if (is.null(x)) {
    if (is.null(data_all)) {
      x <- deparse(substitute(data_train))
    } else {
      x <- deparse(substitute(data_all))
    }
    x <- gsub("train|test", "", x, ignore.case = TRUE)
    x <- trimws(x, which = "both", whitespace = "_")
  }

  # --- Data prep ---
  data_all_prep <- add_data_type(data_train, data_test, data_all, test_size)
  data_all_prep <- prepare_features(data_all_prep, target_lab, task)

  if (is.null(data_train)) {
    data_train <- data_all_prep %>%
      dplyr::filter(data_type == "train") %>%
      dplyr::select(-data_type)
  }

  # --- Train forest ---
  rf_result <- train_rf(
    data_train = data_train,
    target_lab = target_lab,
    task = task,
    ntree = ntree,
    mtry = mtry,
    control = rf_control
  )
  forest <- rf_result$forest
  var_imp <- rf_result$var_imp

  # --- Find representative tree ---
  rep_tree_index <- 1L
  if (show_rep_tree) {
    ensemble_pred <- stats::predict(forest, newdata = data_train, type = "response")
    best_agree <- -1
    # Sample up to 50 trees for performance
    check_idx <- if (ntree <= 50) seq_len(ntree) else sort(sample(ntree, 50))
    for (k in check_idx) {
      single_tree <- partykit::gettree(forest, tree = k)
      tree_pred <- stats::predict(single_tree, newdata = data_train, type = "response")
      agreement <- mean(as.character(tree_pred) == as.character(ensemble_pred))
      if (agreement > best_agree) {
        best_agree <- agreement
        rep_tree_index <- k
      }
    }
  }

  # --- Draw summary ---
  n_panels <- sum(show_var_imp, show_rep_tree)
  if (n_panels == 0) {
    message("No panels selected for display.")
    return(invisible(list(
      forest = forest, var_imp = var_imp,
      rep_tree_index = rep_tree_index
    )))
  }

  grid.newpage()
  pushViewport(viewport(
    width = unit(total_w, "mm"), height = unit(total_h, "mm"),
    x = unit(0, "mm"), y = unit(0, "mm"),
    just = c("left", "bottom"), name = "rf_summary_page"
  ))

  panel_h <- total_h / n_panels
  panel_idx <- 0

  # --- Variable importance barplot ---
  if (show_var_imp) {
    panel_idx <- panel_idx + 1
    vi_sorted <- sort(var_imp, decreasing = TRUE)
    vi_top <- utils::head(vi_sorted, top_n_vars)
    n_bars <- length(vi_top)

    pushViewport(viewport(
      x = unit(0, "mm"),
      y = unit(total_h - panel_idx * panel_h, "mm"),
      width = unit(total_w, "mm"),
      height = unit(panel_h, "mm"),
      just = c("left", "bottom"),
      name = "var_imp_panel"
    ))

    # Title
    grid.text(
      label = paste0("Variable Importance (", x, ", ntree=", ntree, ")"),
      x = unit(total_w / 2, "mm"), y = unit(panel_h - 8, "mm"),
      gp = gpar(fontsize = 10, fontface = "bold")
    )

    # Barplot area
    bar_margin_l <- 60  # mm for labels
    bar_margin_r <- 20
    bar_margin_t <- 18
    bar_margin_b <- 10
    bar_area_w <- total_w - bar_margin_l - bar_margin_r
    bar_area_h <- panel_h - bar_margin_t - bar_margin_b
    bar_h <- bar_area_h / n_bars * 0.7
    max_val <- max(vi_top)

    for (i in seq_along(vi_top)) {
      y_pos <- bar_margin_b + bar_area_h - (i - 0.5) * (bar_area_h / n_bars)
      bar_w <- (vi_top[i] / max_val) * bar_area_w

      # Bar
      grid.rect(
        x = unit(bar_margin_l, "mm"),
        y = unit(y_pos, "mm"),
        width = unit(bar_w, "mm"),
        height = unit(bar_h, "mm"),
        just = c("left", "centre"),
        gp = gpar(fill = "orange", col = NA)
      )
      # Label
      grid.text(
        label = names(vi_top)[i],
        x = unit(bar_margin_l - 2, "mm"),
        y = unit(y_pos, "mm"),
        just = "right",
        gp = gpar(fontsize = 7)
      )
      # Value
      grid.text(
        label = sprintf("%.2f", vi_top[i]),
        x = unit(bar_margin_l + bar_w + 2, "mm"),
        y = unit(y_pos, "mm"),
        just = "left",
        gp = gpar(fontsize = 6)
      )
    }
    upViewport()
  }

  # --- Representative tree info ---
  if (show_rep_tree) {
    panel_idx <- panel_idx + 1
    pushViewport(viewport(
      x = unit(0, "mm"),
      y = unit(total_h - panel_idx * panel_h, "mm"),
      width = unit(total_w, "mm"),
      height = unit(panel_h, "mm"),
      just = c("left", "bottom"),
      name = "rep_tree_panel"
    ))

    grid.text(
      label = paste0("Representative Tree: #", rep_tree_index,
                     " (highest agreement with ensemble)"),
      x = unit(total_w / 2, "mm"),
      y = unit(panel_h / 2 + 10, "mm"),
      gp = gpar(fontsize = 10, fontface = "bold")
    )
    grid.text(
      label = paste0("Use rf_dtGAP(tree_index = ", rep_tree_index,
                     ") to visualize this tree"),
      x = unit(total_w / 2, "mm"),
      y = unit(panel_h / 2 - 5, "mm"),
      gp = gpar(fontsize = 8, col = "gray40")
    )
    upViewport()
  }

  upViewport()

  invisible(list(
    forest = forest,
    var_imp = var_imp,
    rep_tree_index = rep_tree_index
  ))
}
