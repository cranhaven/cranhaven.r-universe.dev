#' Compare Multiple Decision Tree Models Side-by-Side
#'
#' Runs the dtGAP pipeline for each specified model and composes the results
#' side-by-side on a single wide page. Shared data preparation is performed
#' once; each model gets its own tree + heatmap panel.
#'
#' @param models Character vector of length >= 2. Models to compare.
#'   Each must be one of \code{"rpart"}, \code{"party"}, \code{"C50"}, or
#'   \code{"caret"}.
#' @param data_train Data frame. Training data.
#' @param data_test Data frame. Test data.
#' @param data_all Data frame. Full dataset (alternative to separate train/test).
#' @param target_lab Character. Name of the target column.
#' @param show Character. Which subset to show: \code{"all"}, \code{"train"},
#'   or \code{"test"}.
#' @param test_size Numeric. Proportion for test split (default 0.3).
#' @param task Character. \code{"classification"} or \code{"regression"}.
#' @param total_w Numeric. Total page width in mm (default 594, 2x A4 width).
#' @param total_h Numeric. Total page height in mm (default 210).
#' @param ... Additional visual parameters passed to each dtGAP panel
#'   (e.g. \code{trans_type}, \code{col_proximity}, \code{print_eval}).
#'
#' @return Draws the side-by-side comparison to the current graphics device.
#'   Called for its side effect; returns invisibly.
#'
#' @export
#'
#' @examples
#' \donttest{
#' compare_dtGAP(
#'   models = c("rpart", "party"),
#'   data_all = Psychosis_Disorder,
#'   target_lab = "UNIQID",
#'   show = "all",
#'   trans_type = "none",
#'   print_eval = FALSE
#' )
#' }
compare_dtGAP <- function(models = c("rpart", "party"),
                          data_train = NULL,
                          data_test = NULL,
                          data_all = NULL,
                          target_lab = NULL,
                          show = c("all", "train", "test"),
                          test_size = 0.3,
                          task = c("classification", "regression"),
                          total_w = 594,
                          total_h = 210,
                          ...) {
  show <- match.arg(show)
  task <- match.arg(task)
  valid_models <- c("rpart", "party", "C50", "caret")
  bad <- setdiff(models, valid_models)
  if (length(bad) > 0) {
    stop("Unsupported model(s): ", paste(bad, collapse = ", "),
         ". Choose from: ", paste(valid_models, collapse = ", "))
  }
  if (length(models) < 2) {
    stop("`models` must contain at least 2 models for comparison.")
  }

  n_models <- length(models)
  panel_w <- total_w / n_models
  label_h <- 8  # mm height for model label

  # Capture each model's dtGAP output as a grob
  grobs <- vector("list", n_models)
  for (k in seq_len(n_models)) {
    grobs[[k]] <- grid.grabExpr({
      dtGAP(
        data_train = data_train,
        data_test = data_test,
        data_all = data_all,
        target_lab = target_lab,
        show = show,
        model = models[k],
        test_size = test_size,
        task = task,
        total_w = panel_w,
        total_h = total_h - label_h,
        ...
      )
    })
  }

  # Compose panels side-by-side
  grid.newpage()
  pushViewport(viewport(
    width  = unit(total_w, "mm"),
    height = unit(total_h, "mm"),
    x = unit(0, "mm"), y = unit(0, "mm"),
    just = c("left", "bottom"),
    name = "compare_page"
  ))

  for (k in seq_len(n_models)) {
    x_start <- (k - 1) * panel_w

    # Model label
    pushViewport(viewport(
      x = unit(x_start + panel_w / 2, "mm"),
      y = unit(total_h - label_h / 2, "mm"),
      width = unit(panel_w, "mm"),
      height = unit(label_h, "mm"),
      just = "centre"
    ))
    grid.text(
      label = paste("Model:", models[k]),
      gp = gpar(fontsize = 10, fontface = "bold")
    )
    upViewport()

    # Panel content
    pushViewport(viewport(
      x = unit(x_start, "mm"),
      y = unit(0, "mm"),
      width = unit(panel_w, "mm"),
      height = unit(total_h - label_h, "mm"),
      just = c("left", "bottom"),
      name = paste0("panel_", k)
    ))
    grid.draw(grobs[[k]])
    upViewport()
  }

  upViewport()
  invisible(NULL)
}
