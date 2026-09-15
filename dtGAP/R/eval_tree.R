#' Evaluate Tree Model Predictions and Metrics
#'
#' @description
#' Generates summary information and confusion matrix metrics for training and/or test subsets
#' based on a fitted decision tree and sorted matrix results.
#'
#' @param x Character. Name or label of the dataset.
#' @param fit A fitted partykit tree object used to extract split variables.
#' @param task Character. Type of task: "classification" or "regression".
#' @param tree_res List. Output from \code{compute_tree()}
#' @param target_lab Character. Name of the target column in \code{tree_res$dat}.
#' @param sorted_dat List. Output from \code{sorted_mat()}.
#' @param show Character. "train","test", or "all" to select subset before sorting.
#' @param model Character. Identifier for the model method (e.g., "rpart").
#' @param simple_metrics Logical. If TRUE, use simple metric summary instead of full confusion matrix (default FALSE).
#' @param col_proximity Character. Correlation method: "pearson","spearman","kendall".
#' @param linkage_method Character. Linkage for supervised distance: "CT","SG","CP".
#' @param seriate_method Character. Seriation method for distance objects; see
#'   `seriation::list_seriation_methods("dist")` for all supported options. Default: `"TSP"`.
#'
#' @return A list with elements:
#'   \item{data_info}{Character summary of dataset name, sizes, methods, and scores.}
#'   \item{train_metrics}{Character output of the train confusion matrix (if applicable).}
#'   \item{test_metrics}{Character output of the test confusion matrix (if applicable).}
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(rpart)
#' library(partykit)
#' library(ggparty)
#' library(dplyr)
#' library(seriation)
#' data_all <- add_data_type(
#'   data_train = train_covid, data_test = test_covid
#' )
#' data <- prepare_features(
#'   data_all,
#'   target_lab = "Outcome",
#'   task = "classification"
#' )
#' train_tree <- train_tree(
#'   data_train = train_covid,
#'   target_lab = "Outcome", model = "rpart"
#' )
#' fit <- train_tree$fit
#' var_imp <- train_tree$var_imp
#' tree_res <- compute_tree(
#'   fit,
#'   model = "rpart", show = "test",
#'   data = data, target_lab = "Outcome",
#'   task = "classification"
#' )
#' sorted_dat <- sorted_mat(
#'   tree_res,
#'   target_lab = "Outcome", show = "test"
#' )
#' # Case 1: Pass the dataset name
#' eval_tree(
#'   x = "covid", fit = fit,
#'   task = "classification",
#'   tree_res = tree_res,
#'   target_lab = "Outcome",
#'   sorted_dat = sorted_dat,
#'   show = "test", model = "rpart"
#' )
#' }
eval_tree <- function(x = NULL,
                      fit = NULL,
                      task = c("classification", "regression"),
                      tree_res = NULL,
                      target_lab = NULL,
                      sorted_dat = NULL,
                      show = c("all", "train", "test"),
                      model = c("rpart", "party", "C50", "caret", "cforest"),
                      col_proximity = c("pearson", "spearman", "kendall"),
                      linkage_method = c("CT", "SG", "CP"),
                      seriate_method = "TSP",
                      simple_metrics = FALSE) {
  task <- match.arg(task)
  show <- match.arg(show)
  model <- match.arg(model)
  col_proximity <- match.arg(col_proximity)
  linkage_method <- match.arg(linkage_method)
  valid_methods <- seriation::list_seriation_methods("dist")
  if (!(seriate_method %in% valid_methods)) {
    stop("`seriate_method` must be one of: ",
         paste(valid_methods, collapse = ", "))
  }

  dat <- tree_res$dat
  if (show == "all") {
    pred_train <- subset(dat, data_type == "train")
    pred_test <- subset(dat, data_type == "test")
  } else if (show == "train") {
    pred_train <- dat
    pred_test <- NULL
  } else {
    pred_train <- NULL
    pred_test <- dat
  }
  train_size <- if (!is.null(pred_train))
    nrow(pred_train)
  else
    nrow(fit$data)
  test_size <- if (!is.null(pred_test))
    nrow(pred_test)
  else
    NA

  cRGAR_score <- sprintf("%.2f", sorted_dat$cRGAR_score)
  data_info <- paste0(
    "Dataset: ",
    x,
    " | Model: ",
    model,
    " | Train size: ",
    train_size,
    if (!is.na(test_size))
      paste0(" | Test size: ", test_size),
    "\n",
    if (!is.null(sorted_dat$col_pro_mat_sorted))
      paste0("Column proximity: ", col_proximity),
    if (!is.null(sorted_dat$row_pro_mat_sorted))
      paste0(" | Linkage: ", linkage_method),
    if (!is.null(sorted_dat$row_pro_mat_sorted) &&
        !is.null(sorted_dat$col_pro_mat_sorted)) {
      paste0(" | Seriate: ", seriate_method, " | cRGAR: ", cRGAR_score, "\n")
    }
  )


  calc_cm <- function(df) {
    df[[target_lab]] <- factor(df[[target_lab]])
    df$y_hat <- factor(df$y_hat)
    cm <- caret::confusionMatrix(df$y_hat, df[[target_lab]])
    paste(capture.output(cm), collapse = "\n")
  }


  calc_simple_class <- function(df) {
    metrics <- yardstick::metric_set(
      yardstick::accuracy,
      yardstick::bal_accuracy,
      yardstick::kap,
      yardstick::precision,
      yardstick::recall,
      yardstick::specificity
    )
    metrics(df,
            truth = !!rlang::sym(target_lab),
            estimate = y_hat) %>%
      dplyr::transmute(print_metric = paste(toupper(.metric), format(.estimate, digits = 3), sep = ": ")) %>%
      dplyr::pull() %>%
      paste(collapse = "\n")
  }

  calc_simple_reg <- function(df) {
    metrics <- yardstick::metric_set(yardstick::rsq,
                                     yardstick::mae,
                                     yardstick::rmse,
                                     yardstick::ccc)
    metrics(df,
            truth = !!rlang::sym(target_lab),
            estimate = y_hat) %>%
      dplyr::transmute(print_metric = paste(toupper(.metric), format(.estimate, digits = 3), sep = ": ")) %>%
      dplyr::pull() %>%
      paste(collapse = "\n")
  }

  train_metrics <- NULL
  if (!is.null(pred_train)) {
    if (task == "classification") {
      train_metrics <- if (simple_metrics) {
        paste("Train Metrics:", calc_simple_class(pred_train), sep = "\n")
      } else {
        paste("Train Metrics:", calc_cm(pred_train), sep = "\n")
      }
    } else {
      # regression
      train_metrics <- paste("Train Metrics:", calc_simple_reg(pred_train), sep = "\n")
    }
  }

  test_metrics <- NULL
  if (!is.null(pred_test)) {
    if (task == "classification") {
      test_metrics <- if (simple_metrics) {
        paste("Test Metrics:", calc_simple_class(pred_test), sep = "\n")
      } else {
        paste("Test Metrics:", calc_cm(pred_test), sep = "\n")
      }
    } else {
      # regression
      test_metrics <- paste("Test Metrics:", calc_simple_reg(pred_test), sep = "\n")
    }
  }

  list(
    data_info     = data_info,
    train_metrics = train_metrics,
    test_metrics  = test_metrics
  )
}


#' Draw Text Indicator on Grid Graphics
#' @noRd
draw_indicator <- function(text_eval,
                           x_eval = 15,
                           y_eval = NULL,
                           size = 7,
                           lineheight = 1.2,
                           just = "left") {
  grid.text(
    label = text_eval,
    x     = unit(x_eval, "mm"),
    y     = unit(y_eval, "mm"),
    gp    = gpar(fontsize = size, lineheight = lineheight),
    just  = just
  )
}
