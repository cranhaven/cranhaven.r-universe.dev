#' Prepare Tree Plot Data for Visualization
#'
#' This function processes a tree model's output and prepares node and segment data
#' for visualization using `ggplot2` or other plotting tools. It supports various tree
#' model formats such as `rpart`, `party`, `C50`, and `caret`.
#'
#' @param tree_res A list object containing tree plotting information, including a `plot_data` data frame.
#' @param model A string indicating the tree model used. Options are `"rpart"`, `"party"`, `"C50"`, or `"caret"`.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{plot_data}{A data frame of node-level information with labels for visualization.}
#'   \item{branches}{A data frame of edge (branch) coordinates for connecting parent and child nodes.}
#' }
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
#' prepare_tree(tree_res, model = "rpart")
#' }
prepare_tree <- function(tree_res,
                         model = c("rpart", "party", "C50", "caret", "cforest")) {
  model <- match.arg(model)
  plot_data <- tree_res$plot_data

  total_n <- plot_data %>%
    filter(is.na(parent)) %>%
    pull(recalculated_nodesize)


  asterisk_sign <- function(p_value) {
    ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", "")))
  }


  if (model %in% c("party", "cforest")) {
    has_pvalue <- "p.value" %in% names(plot_data) &&
      is.numeric(plot_data$p.value) &&
      any(!is.na(plot_data$p.value))

    if (has_pvalue) {
      plot_data <- plot_data %>% mutate(
        parent_label = paste0(
          splitvar,
          " (",
          round(recalculated_nodesize / total_n * 100, 0),
          "%)\n",
          "p = ",
          formatC(p.value, format = "f", digits = 3),
          " ",
          "(",
          asterisk_sign(p.value),
          ")"
        ),
        child_label = paste0(round(
          recalculated_nodesize / total_n * 100, 0
        ), "%")
      )
    } else {
      plot_data <- plot_data %>% mutate(
        parent_label = paste0(
          splitvar,
          " (",
          round(recalculated_nodesize / total_n * 100, 0),
          "%)"
        ),
        child_label = paste0(round(
          recalculated_nodesize / total_n * 100, 0
        ), "%")
      )
    }
  }

  if (model == "rpart") {
    prob_cols <- grep("^prob_", names(plot_data), value = TRUE)
    plot_data <- plot_data %>%
      rowwise() %>%
      mutate(
        prob_text = paste(round(c_across(
          all_of(prob_cols)
        ), 2), collapse = ", "),
        parent_label = paste0(
          splitvar,
          " (",
          round(recalculated_nodesize / total_n * 100, 0),
          "%)\n",
          "(",
          prob_text,
          ")"
        ),
        child_label = paste0(
          "(",
          round(recalculated_nodesize / total_n * 100, 0),
          "%)\n",
          "(",
          prob_text,
          ")"
        )
      ) %>%
      ungroup() %>%
      dplyr::select(-prob_text)
  }


  plot_data <- plot_data %>% mutate(node_label = if_else(kids > 0, parent_label, child_label))

  compute_branches <- function(pd) {
    children <- pd %>%
      filter(!is.na(parent)) %>%
      dplyr::select(child_id = id, parent)
    parents <- pd %>% dplyr::select(parent_id = id,
                                    x_parent = x,
                                    y_parent = y)
    segs <- children %>%
      left_join(parents, by = c("parent" = "parent_id")) %>%
      left_join(pd %>% dplyr::select(
        child_id = id,
        x_child = x,
        y_child = y
      ),
      by = "child_id")
    segs %>% transmute(
      x_start = x_parent,
      y_start = y_parent,
      x_end   = x_child,
      y_end   = y_child
    )
  }
  branches <- compute_branches(plot_data)


  branch_labels <- plot_data %>%
    left_join(plot_data %>%
                select(id, x_par = x, y_par = y),
              by = c("parent" = "id"))

  list(plot_data = plot_data,
       branches = branches,
       branch_labels = branch_labels)
}
