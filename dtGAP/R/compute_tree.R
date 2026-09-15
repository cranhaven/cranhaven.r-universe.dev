#' Compute Decision Tree Data for Plotting and Analysis
#'
#' @description
#' Builds and processes a decision tree model object to prepare data for plotting,
#' including layout positions and terminal node summaries.
#' need to run util.R first
#'
#' @param fit A fitted decision party tree object.
#' @param model Character. Which implementation to use: one of "rpart", "party", "C50", or "caret".
#' @param show Character. Which subset to return:  "all", "train" or "test" .
#' @param data A data.frame containing the features and target for prediction.
#' @param target_lab Character. Name of the target column.
#' @param task Character. Task type: "classification" or "regression".
#' @param custom_layout Optional data.frame with custom node positions (columns: id, x, y).
#' @param panel_space Numeric. Vertical spacing between panels in layout.
#'
#' @return A list with components:
#'   * fit: the original fitted model
#'   * dat: data.frame of observations with node assignments and predictions
#'   * plot_data: data.frame of nodes with plotting variables and probabilities
#'   * layout: data.frame of node x/y positions
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(rpart)
#' library(partykit)
#' library(ggparty)
#' library(dplyr)
#' data <- add_data_type(
#'   data_all = Psychosis_Disorder
#' )
#' data <- prepare_features(
#'   data,
#'   target_lab = "UNIQID",
#'   task = "classification"
#' )
#' fit <- train_tree(
#'   data = data, target_lab = "UNIQID",
#'   model = "rpart"
#' )$fit
#' tree_res <- compute_tree(
#'   fit,
#'   model = "rpart", show = "all",
#'   data = data, target_lab = "UNIQID",
#'   task = "classification"
#' )
#' tree_res$dat
#' tree_res$plot_data
#' }
compute_tree <- function(fit = NULL,
                         model = c("rpart", "party", "C50", "caret", "cforest"),
                         show = c("all", "train", "test"),
                         data = NULL,
                         target_lab = NULL,
                         task = c("classification", "regression"),
                         custom_layout = NULL,
                         panel_space = 0.001) {
  model <- match.arg(model)
  show <- match.arg(show)
  task <- match.arg(task)

  dat <- prediction_df(fit, task, data)

  plot_data <- ggparty::ggparty(fit)$data
  term_dat <- term_node_pos(plot_data, dat)

  # add nodesize and layout information
  node_size_df <- recalculate_nodesize(plot_data, term_dat)

  layout <- position_nodes(plot_data, term_dat, custom_layout, panel_space)
  plot_data <- plot_data %>%
    left_join(node_size_df, by = "id") %>%
    dplyr::select(-x, -y) %>%
    left_join(layout, by = "id")

  # add class probability information
  if (task == "classification" && model %in% c("rpart", "caret")) {
    class_prob <- compute_class_prob(plot_data, dat, target_lab)
    plot_data <- plot_data %>%
      left_join(class_prob, by = "id")
  }

  # add break_label
  plot_data$breaks_clean <- sapply(plot_data$breaks_label, function(label) {
    if (all(is.na(label))) {
      return("NA")
    } else if (all(!str_detect(label, "\\d"))) {
      return(paste(label, collapse = ", "))
    } else {
      op <- str_extract(label, "<=|>=|<|>")
      val <- round(as.numeric(str_extract(label, "\\d+\\.?\\d*")), 2)
      return(paste0(op, " ", val))
    }
  })

  list(
    fit = fit,
    dat = dat,
    plot_data = plot_data,
    layout = layout
  )
}

#' Compute Class Probability Distributions for Tree Nodes
#' @noRd
compute_class_prob <- function(plot_data, dat, target_lab) {
  classes <- sort(unique(dat[[target_lab]]))
  n_class <- length(classes)
  summary_list <- list()


  leaf_nodes <- plot_data %>% filter(kids == 0)
  for (i in seq_len(nrow(leaf_nodes))) {
    cur_node <- leaf_nodes$id[i]
    node_data <- dat %>% filter(node_id == cur_node)

    if (nrow(node_data) == 0) {
      summary_list[[as.character(cur_node)]] <- list(count = 0,
                                                     class_counts = setNames(rep(0, n_class), classes))
    } else {
      counts <- nrow(node_data)
      cnts <- table(factor(node_data[[target_lab]], levels = classes))
      cnts <- as.numeric(cnts)
      names(cnts) <- classes
      summary_list[[as.character(cur_node)]] <- list(count = counts, class_counts = cnts)
    }
  }

  internal_nodes <- plot_data %>%
    filter(kids != 0) %>%
    arrange(desc(level))

  for (i in seq_len(nrow(internal_nodes))) {
    cur_node <- internal_nodes$id[i]
    child_ids <- plot_data %>%
      filter(parent == cur_node) %>%
      pull(id)
    total_count <- 0
    total_class_counts <- setNames(rep(0, n_class), classes)
    for (child in child_ids) {
      child_key <- as.character(child)
      if (!is.null(summary_list[[child_key]])) {
        total_count <- total_count + summary_list[[child_key]]$count
        total_class_counts <- total_class_counts + summary_list[[child_key]]$class_counts
      }
    }

    summary_list[[as.character(cur_node)]] <- list(count = total_count, class_counts = total_class_counts)
  }


  result <- data.frame(id = names(summary_list), stringsAsFactors = FALSE)
  for (cl in classes) {
    result[[paste0("prob_", cl)]] <- sapply(names(summary_list), function(nid) {
      node_summary <- summary_list[[nid]]
      tot <- node_summary$count
      if (tot > 0) {
        node_summary$class_counts[as.character(cl)] / tot
      } else {
        NA
      }
    })
  }

  result$id <- as.numeric(result$id)
  return(result[1:n_class])
}


# ------------------------------------------------------------------------------------
#' Calculate Node Sizes Based on Terminal Counts
#' @noRd

recalculate_nodesize <- function(plot_data, term_dat) {
  node_size_df <- term_dat %>%
    dplyr::select(id, n) %>%
    rename(recalculated_nodesize = n)

  internal_nodes <- plot_data %>%
    filter(kids != 0) %>%
    arrange(desc(level)) # Process from bottom up levels

  for (i in 1:nrow(internal_nodes)) {
    parent_id <- internal_nodes$id[i]
    child_ids <- plot_data$id[plot_data$parent == parent_id]

    children_nodesize <- node_size_df %>%
      filter(id %in% child_ids) %>%
      pull(recalculated_nodesize)
    parent_nodesize <- sum(children_nodesize)

    node_size_df <- node_size_df %>%
      add_row(id = parent_id, recalculated_nodesize = parent_nodesize)
  }

  node_size_df <- node_size_df %>% distinct(id, .keep_all = TRUE) # Remove duplicated terminal nodes from internal node loop if any
  return(node_size_df) # Return only node_size_df
}


# ------------------------------------------------------------------------------------
#' Apply the predicted tree on either new test data or training data.
#' @noRd
prediction_df <- function(fit, task, data) {
  data <- stats::na.omit(data)
  node_pred <- stats::predict(fit, newdata = data, type = "node")
  y_pred <- stats::predict(fit,
                           newdata = data,
                           type = "response",
                           simplify = FALSE) %>%
    .simplify_pred(id = node_pred, nam = as.character(node_pred))

  data_pred <- data %>%
    cbind(node_id = node_pred, y_hat = y_pred)

  if (task == "regression") {
    data_pred$y_hat <- round(data_pred$y_hat)
  }

  if (task == "classification") {
    y_prob <- stats::predict(fit,
                             newdata = data,
                             type = "prob",
                             simplify = FALSE) %>%
      .simplify_pred(id = node_pred, nam = as.character(node_pred))
    data_pred <- cbind(data_pred, y_prob)
  }

  data_pred <- data_pred %>%
    arrange(node_id) %>%
    mutate(Sample = row_number())

  data_pred
}


# ------------------------------------------------------------------------------------
#' Determines terminal node position.
#' @noRd
term_node_pos <- function(plot_data, dat) {
  node_labels <- dat %>%
    distinct(Sample, .keep_all = T) %>%
    count(node_id, y_hat) %>%
    rename(id = node_id)

  result <- plot_data %>%
    filter(kids == 0) %>%
    left_join(node_labels, by = "id") %>%
    mutate_at(vars(n), ~ replace(., is.na(.), 0))

  return(result)
}


#' Compute Smart Node Layout
#' @noRd
position_nodes <- function(plot_data,
                           term_dat,
                           custom_layout,
                           panel_space) {
  node_size <- term_dat$n
  max_level <- max(plot_data$level)

  new_y <- vector("numeric", length = nrow(term_dat))
  for (i in seq_along(term_dat$id)) {
    raw_pos <- (sum(node_size[0:i]) - node_size[i] / 2) / sum(node_size)
    new_y[i] <- raw_pos * (1 - (nrow(term_dat) - 1) * panel_space) + (i - 1) * panel_space
  }
  traverse <- term_dat %>% mutate(x = 1, y = new_y)

  adj_plot_data <- plot_data %>%
    dplyr::select(id, x, y, parent, level, kids) %>%
    filter(!id %in% term_dat$id) %>%
    bind_rows(traverse)

  while (!is.na(traverse$parent[1])) {
    last_lev <- traverse %>%
      dplyr::select(-n) %>%
      add_count(parent) %>%
      filter(n == 2)
    these_parents <- unique(last_lev$parent)

    for (p in these_parents) {
      kids_df <- last_lev[last_lev$parent == p, ]
      new_y_pos <- mean(kids_df$y)

      parent_level <- adj_plot_data$level[adj_plot_data$id == p]
      x_pos <- 1 * parent_level / max_level

      par_id <- adj_plot_data$id == p
      adj_plot_data[par_id, c("x", "y")] <- list(x_pos, new_y_pos)

      traverse <- traverse %>%
        filter(!(id %in% kids_df$id)) %>%
        bind_rows(adj_plot_data[par_id, ])
    }
  }

  if (!is.null(custom_layout)) {
    adj_plot_data <- adj_plot_data %>%
      filter(!id %in% custom_layout$id) %>%
      bind_rows(custom_layout)
  }

  adj_plot_data %>% dplyr::select(id, x, y)
}
