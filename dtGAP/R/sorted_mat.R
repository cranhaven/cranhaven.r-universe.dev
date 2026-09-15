#' Sort Feature Matrix by Tree and Correlation Structure
#'
#' @description
#' Orders samples and features based on tree-derived node grouping and correlation-based seriation.
#'
#' @param tree_res A list returned by compute_tree(), containing fit, dat, and plot_data.
#' @param target_lab Character. Name of the target column to exclude from features.
#' @param show Character. "train","test", or "all" to select subset before sorting.
#' @param trans_type Character. One of "percentize","normalize","scale","none" passed to scale_norm().
#' @param col_proximity Character. Correlation method: "pearson","spearman","kendall".
#' @param linkage_method Character. Linkage for supervised distance: "CT","SG","CP".
#' @param seriate_method Character. Seriation method for distance objects; see
#'   `seriation::list_seriation_methods("dist")` for all supported options. Default: `"TSP"`.
#' @param w Integer. Window size for RGAR calculation.
#' @param sort_by_data_type Logical. If TRUE, preserves data_type grouping within nodes.
#'
#' @return A list with:
#'   * sorted_row_names, sorted_col_names
#'   * row_pro_mat_sorted, col_pro_mat_sorted
#'   * cRGAR_score
#'   * sorted_test_matrix
#'   * node_ids
#'   * dat_sorted
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
#' sorted_dat <- sorted_mat(
#'   tree_res,
#'   target_lab = "UNIQID",
#'   show = "all", trans_type = "none",
#'   seriate_method = "GW_average",
#'   sort_by_data_type = FALSE
#' )
#' sorted_dat$row_pro_mat_sorted
#' sorted_dat$col_pro_mat_sorted
#' sorted_dat$cRGAR_score
#' }
sorted_mat <- function(tree_res = NULL,
                       target_lab = NULL,
                       show = c("all", "train", "test"),
                       trans_type = c("normalize", "scale", "percentize", "none"),
                       col_proximity = c("pearson", "spearman", "kendall"),
                       linkage_method = c("CT", "SG", "CP"),
                       seriate_method = "TSP",
                       w = 5,
                       sort_by_data_type = TRUE) {
  show <- match.arg(show)
  trans_type <- match.arg(trans_type)
  col_proximity <- match.arg(col_proximity)
  linkage_method <- match.arg(linkage_method)
  # coerce & validate
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


  stopifnot(all(c("fit", "dat", "plot_data") %in% names(tree_res)))

  df_fit <- tree_res$fit$data
  dat <- tree_res$dat
  plot_data <- tree_res$plot_data


  feats <- setdiff(colnames(df_fit), target_lab)
  if (show == "all") {
    feats <- setdiff(feats, "data_type")
  }

  leaf_nodes <- plot_data %>%
    dplyr::filter(kids == 0) %>%
    arrange(desc(y)) %>%
    dplyr::pull(id) %>%
    as.character()

  dat <- dat %>%
    dplyr::mutate(node_id = as.character(node_id)) %>%
    dplyr::arrange(factor(node_id, levels = leaf_nodes))

  numeric_feats <- feats[vapply(dat[feats], is.numeric, logical(1))]
  if (length(numeric_feats) > 0) {
    dat <- dat %>%
      dplyr::mutate(across(
        all_of(numeric_feats),
        ~ scale_norm(.x, trans_type = trans_type)
      ))
  }

  if (show == "all") {
    X <- dat %>% dplyr::select(Sample, node_id, all_of(feats), data_type)
    data_type <- X$data_type
    names(data_type) <- X$Sample
    mat <- X %>%
      dplyr::select(-Sample, -node_id, -data_type) %>%
      as.matrix()
    use_sort_by_dt <- sort_by_data_type
  } else {
    X <- dat %>% dplyr::select(Sample, node_id, all_of(feats))
    data_type <- NULL
    mat <- X %>%
      dplyr::select(-Sample, -node_id) %>%
      as.matrix()
    use_sort_by_dt <- FALSE
  }

  rownames(mat) <- X$Sample
  node_ids <- X$node_id
  names(node_ids) <- X$Sample


  # if any feature is factor, skip and return early
  if (any(vapply(df_fit[feats], is.factor, logical(1)))) {
    return(
      list(
        sorted_row_names    = NULL,
        sorted_col_names    = NULL,
        row_pro_mat_sorted  = NULL,
        col_pro_mat_sorted  = NULL,
        cRGAR_score         = NULL,
        sorted_test_matrix  = mat,
        node_ids            = node_ids,
        dat_sorted          = dat
      )
    )
  }

  # column seriation
  cor_mat <- seriate_supervised_col(mat, node_ids, seriate_method)
  sorted_col_names <- cor_mat$sorted_col_names
  col_pro_mat_sorted <- cor_mat$col_pro_mat_sorted

  # row seriation
  row_supervised_mat <- seriate_within_groups_supervised(mat,
                                                         node_ids,
                                                         data_type,
                                                         linkage_method,
                                                         seriate_method,
                                                         w,
                                                         sort_by_data_type = use_sort_by_dt)
  sorted_row_names <- row_supervised_mat$ordered_names
  row_pro_mat_sorted <- row_supervised_mat$mat_ordered
  cRGAR_score <- row_supervised_mat$cRGAR_score

  sorted_test_matrix <- mat[sorted_row_names, sorted_col_names]
  dat_sorted <- dat[match(sorted_row_names, as.character(dat$Sample)), ]


  return(
    list(
      sorted_row_names = sorted_row_names,
      sorted_col_names = sorted_col_names,
      row_pro_mat_sorted = row_pro_mat_sorted,
      col_pro_mat_sorted = col_pro_mat_sorted,
      cRGAR_score = cRGAR_score,
      sorted_test_matrix = sorted_test_matrix,
      node_ids = node_ids,
      dat_sorted = dat_sorted
    )
  )
}


#' Performs supervised column seriation
#' @noRd

seriate_supervised_col <- function(mat, node_ids, seriate_method = "TSP") {
  if (is.null(rownames(mat)))
    stop("'mat' must have row names.")
  if (!all(rownames(mat) %in% names(node_ids))) {
    stop("All row names of 'mat' must be present in the names of 'node_ids'.")
  }

  classes <- levels(as.factor(node_ids))
  n_total <- length(node_ids)
  var_names <- colnames(mat)
  p_list <- table(node_ids) / n_total

  full_col_names <- colnames(mat)
  combined_cor <- matrix(
    0,
    nrow = length(full_col_names),
    ncol = length(full_col_names),
    dimnames = list(full_col_names, full_col_names)
  )
  weight_mat <- matrix(
    0,
    nrow = length(full_col_names),
    ncol = length(full_col_names),
    dimnames = list(full_col_names, full_col_names)
  )

  for (class in classes) {
    class_idx <- which(node_ids == class)
    mat_sub <- mat[class_idx, , drop = FALSE]

    valid_vars <- apply(mat_sub, 2, function(col)
      sd(col, na.rm = TRUE)) > 0
    mat_sub <- mat_sub[, valid_vars, drop = FALSE]

    if (nrow(mat_sub) > 1 && ncol(mat_sub) >= 2) {
      cor_sub <- cor(mat_sub, use = "pairwise.complete.obs")

      non_na_idx <- which(!is.na(cor_sub), arr.ind = TRUE)


      cor_full <- matrix(
        NA,
        nrow = length(full_col_names),
        ncol = length(full_col_names),
        dimnames = list(full_col_names, full_col_names)
      )

      common_names <- intersect(rownames(cor_sub), full_col_names)
      cor_full[common_names, common_names] <- cor_sub[common_names, common_names]


      weight <- as.numeric(p_list[class])

      for (idx in seq_len(nrow(non_na_idx))) {
        i <- rownames(cor_sub)[non_na_idx[idx, 1]]
        j <- colnames(cor_sub)[non_na_idx[idx, 2]]
        combined_cor[i, j] <- combined_cor[i, j] + weight * cor_sub[i, j]
        weight_mat[i, j] <- weight_mat[i, j] + weight
      }
    }
  }

  final_cor <- combined_cor / weight_mat


  col_ser <- seriate(as.dist(1 - combined_cor), method = seriate_method)
  sorted_col_names <- var_names[get_order(col_ser)]
  col_pro_mat_sorted <- combined_cor[sorted_col_names, sorted_col_names]

  return(
    list(
      sorted_col_names = sorted_col_names,
      col_pro_mat_sorted = col_pro_mat_sorted
    )
  )
}


#' Find Representatives for Supervised Distance
#' @noRd


find_representatives <- function(cat1_label, cat2_label, X, Y, linkage) {
  if (cat1_label == cat2_label) {
    warning("Categories are the same. Representatives are typically for different categories.")
    return(NULL)
  }

  idx_cat1 <- which(Y == cat1_label)
  idx_cat2 <- which(Y == cat2_label)


  if (length(idx_cat1) == 0 || length(idx_cat2) == 0) {
    stop("One or both specified categories have no data points.")
  }

  X_cat1 <- X[idx_cat1, , drop = FALSE]
  X_cat2 <- X[idx_cat2, , drop = FALSE]

  R_ij <- NULL
  R_ji <- NULL

  # --- Centroid Linkage (CT) ---
  if (linkage == "CT") {
    # R(i, j) is the centroid of G(x_i)
    R_ij <- colMeans(X_cat1)
    # R(j, i) is the centroid of G(x_j)
    R_ji <- colMeans(X_cat2)
  }
  # --- Single (SG) or Complete (CP) Linkage ---
  else if (linkage %in% c("SG", "CP")) {
    dist_matrix <- outer(1:nrow(X_cat1), 1:nrow(X_cat2), FUN = Vectorize(function(r1, r2) {
      euclidean_dist(X_cat1[r1, , drop = TRUE], X_cat2[r2, , drop = TRUE])
    }))

    if (linkage == "SG") {
      min_val_idx_arr <- which(dist_matrix == min(dist_matrix), arr.ind = TRUE)[1, ]
    } else {
      # linkage == "CP"
      min_val_idx_arr <- which(dist_matrix == max(dist_matrix), arr.ind = TRUE)[1, ]
    }

    idx_in_cat1 <- min_val_idx_arr[1] # row_idx for cat1
    idx_in_cat2 <- min_val_idx_arr[2] # col_idx for cat2

    # R(i, j) is the point from G(x_i) involved in the min/max pair
    R_ij <- X_cat1[idx_in_cat1, , drop = TRUE]
    # R(j, i) is the point from G(x_j) involved in the min/max pair
    R_ji <- X_cat2[idx_in_cat2, , drop = TRUE]
  } else {
    stop("Invalid linkage method specified. Use 'SG', 'CP', or 'CT'.")
  }

  return(list(R_ij = R_ij, R_ji = R_ji))
}


#' Calculate Supervised Distance Between Two Points
#' @noRd

supervised_distance_pair <- function(i, j, X, Y, linkage, representatives = NULL) {
  x_i <- X[i, , drop = TRUE]
  x_j <- X[j, , drop = TRUE]
  y_i <- Y[i]
  y_j <- Y[j]

  # Case 1: Same category -> Euclidean distance
  if (y_i == y_j) {
    return(euclidean_dist(x_i, x_j))
  }

  # Case 2: Different categories -> Supervised distance formula
  else {
    # Find representatives
    if (is.null(representatives)) {
      reps <- find_representatives(y_i, y_j, X, Y, linkage)
    } else {
      reps <- representatives
    }
    if (is.null(reps)) {
      stop("Could not determine representatives for different categories.")
    }
    R_ij <- reps$R_ij
    R_ji <- reps$R_ji

    dist_i_Rij <- euclidean_dist(x_i, R_ij)
    dist_Rij_Rji <- euclidean_dist(R_ij, R_ji)
    dist_j_Rji <- euclidean_dist(x_j, R_ji)
    return(dist_i_Rij + dist_Rij_Rji + dist_j_Rji)
  }
}
euclidean_dist <- function(u, v) {
  sqrt(sum((u - v)^2))
}

#' Compute Supervised Distance Matrix
#' @noRd


calculate_supervised_distance_matrix <- function(X, Y, linkage) {
  N <- nrow(X)
  if (length(Y) != N) {
    stop("Number of labels in Y must match number of rows in X.")
  }

  dist_matrix <- matrix(0, nrow = N, ncol = N)
  rownames(dist_matrix) <- rownames(X)
  colnames(dist_matrix) <- rownames(X)

  unique_categories <- unique(Y)

  # Precompute representatives for each category pair
  if (linkage == "CT") {
    centroids <- lapply(unique_categories, function(cat) {
      colMeans(X[Y == cat, , drop = FALSE])
    })
    names(centroids) <- unique_categories
  } else {
    reps_cache <- list()
    for (pair in combn(unique_categories, 2, simplify = FALSE)) {
      reps <- find_representatives(pair[1], pair[2], X, Y, linkage)
      key <- paste(sort(c(
        as.character(pair[1]), as.character(pair[2])
      )), collapse = "-")
      reps_cache[[key]] <- list(
        cat1 = pair[1],
        cat2 = pair[2],
        R_cat1 = reps$R_ij,
        R_cat2 = reps$R_ji
      )
    }
  }

  # Same-category blocks: vectorized Euclidean distance via dist()
  for (cat in unique_categories) {
    idx <- which(Y == cat)
    if (length(idx) > 1) {
      dist_matrix[idx, idx] <- as.matrix(dist(X[idx, , drop = FALSE]))
    }
  }

  # Cross-category blocks: vectorized supervised distance via outer sum
  if (length(unique_categories) > 1) {
    for (pair in combn(unique_categories, 2, simplify = FALSE)) {
      c1 <- pair[1]
      c2 <- pair[2]
      idx1 <- which(Y == c1)
      idx2 <- which(Y == c2)

      if (linkage == "CT") {
        R_c1 <- centroids[[c1]]
        R_c2 <- centroids[[c2]]
      } else {
        key <- paste(sort(c(
          as.character(c1), as.character(c2)
        )), collapse = "-")
        stored <- reps_cache[[key]]
        if (c1 == stored$cat1) {
          R_c1 <- stored$R_cat1
          R_c2 <- stored$R_cat2
        } else {
          R_c1 <- stored$R_cat2
          R_c2 <- stored$R_cat1
        }
      }

      # d(i in c1, j in c2) = ||x_i - R_c1|| + ||R_c1 - R_c2|| + ||x_j - R_c2||
      d_to_R_c1 <- sqrt(rowSums(sweep(X[idx1, , drop = FALSE], 2, R_c1)^2))
      d_to_R_c2 <- sqrt(rowSums(sweep(X[idx2, , drop = FALSE], 2, R_c2)^2))
      d_reps <- sqrt(sum((R_c1 - R_c2)^2))

      cross_dist <- outer(d_to_R_c1, d_to_R_c2, "+") + d_reps

      dist_matrix[idx1, idx2] <- cross_dist
      dist_matrix[idx2, idx1] <- t(cross_dist)
    }
  }

  return(dist_matrix)
}


#' Seriate Rows Within Groups Using Supervised Distance
#' @noRd
seriate_within_groups_supervised <- function(mat,
                                             node_ids,
                                             data_type,
                                             linkage_method = c("CT", "SG", "CP"),
                                             seriate_method = "TSP",
                                             w = 5,
                                             sort_by_data_type = FALSE) {
  linkage_method <- match.arg(linkage_method)

  # coerce & validate
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

  X_input <- as.matrix(mat)
  mat_names <- rownames(X_input)
  if (!is.vector(node_ids) || is.null(names(node_ids))) {
    stop("'node_ids' must be a NAMED vector.")
  }
  if (!all(mat_names %in% names(node_ids))) {
    missing <- mat_names[!mat_names %in% names(node_ids)]
    stop("Missing names in node_ids: ", paste(missing, collapse = ", "))
  }

  # If sorting by data_type
  if (sort_by_data_type) {
    if (is.null(data_type) ||
        !is.atomic(data_type) || is.null(names(data_type))) {
      stop(
        "To sort by data_type, please provide a NAMED vector `data_type` parallel to `node_ids`."
      )
    }
    if (!all(mat_names %in% names(data_type))) {
      miss2 <- mat_names[!mat_names %in% names(data_type)]
      stop("Missing names in data_type: ", paste(miss2, collapse = ", "))
    }
  }


  # Compute supervised distance matrix
  Y <- unname(node_ids[mat_names])
  D <- calculate_supervised_distance_matrix(X = X_input, Y = Y, linkage = linkage_method)
  rownames(D) <- mat_names
  colnames(D) <- mat_names


  groups <- unique(node_ids)

  # For each group, possibly sub-split by data_type, then seriate within each piece
  ordered_names <- unlist(lapply(groups, function(g) {
    mem <- names(node_ids)[node_ids == g]
    if (length(mem) < 2) {
      return(mem)
    }

    if (sort_by_data_type) {
      dts <- unique(data_type[mem])

      pieces <- lapply(dts, function(dt) {
        mem_dt <- mem[data_type[mem] == dt]
        if (length(mem_dt) < 2) {
          return(mem_dt)
        }

        if (tolower(seriate_method) == "HC") {
          d_sub <- as.dist(D[mem_dt, mem_dt])
          hc <- HC(d_sub)
          mem_dt[order.dendrogram(as.dendrogram(hc))]
        } else {
          s <- seriate(as.dist(D[mem_dt, mem_dt]), seriate_method)
          mem_dt[get_order(s)]
        }
      })
      return(unlist(pieces))
    }

    tryCatch({
      if (tolower(seriate_method) == "HC") {
        d_sub <- as.dist(D[mem, mem])
        hc <- HC(d_sub)
        mem[order.dendrogram(as.dendrogram(hc))]
      } else {
        s <- seriate(as.dist(D[mem, mem]), seriate_method)
        mem[get_order(s)]
      }
    }, error = function(e) {
      warning("Seriation failed for group ", g, ": ", e$message)
      mem
    })
  }))


  mat_ordered <- D[ordered_names, ordered_names]

  cRGAR_score <- evaluate_seriation_cRGAR(reordered_D = mat_ordered,
                                          node_ids = node_ids,
                                          w = w)

  # Return ordered distance matrix and RGAR scores
  list(
    ordered_names = ordered_names,
    mat_ordered = mat_ordered,
    cRGAR_score = cRGAR_score
  )
}

#' Calculate RGAR Measure for Submatrix
#' @noRd

calculate_RGAR <- function(D_sub, w) {
  n <- nrow(D_sub)
  if (n < 3 || w < 2) {
    return(0.0)
  }
  if (!is.matrix(D_sub) || nrow(D_sub) != ncol(D_sub)) {
    stop("D_sub must be a square matrix.")
  }
  w <- min(w, n - 1)
  anti_robinson_count <- 0
  total_comparisons <- 0
  for (i in 1:(n - 2)) {
    for (j in (i + 2):min(n, i + w)) {
      for (k in (i + 1):(j - 1)) {
        total_comparisons <- total_comparisons + 1
        if (!is.na(D_sub[i, j]) &&
            !is.na(D_sub[i, k]) && !is.na(D_sub[k, j]) &&
            (D_sub[i, j] < D_sub[i, k] ||
             D_sub[i, j] < D_sub[k, j])) {
          anti_robinson_count <- anti_robinson_count + 1
        }
      }
    }
  }
  if (total_comparisons == 0) {
    return(0.0)
  } else {
    return(anti_robinson_count / total_comparisons)
  }
}

#' Evaluate Combined RGAR for Entire Ordered Matrix
#' @noRd

evaluate_seriation_cRGAR <- function(reordered_D, node_ids, w) {
  stopifnot(is.matrix(reordered_D),
            nrow(reordered_D) == ncol(reordered_D))
  ordered_names <- rownames(reordered_D)
  # stopifnot(is.null(ordered_names) || is.null(colnames(reordered_D)) || !all(ordered_names == colnames(reordered_D)))
  n_total <- length(ordered_names)

  node_ids_filtered <- node_ids[names(node_ids) %in% ordered_names]
  missing_names <- setdiff(ordered_names, names(node_ids_filtered))
  if (length(missing_names) > 0) {
    stop(
      sprintf(
        "After filtering, these 'reordered_D' names are not found in 'node_ids': %s",
        paste(missing_names, collapse = ", ")
      )
    )
  }
  if (length(node_ids_filtered) != n_total) {
    warning(
      "Length of filtered 'node_ids' (",
      length(node_ids_filtered),
      ") does not match expected total (",
      n_total,
      "). There may be duplicates or missing entries."
    )
  }

  if (!(is.numeric(w) &&
        length(w) == 1 && w >= 2 && w == as.integer(w))) {
    stop("'w' must be a single integer >= 2.")
  }

  ordered_node_ids_values <- node_ids_filtered[ordered_names]
  unique_groups <- unique(ordered_node_ids_values)

  weighted_rgars <- sapply(
    unique_groups,
    FUN = function(group_id) {
      group_indices <- which(ordered_node_ids_values == group_id)
      group_size <- length(group_indices)

      if (group_size < 3) {
        return(0.0)
      }

      q_t <- group_size / n_total

      D_sub <- reordered_D[group_indices, group_indices, drop = FALSE]

      rgar_t <- calculate_RGAR(D_sub = D_sub, w = w)

      return(q_t * rgar_t)
    }
  )

  combined_rgar <- sum(weighted_rgars, na.rm = TRUE)

  return(combined_rgar)
}
