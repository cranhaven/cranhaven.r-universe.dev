#' Randomly select a column prototype to fill an empty column prototype with
#'
#' @param data The data being biclustered. Must to be a data matrix with only numbers and missing values in the data set. It should have row names and column names.
#' @param obj A matrix for column clusters, typically named P.
#' @param col_min_num Minimum column prototype size in order to be eligible to be chosen when filling an empty column prototype. Default is 10.
#' @param col_num_to_move Number of columns to remove from the sampled prototype to put in the empty column prototype. Default is 5.
#'
#' @importFrom utils head
#' @return A matrix for column clusters, i.e., a P matrix.

fill_empties_P <- function(data, obj, col_min_num = 10,
                                                    col_num_to_move = 5) {
  data <- as.matrix(data)

  empty_protos <- which(colSums(obj) == 0)

  unassigned <- which(rowSums(obj) == 0)

  if(length(unassigned) == 0 & length(empty_protos) == 0) {

    return(obj)

  } else if(length(unassigned) == 0 & length(empty_protos) > 0) {

    num_to_fill <- length(empty_protos)

    if(all(colSums(obj, na.rm = TRUE) < col_min_num)) {
      stop(
        paste0(
          "No column groups with at least col_min_num = ", col_min_num,
          " columns. Specify a smaller col_min_num value."
        )
      )
    }

    for(j in 1:num_to_fill) {
      protos_to_choose_from <- which(colSums(obj, na.rm = TRUE) >= col_min_num)

      num_in_each <- colSums(matrix(obj[, protos_to_choose_from]))

      sampling_frame <- rep(protos_to_choose_from, num_in_each)

      proto_to_use <- sample(sampling_frame, 1)

      dummy_var <- rep(0, ncol(obj))
      dummy_var[empty_protos[j]] <- 1

      chosen_proto_members <- which(obj[, proto_to_use] == 1)

      mean_col <- mean(colMeans(matrix(data[, chosen_proto_members]), na.rm = TRUE), na.rm = TRUE)
      column_means <- colMeans(matrix(data[, chosen_proto_members]), na.rm = TRUE)

      similarity <- (column_means - mean_col) ^ 2
      to_move <- which(similarity %in% head(sort(similarity, decreasing = TRUE),
                                            n = col_num_to_move))

      for(i in 1:col_num_to_move) {
        if(is.na(chosen_proto_members[to_move[i]])) {
          stop(
            "No column clusters with at least col_num_to_move - 1 columns. Specify a smaller col_num_to_move value."
          )
        }
        obj[chosen_proto_members[to_move[i]],] <- dummy_var
      }

    }

    return(obj)
  } else {
    num_to_assign <- length(unassigned)

    for(i in 1:num_to_assign) {
      empty_protos <- colSums(obj)

      obj[unassigned[i], which.min(empty_protos)] <- 1
    }

    return(obj)
  }

}
