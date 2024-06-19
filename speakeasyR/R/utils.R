se2_is_spmatrix_i <- function(obj) {
  class(obj)[[1]] %in% c("dgCMatrix", "ngCMatrix")
}

se2_is_matrix_i <- function(obj) {
  class(obj)[[1]] %in% c("dgCMatrix", "ngCMatrix", "matrix")
}

se2_as_matrix_i <- function(adj_like) {
  if (!se2_is_matrix_i(adj_like)) {
    adj_like <- as.matrix(adj_like)
  }

  if (!se2_is_matrix_i(adj_like)) {
    stop(paste0(
      "Could not convert adj_like to an appropriate type. ",
      "'as.matrix' converted adj_like to \"", class(adj_like)[[0]],
      "\". Currently implemented classes are c(\"matrix\", \"dgCMatrix\"). ",
      "Please open an issue on github to add support for new types."
    ))
  }

  if (nrow(adj_like) != ncol(adj_like)) {
    stop("Graph adjacency matrix must be square.")
  }

  new_adj <- list()
  if ((se2_is_spmatrix_i(adj_like)) &&
    ("x" %in% methods::slotNames(adj_like))) {
    new_adj$values <- adj_like@x
    new_adj$se2_i <- adj_like@i
    new_adj$se2_p <- adj_like@p
  } else if (se2_is_spmatrix_i(adj_like)) {
    new_adj$values <- -1
    new_adj$se2_i <- adj_like@i
    new_adj$se2_p <- adj_like@p
  } else {
    new_adj$values <- adj_like
    new_adj$se2_i <- -1
    new_adj$se2_p <- -1
  }

  new_adj$is_directed <- !Matrix::isSymmetric(adj_like)
  new_adj$n_nodes <- ncol(adj_like)

  new_adj
}
