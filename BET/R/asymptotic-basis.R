# Private helpers extracted from the validated BET0.5.4 paper runtime.
# Function bodies unchanged apart from private-name substitutions.

.beast_asym_soft_threshold <- function(x, threshold) {
  sign(x) * pmax(abs(x) - threshold, 0)
}

.beast_asym_index_matrix <- function(depth) {
  matrix(
    as.logical(vapply(
      0:(2^depth - 1),
      function(a) rev(as.integer(intToBits(a))[seq_len(depth)]),
      integer(depth)
    )),
    ncol = depth,
    byrow = TRUE
  )
}

.beast_asym_cross_interaction_basis <- function(p, D, index = as.list(seq_len(p))) {
  basis <- .beast_asym_index_matrix(p * D)
  keep <- apply(basis, 1L, function(interaction) {
    all(vapply(index, function(group) {
      positions <- unlist(lapply(group, function(j) {
        (j - 1L) * D + seq_len(D)
      }), use.names = FALSE)
      any(interaction[positions])
    }, logical(1L)))
  })
  basis[keep, , drop = FALSE]
}

.beast_asym_empirical_ranks <- function(X) {
  X <- as.matrix(X)
  apply(X, 2L, rank, ties.method = "average")
}

.beast_asym_interaction_means_from_ranks <- function(ranks, D,
                                               index = as.list(seq_len(ncol(ranks))),
                                               basis = NULL) {
  ranks <- as.matrix(ranks)
  n <- nrow(ranks)
  p <- ncol(ranks)
  if (is.null(basis)) {
    basis <- .beast_asym_cross_interaction_basis(p, D, index)
  }

  u <- (ranks - 0.5) / n
  binary_bits <- do.call(cbind, lapply(seq_len(p), function(j) {
    vapply(seq_len(D), function(d) {
      2L * as.integer(floor(u[, j] * 2^d) %% 2L) - 1L
    }, integer(n))
  }))

  # The theory and simulations split the variables into exactly two blocks.
  # Build the nonconstant Walsh columns within each block, then obtain every
  # cross interaction with one cross-product. This is algebraically identical
  # to the generic coordinate loop below but is much faster inside the Gamma
  # estimator, where this function is called many thousands of times.
  if (length(index) == 2L) {
    group_features <- lapply(index, function(group) {
      positions <- unlist(lapply(group, function(j) {
        (j - 1L) * D + seq_len(D)
      }), use.names = FALSE)
      masks <- .beast_asym_index_matrix(length(positions))[-1L, , drop = FALSE]
      vapply(seq_len(nrow(masks)), function(k) {
        selected <- positions[which(masks[k, ])]
        if (length(selected) == 1L) {
          binary_bits[, selected]
        } else {
          apply(binary_bits[, selected, drop = FALSE], 1L, prod)
        }
      }, numeric(n))
    })
    means <- as.vector(t(crossprod(group_features[[1L]], group_features[[2L]]) / n))
    if (is.null(basis)) basis <- .beast_asym_cross_interaction_basis(p, D, index)
    names(means) <- apply(basis + 0L, 1L, paste0, collapse = "")
    return(means)
  }

  means <- vapply(seq_len(nrow(basis)), function(j) {
    selected <- which(basis[j, ])
    if (length(selected) == 1L) {
      mean(binary_bits[, selected])
    } else {
      mean(apply(binary_bits[, selected, drop = FALSE], 1L, prod))
    }
  }, numeric(1L))
  names(means) <- apply(basis + 0L, 1L, paste0, collapse = "")
  means
}

.beast_asym_interaction_means <- function(X, D,
                                    index = as.list(seq_len(ncol(as.matrix(X)))),
                                    basis = NULL) {
  .beast_asym_interaction_means_from_ranks(
    .beast_asym_empirical_ranks(X), D = D, index = index, basis = basis
  )
}
