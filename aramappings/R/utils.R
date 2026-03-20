#' @noRd
compute_orthogonal_projection_matrix <- function(
    V,
    rank_V) {
  m <- ncol(V)

  # Compute matrix U whose columns form a basis for the subspace spanned by the
  # rows of V (of dimension rank_V)
  U <- pracma::zeros(m, rank_V)

  k <- 1
  while (pracma::nnz(V[k, ]) == 0) {
    k <- k + 1
  }

  U[, 1] <- t(V[k, ])

  if (rank_V > 1) {
    j <- k + 1
    U[, 2] <- t(V[j, ])

    while (Matrix::rankMatrix(U)[1] < rank_V) {
      j <- j + 1
      U[, 2] <- t(V[j, ])
    }
  }

  # Orthogonal projection matrix
  U %*% pracma::pinv(U)
}


#' @noRd
ara_l1_norm_coo_lists <- function(
    n_points,
    V,
    row_offset) {
  n <- nrow(V)
  m <- ncol(V)

  aux_vec <- pracma::repmat(
    pracma::Reshape(
      (1 + row_offset):(2 * n * n_points + row_offset),
      2 * n,
      n_points
    ),
    m,
    1
  )
  rows <- c(
    ((1 + row_offset):(2 * n * n_points + row_offset)),
    unlist(as.list(aux_vec))
  )

  aux_vec <- pracma::repmat(
    pracma::Reshape(1:(n * n_points), n, n_points),
    2,
    1
  )
  cols <- c(
    unlist(as.list(aux_vec)),
    rep((n_points * n + 1):(n_points * (m + n)), each = 2 * n)
  )

  vals <- c(
    rep(-1, 2 * n * n_points),
    pracma::repmat(unlist(as.list(rbind(-V, V))), 1, n_points)
  )

  list(rows = rows, cols = cols, vals = vals)
}


#' @noRd
ara_linf_norm_coo_lists <- function(
    n_points,
    V,
    row_offset) {
  n <- nrow(V)
  m <- ncol(V)

  aux_vec <- pracma::repmat(
    pracma::Reshape(
      (1 + row_offset):(2 * n * n_points + row_offset),
      2 * n,
      n_points
    ),
    m,
    1
  )
  rows <- c(
    (1 + row_offset):(2 * n * n_points + row_offset),
    unlist(as.list(aux_vec))
  )

  cols <- rep(1:(n_points * (m + 1)), each = 2 * n)

  vals <- c(
    rep(-1, 2 * n * n_points),
    pracma::repmat(unlist(as.list(rbind(-V, V))), 1, n_points)
  )

  list(rows = rows, cols = cols, vals = vals)
}



#' @noRd
ara_ordered_inequality_coo_lists <- function(
    axis_vector,
    sort_indices,
    row_offset,
    col_offset) {
  N <- length(sort_indices)
  m <- length(axis_vector)

  # Data ranks for selected variable (in increasing order)
  ranks <- order(sort_indices)

  rows <- rep(NA, 2 * m * (N - 1))
  cols <- rep(NA, 2 * m * (N - 1))
  vals <- rep(NA, 2 * m * (N - 1))

  v_block <- unlist(as.list(t(cbind(-axis_vector, axis_vector))))

  idx <- 1
  col <- col_offset + 1
  for (i in 1:N) {
    if (ranks[i] == 1) {
      rows[idx:(idx + m - 1)] <- rep(ranks[i], m) + row_offset
      cols[idx:(idx + m - 1)] <- col:(col + m - 1)
      vals[idx:(idx + m - 1)] <- axis_vector
      idx <- idx + m
      col <- col + m
    } else if (ranks[i] == N) {
      rows[idx:(idx + m - 1)] <- rep(N - 1, m) + row_offset
      cols[idx:(idx + m - 1)] <- col:(col + m - 1)
      vals[idx:(idx + m - 1)] <- -axis_vector
      idx <- idx + m
      col <- col + m
    } else {
      rows[idx:(idx + 2 * m - 1)] <-
        rep((ranks[i] - 1 + row_offset):(ranks[i] + row_offset), m)
      cols[idx:(idx + 2 * m - 1)] <- rep(col:(col + m - 1), each = 2)
      vals[idx:(idx + 2 * m - 1)] <- v_block
      idx <- idx + 2 * m
      col <- col + m
    }
  }

  list(
    rows = rows,
    cols = cols,
    vals = vals
  )
}


#' @noRd
solve_glpkAPI_wrapper <- function(
    nrows,
    ncols,
    kind,
    clower,
    cupper,
    obj,
    type_cols,
    rlower,
    rupper,
    type_rows,
    ne,
    rows,
    cols,
    vals,
    use_glpkAPI_simplex,
    init_index,
    n_points,
    m) {
  lp <- glpkAPI::initProbGLPK()

  glpkAPI::setObjDirGLPK(lp, glpkAPI::GLP_MIN)

  glpkAPI::addRowsGLPK(lp, nrows)
  glpkAPI::addColsGLPK(lp, ncols)

  glpkAPI::setColsKindGLPK(lp, j = c(1:ncols), kind)

  glpkAPI::setColsBndsObjCoefsGLPK(
    lp = lp,
    j = c(1:ncols),
    lb = clower,
    ub = cupper,
    obj_coef = obj,
    type = type_cols
  )

  glpkAPI::setRowsBndsGLPK(
    lp,
    i = c(1:nrows),
    lb = rlower,
    ub = rupper,
    type = type_rows
  )


  # load constraint matrix
  glpkAPI::loadMatrixGLPK(
    lp = lp,
    ne = ne,
    ia = rows,
    ja = cols,
    ra = vals
  )


  # solve linear problem
  if (use_glpkAPI_simplex) {
    glpkAPI::setSimplexParmGLPK(glpkAPI::MSG_LEV, glpkAPI::GLP_MSG_OFF)
    glpkAPI::solveSimplexGLPK(lp)
    solution_status <- glpkAPI::getSolStatGLPK(lp)

    if (solution_status == 5) {
      x <- glpkAPI::getColsPrimGLPK(lp)

      P <- t(pracma::Reshape(
        x[init_index:(init_index + n_points * m - 1)],
        m,
        n_points
      ))
      objval <- glpkAPI::getObjValGLPK(lp)
    } else {
      message("Error: glpkAPI failed to compute an optimal solution")

      P <- matrix(NA, n_points, m)
      objval <- NA
    }
  } else {
    glpkAPI::setInteriorParmGLPK(glpkAPI::MSG_LEV, glpkAPI::GLP_MSG_OFF)
    glpkAPI::solveInteriorGLPK(lp)
    solution_status <- glpkAPI::getSolStatIptGLPK(lp)

    if ((solution_status == 3) || (solution_status == 5)) {
      x <- glpkAPI::getColsPrimIptGLPK(lp)

      P <- t(pracma::Reshape(
        x[init_index:(init_index + n_points * m - 1)],
        m,
        n_points
      ))
      objval <- glpkAPI::getObjValIptGLPK(lp)
    } else {
      message("Error: glpkAPI failed to compute an optimal solution")

      P <- matrix(NA, n_points, m)
      objval <- NA
    }
  }

  status <- rep(solution_status, n_points)

  # remove problem object
  glpkAPI::delProbGLPK(lp)

  list(
    P = P,
    status = status,
    objval = objval
  )
}


#' @noRd
solve_clarabel_wrapper <- function(
    A,
    b,
    obj,
    cones,
    P = NULL,
    init_index,
    n_points,
    m) {
  clarabel_output <- clarabel::clarabel(
    A = A,
    b = b,
    q = obj,
    P = P,
    cones = cones,
    control = list(verbose = FALSE),
    strict_cone_order = TRUE
  )

  if (clarabel_output$status == 2) {
    x <- clarabel_output$x

    P <- t(pracma::Reshape(
      x[init_index:(init_index + n_points * m - 1)],
      m,
      n_points
    ))
    objval <- clarabel_output$obj_val
  } else {
    message("Error: clarabel failed to compute an optimal solution")

    P <- matrix(NA, n_points, m)
    objval <- NA
  }

  status <- rep(clarabel_output$status, n_points)

  list(
    P = P,
    status = status,
    objval = objval
  )
}


#' @noRd
solve_Rglpk_wrapper <- function(
    A,
    b,
    obj,
    bounds,
    dirs,
    init_index,
    n_points,
    m) {
  rglpk_output <- Rglpk::Rglpk_solve_LP(
    obj = obj,
    mat = A,
    dir = dirs,
    rhs = b,
    bounds = bounds,
    types = NULL,
    max = FALSE,
    canonicalize_status = FALSE
  )

  if (rglpk_output$status == 5) {
    x <- rglpk_output$solution

    P <- t(pracma::Reshape(
      x[init_index:(init_index + n_points * m - 1)],
      m,
      n_points
    ))
    objval <- rglpk_output$optimum
  } else {
    message("Error: Rglpk failed to compute an optimal solution")

    P <- matrix(NA, n_points, m)
    objval <- NA
  }

  status <- rep(rglpk_output$status, n_points)

  list(
    P = P,
    status = status,
    objval = objval
  )
}




#' @noRd
min_unconstrained_glpkAPI <- function(
    x,
    nrows,
    ncols,
    kind,
    clower,
    cupper,
    obj,
    type_cols,
    rlower,
    type_rows,
    ne,
    rows,
    cols,
    vals,
    use_glpkAPI_simplex,
    m) {
  rupper <- c(cbind(-x, x))

  solve_glpkAPI_wrapper(
    nrows,
    ncols,
    kind,
    clower,
    cupper,
    obj,
    type_cols,
    rlower,
    rupper,
    type_rows,
    ne,
    rows,
    cols,
    vals,
    use_glpkAPI_simplex,
    length(obj) - m + 1,
    1,
    m
  )
}


#' @noRd
min_unconstrained_clarabel <- function(
    x,
    A,
    obj,
    cones,
    m) {
  b <- c(cbind(-x, x))

  solve_clarabel_wrapper(
    A,
    b,
    obj,
    cones,
    NULL,
    length(obj) - m + 1,
    1,
    m
  )
}


#' @noRd
min_unconstrained_Rglpk <- function(
    x,
    A,
    obj,
    bounds,
    dirs,
    m) {
  b <- c(cbind(-x, x))

  solve_Rglpk_wrapper(
    A,
    b,
    obj,
    bounds,
    dirs,
    length(obj) - m + 1,
    1,
    m
  )
}





#' @noRd
min_exact_glpkAPI <- function(
    x,
    nrows,
    ncols,
    kind,
    clower,
    cupper,
    obj,
    type_cols,
    type_rows,
    ne,
    rows,
    cols,
    vals,
    variable,
    use_glpkAPI_simplex,
    m) {
  n <- length(x)

  # Equality constraint for x[variable]
  rlower <- c(x[variable], rep(-Inf, 2 * n))
  rupper <- c(x[variable], cbind(-x, x))

  solve_glpkAPI_wrapper(
    nrows,
    ncols,
    kind,
    clower,
    cupper,
    obj,
    type_cols,
    rlower,
    rupper,
    type_rows,
    ne,
    rows,
    cols,
    vals,
    use_glpkAPI_simplex,
    length(obj) - m + 1,
    1,
    m
  )
}


#' @noRd
min_exact_clarabel <- function(
    x,
    A,
    obj,
    cones,
    variable,
    m) {
  b <- c(x[variable], cbind(-x, x))

  solve_clarabel_wrapper(
    A,
    b,
    obj,
    cones,
    NULL,
    length(obj) - m + 1,
    1,
    m
  )
}


#' @noRd
min_exact_Rglpk <- function(
    x,
    A,
    obj,
    bounds,
    dirs,
    variable,
    m) {
  b <- c(x[variable], cbind(-x, x))

  solve_Rglpk_wrapper(
    A,
    b,
    obj,
    bounds,
    dirs,
    length(obj) - m + 1,
    1,
    m
  )
}



#' @noRd
extract_CVXR_points_status_objval <- function(
    solution,
    Pvar,
    V,
    N,
    m) {
  if (pracma::strcmpi(solution$status, "optimal") ||
    pracma::strcmpi(solution$status, "optimal_inaccurate")) {
    P <- solution$getValue(Pvar)
    objval <- solution$value
  } else {
    P <- matrix(NA, N, m)
    objval <- NA
    message("Error: CVXR failed to compute an optimal solution")
  }

  status <- rep(solution$status, N)

  list(
    P = P,
    status = status,
    objval = objval
  )
}
