prep_data <- function(x) {
  if (!is_qts_sample(x))
    cli::cli_abort("The input argument {.arg x} should be of class {.cls qts_sample}.")

  q_list <- log(x)
  q_list <- lapply(q_list, \(.x) rbind(.x$x, .x$y, .x$z))
  t_list <- lapply(x, \(.x) .x$time)

  # Prep data
  N <- length(q_list)
  L <- dim(q_list[[1]])[1]
  P <- dim(q_list[[1]])[2]

  if (is.null(t_list))
    grid <- 0:(P-1)
  else
    grid <- matrix(nrow = N, ncol = P)

  values <- array(dim = c(N, L, P))
  for (n in 1:N) {
    values[n, , ] <- q_list[[n]]
    if (!is.null(t_list)) {
      grid[n, ] <- t_list[[n]]
    }
  }

  list(grid = grid, values = values, N = N, L = L, P = P)
}

transpose <- function(x) {
  n <- length(x[[1]])
  lens <- lengths(x)
  if (any(lens != n))
    cli::cli_abort("All elements of the input list should have the same length.")
  lapply(1:n, \(i) sapply(x, \(.x) .x[i]))
}

transpose_flatten <- function(x) {
  n <- length(x[[1]])
  lens <- lengths(x)
  if (any(lens != n))
    cli::cli_abort("All elements of the input list should have the same length.")
  lapply(1:n, \(i) sapply(x, \(.x) .x[[i]]))
}
