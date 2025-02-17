prep_data <- function(x) {
  if (!is_qts_sample(x))
    cli::cli_abort("The input argument {.arg x} should be of class {.cls qts_sample}.")

  q_list <- log(x)
  q_list <- purrr::map(q_list, \(.x) rbind(.x$x, .x$y, .x$z))
  t_list <- purrr::map(x, "time")

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
