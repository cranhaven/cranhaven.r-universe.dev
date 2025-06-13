#' Created a Pooled Dataset from a Subject-Specific One
#'
#' Useful for simulation studies on biospecimen pooling designs.
#'
#'
#' @param dat Data frame with individual level data.
#' @param pool_sizes Integer vector of pool sizes ordered from largest to
#' smallest.
#' @param num_each Integer vector specifying number of pools of each size.
#' @param prop_each Numeric vector specifying proportion of pools of each size.
#'
#'
#' @return Data frame.
#'
#'
#' @export
form_pools <- function(
  dat,
  pool_sizes,
  num_each = NULL,
  prop_each = rep(1 / length(pool_sizes), length(pool_sizes))
) {

  # Check that pool_sizes are given in decreasing order
  if (! all.equal(pool_sizes, sort(pool_sizes, decreasing = TRUE))) {
    stop("The input 'pool_sizes' should give pool sizes from largest to smallest")
  }

  # Get number of observations
  n.obs <- nrow(dat)

  # Sort pool_sizes from big to small
  pool_sizes <- sort(pool_sizes, decreasing = TRUE)

  # Figure out num_each if necessary
  if (is.null(num_each)) {

    n.pool.sizes <- length(pool_sizes)
    pool_sizes_subset <- pool_sizes[-n.pool.sizes]
    A <- matrix(prop_each,
                nrow = n.pool.sizes,
                ncol = n.pool.sizes) -
      diag(n.pool.sizes)
    A <- rbind(A[-n.pool.sizes, ], pool_sizes)
    num_each <- solve(a = A, b = c(rep(0, n.pool.sizes - 1), n.obs))

    # If non-integer values, round down and add leftover observations to last
    # pool size
    if (! all(num_each == as.integer(num_each))) {
      num_each <- floor(num_each)
      num_each[n.pool.sizes] <- num_each[n.pool.sizes] +
        floor(n.obs - sum(num_each * pool_sizes))
    }

  }

  # Print message summarizing pool formation
  for (ii in 1: n.pool.sizes) {
    message("Formed ", num_each[ii], " pools of size ", pool_sizes[ii], sep = "")
  }

  # If leftover observations, print message notifying user
  n.leftover <- n.obs - sum(num_each * pool_sizes)
  if (n.leftover > 0) {
    message("Note: ", n.leftover, " observations were not formed into pools")
  }

  # Add variable for pool size and pool number
  dat$g <- unlist(mapply(
    FUN = function(x, y) rep(x, each = y * x),
    x = pool_sizes, y = num_each
  ))
  dat$poolnumber <- unlist(mapply(
    FUN = function(x, y, z) rep(1: y, each = x),
    x = pool_sizes, y = num_each
  ))

  # Create and return pooled dataset
  dat.pooled <- data.table(dat)[, lapply(.SD, sum), by = c("g", "poolnumber")]
  return(as.data.frame(dat.pooled))

}
