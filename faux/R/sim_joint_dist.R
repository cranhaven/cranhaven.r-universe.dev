#' Simulate category joint distribution
#' 
#' This function is mainly used internally, such as for simulating  missing data patterns, but is available in case anyone finds it useful.
#'
#' @param data the existing tbl
#' @param ... columns to calculate the joint distribution from, if none are chosen, all columns with 10 or fewer unique values will be chosen
#' @param n the number of total observations to return
#' @param empirical Should the returned data have the exact same distribution of conditions? (versus be sampled from a population with this distribution)
#'
#' @return data table
#' @export
#'
#' @examples
#' sim_joint_dist(ggplot2::diamonds, cut, color, n = 10)
sim_joint_dist <- function(data, ..., n = 100, empirical = FALSE) {
  cols <- getcols(data, ...)
  # if not specified, choose all columns with < 10 values
  if (length(cols) == 0) {
    col_unique <- lapply(data, unique) %>%
      sapply(length)

    cols <- names(data[, col_unique<=10, drop = FALSE])
  }

  # count instances of each unique combo
  grps <- data[cols]
  combos <- by(data, grps, function(x) {
    y <- x[1, cols, drop = FALSE]
    y$.n. <- nrow(x)
    y
  }) %>% do.call(rbind, .)
  
  # sample n combos with probs from above
  prob <- combos$.n.
  if (empirical == TRUE) {
    n_per_grp <- round(n*prob/sum(prob))
    # because of rounding, total is sometimes not n
    diff <- n - sum(n_per_grp) 
    grp_n <- length(n_per_grp)
    ndiff <- abs(sum(diff))
    if (ndiff > grp_n) {
      # more diffs than groups
      n_per_grp <- n_per_grp + (floor(ndiff/grp_n) * diff/abs(diff))
      # change ndiff to its remainder
      ndiff <- ndiff %% grp_n
    }
    # add or sub 1 to randomly sampled ndiff items from n_per_grp
    to_adjust <- sample(1:grp_n, ndiff)
    n_per_grp[to_adjust] <- n_per_grp[to_adjust] + diff/abs(diff)
    # select each combo n_per_grp times
    samp <- rep(1:nrow(combos), n_per_grp)
  } else {
    samp <- sample(1:nrow(combos), n, TRUE, prob)
  }
  
  # make a new data from with sampled combo columns
  combos$.n. <- NULL
  new_data <- lapply(samp, function(x) {
    combos[x, , drop = FALSE]
  }) %>% do.call(rbind, .)
  
  rownames(new_data) <- c()
  
  new_data
}

