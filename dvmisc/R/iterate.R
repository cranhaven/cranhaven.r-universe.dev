#' Iterate Function Over All Combinations of User-Specified Inputs, Potentially 
#' Multiple Times
#' 
#' Same idea as \strong{purrr}::\emph{pmap}, but with some different 
#' functionality. It can runs all combinations of vector-valued arguments in 
#' \code{...} or the 1st set, 2nd set, and so forth, and multiple trials can be 
#' run for each scenario, which can be useful for simulations.
#' 
#' @param f A function.
#' @param ... Arguments to \code{f}, any of which can be vector-valued.
#' @param all_combinations Logical value for whether to iterate over all 
#' combinations of arguments in \code{...}, or just use the first set of 
#' elements, then the second, and so on.
#' @param fix List of arguments to \code{f} to hold fixed rather than loop over.
#' @param trials Numeric value.
#' @param varnames Character vector of names for values that \code{f} returns, 
#' to avoid generic labels (V1, V2, ...).
#' 
#' @return Data frame.
#' 
#' @examples
#' # Define function to generate data from N(mu, sigsq) and perform t-test.
#' f <- function(n = 100, mu = 0, sigsq = 1, alpha = 0.05) {
#'   x <- rnorm(n = n, mean = mu, sd = sqrt(sigsq))
#'   fit <- t.test(x = x, alpha = alpha)
#'   return(list(t = fit$statistic, p = fit$p.value))
#' }
#' 
#' # Call f once for various sample sizes and means
#' f %>% iterate(n = c(100, 500), mu = c(0.1, 0.25))
#' 
#' # Run 100 trials for each scenario and calculate empirical power
#' f %>% iterate(n = c(100, 500), mu = c(0.1, 0.25), trials = 100) %>%
#'   group_by(n, mu) %>%
#'   summarise(mean(p < 0.05))
#'
#' @export
iterate <- function(
  f, ..., all_combinations = TRUE, fix = NULL, trials = 1, varnames = NULL) {
  
  # Construct data frame where each row is 1 set of inputs
  if (all_combinations) {
    arg.sets <- expand_grid(...)
  } else {
    arg.sets <- as.data.frame(list(...), stringsAsFactors = FALSE)
  }
  
  # Loop through combinations and run however many trials of each set
  growing.list <- vector(mode = "list", length = nrow(arg.sets) * trials)
  index <- 0
  for (ii in 1: nrow(arg.sets)) {
    for (jj in 1: trials) {
      index <- index + 1
      growing.list[[index]] <- do.call(f, c(arg.sets[ii, , drop = FALSE], fix))
    }
  }
  
  # Prep for merge depending on what f returns
  gl1 <- growing.list[[1]]
  if (is.list(gl1)) {
    if (is.null(names(gl1))) {
      if (is.null(varnames)) {
        n.each <- length(gl1)
        labels <- paste("V", 1: n.each, sep = "")
        growing.list <- lapply(growing.list, function(x) {
          y <- x
          names(y) <- labels
          return(y)
        })
      }
    }
    premerge <- dplyr::bind_rows(growing.list)
  } else {
    premerge <- do.call(rbind, growing.list)
  }
  
  # Add variable names if specified
  if (! is.null(varnames)) {
    colnames(premerge)[(ncol(premerge) - length(varnames) + 1): 
                         ncol(premerge)] <- varnames
  }
  
  # Return data table with results
  ret <- cbind(as.data.table(arg.sets[rep(1: nrow(arg.sets), each = trials), , drop = FALSE]), 
               premerge)
  return(ret)
  
}
