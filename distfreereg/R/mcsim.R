mcsim <-
  function(B, r, aggregator_matrix, stat, verbose){
    stopifnot(isTRUE(as.integer(B) == B), identical(length(B), 1L), is.matrix(r))
    n <- nrow(r)
    tr <- t(r)
    seqn <- seq_len(n)
    output <- matrix(NA, nrow = B, ncol = length(stat))
    for(i in seq_len(B)){
      if(isTRUE(verbose) && isTRUE(i %% as.integer(B/10) == 0))
        message("...", i, " of ", B)
      epsilon <- rnorm(n)
      e_proj <- epsilon - r %*% (tr %*% epsilon)
      epsp <- calc_epsp(transformed_residuals = e_proj, res_order = seqn,
                        aggregator_matrix = aggregator_matrix)
      output[i,] <- calc_stats(epsp = epsp, stat = stat)
    }
    output <- as.list(as.data.frame(output))
    names(output) <- stat
    return(output)
  }
