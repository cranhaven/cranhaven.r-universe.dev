determine_order <-
  function(X, ordering, n, verbose){
    if(is.null(X)){
      if(isPermutation(ordering)){
        output <- ordering
      } else{
        output <- seq_len(n)
      }
    } else{
      if(identical(ordering, "asis")){
        if(isTRUE(verbose)) message("Leaving observation order as is...")
        output <- 1:nrow(X)
      } else {
        if(identical(ordering, "optimal")){
          stopifnot(is.matrix(X))
          if(isTRUE(verbose)) message("Ordering observations using optimal transport...")
          m <- nrow(X)
          mbyn <- length(X)
          stopifnot(mbyn > 0)
          reference <- matrix((1:mbyn)/mbyn, nrow = m, byrow = TRUE)
          if(isTRUE(verbose)) message("- Creating distance matrix...")
          dist_mat <- matrix(NA, nrow = m, ncol = m)
          for(i in 1:m){
            for(j in 1:m){
              dist_mat[i,j] <- sqrt(sum((reference[i,] - X[j,])^2))
            }
          }
          if(isTRUE(verbose)) message("- Solving linear sum assignment problem using the Hungarian method...")
          output <- tryCatch(clue::solve_LSAP(dist_mat),
                             error = function(e) stop("Hungarian method failure: ", e))
          stopifnot(isPermutation(output))
        } else {
          if(identical(ordering, "simplex")){
            stopifnot(is.matrix(X))
            if(isTRUE(verbose)) message("Ordering observations by simplex method...")
            X_scaled <- scale_to_01(X)
            output <- order(rowSums(X_scaled))
          } else {
            # By this point, the only option left should be the natural ordering
            # or an ordering specified by columns; both are handled by
            # natural_order().
            if(identical(ordering, "natural")){
              if(isTRUE(verbose)) message("Ordering observations by natural order...")
              ordering <- 1:ncol(X)
            } else {
              if(isTRUE(verbose)) message("Ordering observations by specified columns...")
              # Unlist, since if ordering specifies columns, it must be a list.
              ordering <- unlist(ordering)
            }
            output <- natural_order(data = X, cols = ordering)
          }
        }
      }
    }
    return(output)
  }