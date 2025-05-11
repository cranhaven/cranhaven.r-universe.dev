calc_epsp <-
  function(transformed_residuals, res_order, aggregator_matrix){
    stopifnot(is.numeric(transformed_residuals),
              is.null(aggregator_matrix) || is.matrix(aggregator_matrix))
    if(is.null(aggregator_matrix)){
      output <- cumsum(transformed_residuals[res_order])/sqrt(length(transformed_residuals))
    } else {
      output <- cumsum(as.vector(transformed_residuals[res_order] %*% aggregator_matrix))/sqrt(ncol(aggregator_matrix))
    }
    return(output)
  }
