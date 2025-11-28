weighted.sd <- function(x, w){

  delta_sq <- (x - mean(x))^2
  nzero_w <- (length(w[w > 0]) - 1) / length(w[w > 0])
  result <- sqrt(sum(w * (delta_sq)) / (nzero_w * sum(w)))

  return(result)
}

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

create_calibmatrix <- function(x){


  unique_obs <- unique(x)

  result <-
    lapply(unique_obs,
           function(y) {

             z <- as.integer(y == x)

             return(z)

           })

  result <- do.call(cbind, result)

  colnames(result) <- unique_obs

  return(result)

}
