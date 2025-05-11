calc_mcse_binom <- function(p, B){
  stopifnot(is.numeric(unlist(p)), all(unlist(p) >= 0), all(unlist(p) <= 1),
            is.integer(B))
  lapply(p, function(x) if(any(x == 0:1)) NA_real_ else sqrt(x*(1-x)/B))
}
